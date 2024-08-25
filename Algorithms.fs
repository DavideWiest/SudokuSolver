module SudokuSolver.Algorithms

open SudokuSolver.Domain
open SudokuSolver.Util

let collectRelevantSquaresByFn (solvingBoard: SolvingSudokuSquare array2d) row column fn =
    fn solvingBoard row column |> Array.toList

let collectAllRelevantSquares (solvingBoard: SolvingSudokuSquare array2d) row column =
    relevantSquareGetters
    |> List.collect (fun fn -> collectRelevantSquaresByFn solvingBoard row column fn)

let filterSquaresSolved (values: SolvingSudokuSquare seq) =
    values 
    |> Seq.choose (fun x -> 
        match x with
        | SolvedCell n -> Some n
        | UnsolvedCell _ -> None)

let filterSquaresUnsolved (values: SolvingSudokuSquare seq) =
    values 
    |> Seq.choose (fun x -> 
        match x with
        | SolvedCell n -> None
        | UnsolvedCell ns -> Some ns)

let filterPossibilities (possibilities: int seq) (values: int seq) =
    possibilities 
    |> Seq.filter (fun x -> not (Seq.contains x values))

let filterPossibilitiesOneOccurrence (possibilities: int list) (values: int seq) = 
    let occurrenceCount = Seq.countBy id values
    possibilities |> Seq.filter (fun x -> occurrenceCount |> Seq.contains (x, 1))

let filterSquaresSolvedArray (values: SolvingSudokuSquare array) = filterSquaresSolved values |> Array.ofSeq

let filterSquaresUnsolvedList (values: SolvingSudokuSquare list) = filterSquaresUnsolved values |> List.ofSeq
let filterSquaresUnsolvedArray (values: SolvingSudokuSquare array) = filterSquaresUnsolved values |> Array.ofSeq

let filterPossibilitiesArray (possibilities: int list) (values: int array) = filterPossibilities possibilities values |> Array.ofSeq

let filterPossibilitiesOneOccurrenceList (possibilities: int list) (values: int list) =  filterPossibilitiesOneOccurrence possibilities values |> List.ofSeq
let filterPossibilitiesOneOccurrenceArray (possibilities: int list) (values: int array) = filterPossibilitiesOneOccurrence possibilities values |> Array.ofSeq

let passToSquare (poss: int list) =
    match poss.Length with
    | 0 -> failwith "No possibilities left"
    | 1 -> SolvedCell poss.[0]
    | _ -> UnsolvedCell poss

let multiPossToSolvedSquare (possLists: int list list) =
    possLists |> List.collect (fun x -> x) |> List.distinct |> passToSquare


let getDirectPossibilities (solvingBoard: SolvingSudokuSquare array2d) row column (possibilities: int list) =
    [| getRow; getColumn; getBlock |]
    |> Array.collect (fun fn -> fn solvingBoard row column)
    |> Array.choose (fun x -> 
        match x with
        | SolvedCell n -> Some n
        | UnsolvedCell _ -> None)
    |> (fun y -> Array.filter (fun x -> not (Array.contains x y)) (possibilities |> Array.ofList))
    |> List.ofArray
    |> passToSquare

let getDirectPossibilitiesThroughElimination (solvingBoard: SolvingSudokuSquare array2d)  =

    let fieldFns = [|
        fun x board -> getRow board x 0
        fun x board -> getColumn board 0 x
        fun x board -> getBlock board (x / 3 * 3) (x % 3 * 3)
    |]
    
    let accessFns = [|
        fun i j -> (i,j)
        fun i j -> (j,i)
        fun i j -> (i / 3 * 3 + j / 3, i % 3 * 3 + j % 3)
    |]

    let fields =
        fieldFns
        |> Array.map (fun fn -> 
            [|0..8|] 
            |> Array.map (fun x -> fn x solvingBoard)
        )
    
    let definedFieldValues = 
        fields
        |> Array.map (fun x -> x |> Array.map filterSquaresSolvedArray)

    let undefinedFieldValues = 
        fields
        |> Array.map (fun x -> x |> Array.map filterSquaresUnsolvedArray)
        |> Array.map (fun x -> 
            x 
            |> Array.map (fun y -> 
                y 
                |> Array.collect (fun z -> z |> Array.ofList)
            )
        )

    let mutable board = solvingBoard

    definedFieldValues 
    |> Array.iteri (fun fieldI field ->
        for i in [0..8] do
            for j in [0..8] do
                let (x, y) = accessFns.[fieldI] i j
                match board.[x, y] with
                | UnsolvedCell possibilities ->
                    // may not be the correct indices
                    board.[x,y] <- filterPossibilitiesArray possibilities field.[i] |> List.ofArray |> passToSquare
                | _ -> ()
    )

    undefinedFieldValues
    |> Array.iteri (fun fieldI field ->
        for i in [0..8] do
            for j in [0..8] do
                let (x, y) = accessFns.[fieldI] i j
                match board.[x, y] with
                | UnsolvedCell possibilities ->
                    let possibilities = filterPossibilitiesOneOccurrenceArray possibilities field.[i]
                    if possibilities.Length = 1 then board.[x,y] <- possibilities |> List.ofArray |> passToSquare
                | _ -> ()
    )

    board

// it is efficient to assume that direct possibilities have been exhausted - they are still checked because they might reappear and take less time
let getDirectAndIndirectPossibilities (relevantSquareGetters: SquareArrayGetter list) (solvingBoard: SolvingSudokuSquare array2d) row column possibilities =
    let findIndirectPossibilities (squareGetterFn: SquareArrayGetter) solvingBoard row column directPossibilities = 
        squareGetterFn
        |> collectRelevantSquaresByFn solvingBoard row column
        |> filterSquaresUnsolvedList
        |> List.collect (fun x -> x)
        |> filterPossibilitiesOneOccurrenceList directPossibilities
    
    let directPossSquare = getDirectPossibilities solvingBoard row column possibilities

    match directPossSquare with
    | SolvedCell _ -> directPossSquare
    | UnsolvedCell directPossibilities ->

        let indirectPossibility =
            relevantSquareGetters 
            |> Seq.tryPick (fun squareGetterFn ->
            let result = findIndirectPossibilities squareGetterFn solvingBoard row column directPossibilities
            if result.Length = 1 then Some result else None)

        let poss: int list = 
            if indirectPossibility.IsSome
            then indirectPossibility.Value
            else directPossibilities

        poss |> passToSquare

let getSecondOrderPossibilities (relevantSquareGetters: SquareArrayGetter list) (solvingBoard: SolvingSudokuSquare array2d) row column possibilities =
    let possCell = getDirectAndIndirectPossibilities relevantSquareGetters solvingBoard row column possibilities

    match possCell with
    | SolvedCell _ -> possCell
    | UnsolvedCell remainingPoss ->

        // not likely that a square with more than 4 possibilities will be solved by this algorithm
        if remainingPoss.Length > 4 then UnsolvedCell remainingPoss else

        let findPairs (values: int list seq)  =
            let valuesOfNMinusOne = 
                values
                |> Seq.filter (fun x -> x |> Seq.length = remainingPoss.Length - 1)
                |> Seq.map Seq.ofList

            remainingPoss
            |> Seq.tryPick (fun x ->
                let search = remainingPoss |> Seq.filter (fun y -> y <> x)
                let occurrenceCount = Seq.countOccurrences search valuesOfNMinusOne
                if occurrenceCount = remainingPoss.Length - 1 then Some [x] else None
            ) 

        let solutions = 
            relevantSquareGetters
            |> List.map (fun fn -> collectRelevantSquaresByFn solvingBoard row column fn)
            |> List.map filterSquaresUnsolved
            |> List.tryPick findPairs
        
        let poss =
            if solutions.IsSome
            then solutions.Value
            else remainingPoss

        poss |> passToSquare

let backTrack (innerAlg: SolvingSudokuBoard -> SolvingSudokuBoard) (solvingBoard: SolvingSudokuSquare array2d) =
    // find first unsolved square
    let rec findFirstUnsolved (board: SolvingSudokuSquare array2d) row column =
        board 
        |> Array2D.mapi (fun i j  x -> (i, j, x))
        |> Array2D.flattenToArray
        |> Array.tryPick (fun (i,j,x) -> 
            match x with
            | UnsolvedCell _ -> Some (i, j)
            | _ -> None)
        
    let rec backTrackInner (board: SolvingSudokuSquare array2d) =
        match findFirstUnsolved board 0 0 with
        | None -> Some board
        | Some (row, column) ->
            match board.[row, column] with
            | UnsolvedCell possibilities ->
                possibilities
                |> List.map (fun x -> 
                    try 
                        let mutable newBoard = Array2D.copy board
                        newBoard.[row, column] <- SolvedCell x
                        backTrackInner (innerAlg newBoard)
                    with
                    | _ -> None
                )
                |> Seq.tryPick id
            | _ -> failwith "This should not happen"

    match backTrackInner (innerAlg solvingBoard) with
    | Some board -> board
    | None -> solvingBoard

type StepwiseSquareSolvingFn = SolvingSudokuBoard -> int -> int -> int list -> SolvingSudokuSquare

let applyToUnsolved (f: StepwiseSquareSolvingFn) (solvingBoard: SolvingSudokuSquare array2d) =
    let mutable board = solvingBoard

    //for _ in [0..81] do
    //    let (row, column) = findNextOptimalSquare board
    for row in [0..8] do
        for column in [0..8] do
            match board.[row, column] with
            | UnsolvedCell possibilities -> board.[row, column] <- f board row column possibilities
            | _ -> ()

    board

// this should work but does not
let solveSudokuStepwiseInner (boardIterationFn: SolvingSudokuBoard -> SolvingSudokuBoard) (board: SolvingSudokuBoard) =
    let rec solveUntilStable boarditerationFn board solvedLast =
        let newBoard = boarditerationFn board
        let newSolved = countSolvedSquares newBoard
        if newSolved - solvedLast = 0 then newBoard else solveUntilStable boarditerationFn newBoard newSolved

    solveUntilStable boardIterationFn board 0

let stepwiseAlgorithmChain (boardIterationFns: StepwiseSquareSolvingFn list) (board: SolvingSudokuBoard) =
    boardIterationFns
    |> List.fold (fun board fn -> solveSudokuStepwiseInner (applyToUnsolved fn) board) board

let generalAlgorithmChain (boardIterationFns: (SolvingSudokuBoard -> SolvingSudokuBoard) list) (board: SolvingSudokuBoard) =
    boardIterationFns
    |> List.fold (fun board fn -> fn board) board

// ---
// Algorithms
// ---

// fast but solves only 82% of easy puzzles, ca 40% of hard puzzles
let solveSudokuStepwiseDirectP board = solveSudokuStepwiseInner (applyToUnsolved getDirectPossibilities) board

// solves 94% of easy puzzles
let solveSudokuStepwiseIndirectP = fun (relevantSquareGetters: SquareArrayGetter list) -> solveSudokuStepwiseInner (applyToUnsolved (getDirectAndIndirectPossibilities relevantSquareGetters))

// not slow and solves 98% of easy puzzles
let solveSudokuStepwiseIndirectChained = fun (relevantSquareGetters: SquareArrayGetter list) -> stepwiseAlgorithmChain [getDirectPossibilities; (getDirectAndIndirectPossibilities relevantSquareGetters)]

// a bit slower and solves all puzzles
let chainedWithBacktrack = fun (relevantSquareGetters: SquareArrayGetter list) -> backTrack (solveSudokuStepwiseIndirectChained relevantSquareGetters)

// ???
let solveSudokuStepwiseWithSecondOrder = fun (relevantSquareGetters: SquareArrayGetter list) -> solveSudokuStepwiseInner (applyToUnsolved (getSecondOrderPossibilities relevantSquareGetters))

// a lot slower and solves all puzzles
let chainedWithBacktrackOnlyDirect =  backTrack (solveSudokuStepwiseDirectP)

// slower and uglier than the chained version
let solveSudokuStepwiseElimination = solveSudokuStepwiseInner (getDirectPossibilitiesThroughElimination)

