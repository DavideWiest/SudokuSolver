module SudokuSolver.Util

open SudokuSolver.Domain

let charToInt dChar = int(dChar - char('0'))
let boolToInt b = if b then 1 else 0

module Seq =
    let countOccurrences a seq =
        seq
        |> Seq.filter (fun x -> x = a)
        |> Seq.length

module Array =
    let indexElementsAsTuple projection (arr: 'a array) =
        arr
        |> Array.mapi (fun i x -> i, projection x)

    let maxIndexBy projection (arr: 'a array) =
        arr
        |> indexElementsAsTuple id
        |> Array.maxBy snd
        |> fst

    let minIndexBy projection (arr: 'a array) =
        arr
        |> indexElementsAsTuple id
        |> Array.minBy snd
        |> fst

    let filterReturnIndex filter (arr: 'a array) =
        arr
        |> indexElementsAsTuple id
        |> Array.filter (snd >> filter)
        |> Array.map fst

    let filteri f (arr: 'a array) =
        arr
        |> Array.mapi (fun i x -> i, x)
        |> Array.filter (fun (i, x) -> f i x)
        |> Array.map snd

module Array2D =
    let flattenToArray (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toArray

    let flattenToSeq (arr: 'T [,]) = arr |> Seq.cast<'T>
    let flattenToArrayOfArray (arr: 'T [,]) = Array.init (arr.GetLength(0)) (fun i -> Array.init (arr.GetLength(1)) (fun j -> arr.[i, j]))
    let flattenToList (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toList

    let transpose (arr: 'T [,]) = 
        let rows = arr.GetLength(0)
        let cols = arr.GetLength(1)
        Array2D.init cols rows (fun i j -> arr.[j, i])

    let fromArray cols rows (arr: 'T array) = 
        if arr.Length = cols * rows then
            Array2D.init rows cols (fun i j -> arr.[i * cols + j])
        else
            failwith "Invalid array dimensions"
    
    let filterReturnIndex filter (arr: 'T [,]) =
        arr
        |> Array2D.mapi (fun i j x -> i, j, x)
        |> flattenToSeq
        |> Seq.filter (fun (i, j, x) -> filter x)
        |> Seq.map (fun (i, j, x) -> i, j)
        |> Seq.toArray

module List =
    let averageByOrZero projection (list: 'a list) =
        if List.isEmpty list then 0.0 else list |> List.averageBy projection

    let averageOrZero (list: float list) =
        if List.isEmpty list then 0.0 else list |> List.average

let optionize (predicate: 'a -> bool) (value: 'a) =
    if predicate value then Some value else None

let intersectionOfLists list1 list2 = list1 |> List.filter (fun x -> List.contains x list2)

let writeToFile (filename: string) (content: string) =
    System.IO.File.WriteAllText(filename, content)

let appendToFile (filename: string) (content: string) =
    System.IO.File.AppendAllText(filename, content)

let modPosRepr i d = 
    let rem = i % d
    if rem < 0 then rem + d else rem

// ---
// SUDOKU BOARD UTILS
// ---

let getRowInner (board: SolvingSudokuBoard) row =
    board.[row, *]

let getRow (board: SolvingSudokuBoard) row column =
    getRowInner board row

let getColumnInner (board: SolvingSudokuBoard) column =
    board.[*, column]

let getColumn (board: SolvingSudokuBoard) row column =
    getColumnInner board column

let getBlock (board: SolvingSudokuBoard) row column =
    let blockRow = row / 3
    let blockColumn = column / 3
    let startRow = blockRow * 3
    let startColumn = blockColumn * 3
    board.[startRow..startRow + 2, startColumn..startColumn + 2] |> Array2D.flattenToArray

let relevantSquareGetters = [getRow; getColumn; getBlock]

let countSolvedSquares (board: SolvingSudokuBoard) =
    board
    |> Array2D.map (fun x -> 
        match x with 
        | SolvedCell s -> 1
        | _ -> 0
    )
    |> Array2D.flattenToSeq
    |> Seq.sum

let booleanMatrixVisualization (arr2D: bool array2d) =
    arr2D
    |> Array2D.map (fun value ->
        if value then "X" else " "
    )
    |> Array2D.flattenToArrayOfArray
    |> Array.map (String.concat " | ")
    |> String.concat "\n"
    |> printfn "Test Matrix:\n%s"

// ---
// CONVERSION
// ---

let cellToList row col (board: SolvingSudokuBoard) =
    match board[row, col] with
    | SolvedCell n -> [n]
    | UnsolvedCell ns -> ns

let arrayToSudokuCell (arr: int array) =
    match arr.Length with
        | 1 -> SolvedCell (arr.[0] + 1)
        | _ -> UnsolvedCell (arr |> Array.map (fun x -> x + 1) |> Array.toList)

let solvedSudokuBoardToIntArray = Array2D.flattenToArray

let convertToSolvingBoard board =
    board 
    |> Array2D.map (fun square ->
        match square with
        | Number n -> SolvedCell n
        | Empty -> UnsolvedCell [1..9]
    )

let convertToSolvedBoard solvingBoard =
    let convertToSolvedBoard' solvingBoard : SolvedSudokuBoard =
        solvingBoard
        |> Array2D.mapi (fun i j  square -> 
            match square with
            | SolvedCell n -> n
            | UnsolvedCell ns -> 
                let positionString = (i.ToString()) + "," + (j.ToString())
                let possibilitiesString = (String.concat "," (ns |> List.map (fun n -> n.ToString())))
                failwith ("Unsolved square at " + positionString + ". Possibilities: " + possibilitiesString)
        )

    try 
        Ok (convertToSolvedBoard' solvingBoard)
    with e -> Error (solvingBoard)

let cellResultToString cellValues = [1..9] |> List.map (fun n -> if List.contains n cellValues then n.ToString() else " ") |> String.concat ""

let cellToString cell =
    match cell with
    | SolvedCell n -> n.ToString() |> sprintf "   [%s]   "
    | UnsolvedCell ns -> cellResultToString ns

let boardToString (squareToStringFn: 'a -> string) (board: 'a array2d) : string = 
    board 
    |> Array2D.map squareToStringFn
    |> Array2D.flattenToArrayOfArray 
    |> Array.map (fun row -> row |> String.concat " | ")
    |> String.concat "\n"

let solvingBoardToString (board: SolvingSudokuBoard) = 
    board
    |> boardToString cellToString

let solvedBoardToString (board: SolvedSudokuBoard) = 
    board
    |> boardToString (fun n -> n.ToString())

let unsolvedBoardFromDigitArray (digitString: string) (isEmptyFieldFn: char -> bool) =
    digitString.ToCharArray()
    |> Array.map (fun dChar -> if isEmptyFieldFn dChar then Empty else Number (charToInt dChar))

let solvedBoardFromDigitArray (digitString: string) =
    digitString.ToCharArray()
    |> Array.map (fun dChar -> charToInt dChar)



let printPath (path: (int*int) seq) =
    let maxX = path |> Seq.map (fun (x, y) -> x) |> Seq.max
    let maxY = path |> Seq.map (fun (x, y) -> y) |> Seq.max

    let pathArray = Array2D.init (maxX + 1) (maxY + 1) (fun i j -> Seq.findIndex (fun (x, y) -> x = i && y = j) path)

    pathArray
    |> boardToString (fun n -> if n = -1 then " " else n.ToString())

let timeAndReturn title f a =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let result = f a
    stopwatch.Stop()
    printfn "%s took %.0fµs" title stopwatch.Elapsed.TotalMilliseconds
    result
