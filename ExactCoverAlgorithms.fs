module SudokuSolver.DLXAlgorithm

open SudokuSolver.Domain
open SudokuSolver.Util
open System

// ---
// Types
// ---

let private S = 9
let private numValues = 9
let private blockRows = 3
let private blockCols = 3
let private numConstraints = 4 // value, row, col, block
let private constraintMatrixRowCount = S * S * numValues
let private constraintMatrixColCount = S * S * numConstraints

type BoardValue = {
    row: int
    col: int
    valueIndex: int
}

type ConstraintIndex = {
    constraintNum: int
    row: int
    col: int
}

// ---
// Util
// ---

let destructureValueIndex idx = 
    {
        row = idx / S / S
        col = (idx / S) % S
        valueIndex = idx % S
    }

let destructureConstraintIndex idx =
    let remainder = idx % (S * S)
    {
        constraintNum = idx / (S * S)
        row = remainder / S
        col = remainder % S
    }

let getValueIndex row col valueIndex = row * S * S + col * S + valueIndex

let getColumnSumOfTrues (matrix: bool array2d) = 
    [| 0..matrix.GetLength 1 - 1 |]
    |> Array.map (fun col -> matrix[*, col] |> Array.sumBy boolToInt)

let getSumOfTrues = getColumnSumOfTrues >> Array.sum

// ---
// Conversion
// ---

let matrixToBoard (matrix: bool array2d) : SolvingSudokuBoard =
    let matrixValueArray = Array.init constraintMatrixRowCount (fun rowI -> matrix[rowI, *] |> Array.sumBy boolToInt)
    
    Array2D.init S S (fun i j ->
        [| 0..numValues - 1 |]
        |> Array.filter (fun valueIndex -> matrixValueArray.[getValueIndex i j valueIndex] > 0)
        |> arrayToSudokuCell
    )

let constraintMatrixToString (matrix: bool array2d) =
    let mutable lines = []
    for row in 0..matrix.GetLength 0 - 1 do
        let line = 
            matrix.[row, *]
            |> Array.map (fun x -> if x then "X" else " ")
            |> String.concat ""
        lines <- lines @ [line]

    lines 
    |> String.concat "\n"

// ---
// Constraint Matrix
// ---

let getGenericBoardConstraintMatrix =
    Array2D.init constraintMatrixRowCount constraintMatrixColCount (fun row col -> 
        let (valueRow, valueCol, valueIndex) = 
            row |> destructureValueIndex |> fun boardValue -> (boardValue.row, boardValue.col, boardValue.valueIndex)
        let (constraintNum, constraintRow, constraintCol) = 
            col |> destructureConstraintIndex |> fun constraintIndex ->  (constraintIndex.constraintNum, constraintIndex.row, constraintIndex.col)

        // value constraint
        (constraintNum = 0 && valueRow * 9 + valueCol = constraintRow * 9 + constraintCol)
        // row constraint
        || (constraintNum = 1 && valueIndex = constraintCol && valueRow = constraintRow)
        // col constraint
        || (constraintNum = 2 && valueIndex = constraintCol && valueCol = constraintRow)
        // block constraint
        || (constraintNum = 3 && valueIndex = constraintCol && (valueRow / blockRows) * 3 + (valueCol / blockCols) = constraintRow)
    )

let applyBoardToConstraintMatrix (matrix: bool array2d) (board: SolvingSudokuBoard)  =
    Array.init constraintMatrixRowCount ( fun row ->
        let (valueRow, valueCol, valueIndex) = 
            row |> destructureValueIndex |> fun boardValue -> (boardValue.row, boardValue.col, boardValue.valueIndex)

        if cellToList valueRow valueCol board |> List.contains (valueIndex + 1) then matrix[row, *] else Array.init constraintMatrixColCount (fun _ -> false)
    )
    |> array2D

// ---
// Algorithm X
// ---

let reduceProblem (problem: bool[,]) (rowChosenI: int) =
    let colsToDelete = problem[rowChosenI, *] |> Array.filterReturnIndex id
    let colsToKeep = problem[rowChosenI, *] |> Array.filterReturnIndex (id >> not)

    let reducedProblem = 
        colsToDelete
        |> Array.fold (fun (acc: bool array2d) colI -> 
            acc.[*, colI]
            |> Array.mapi (fun i x -> if x then Array.init (problem.GetLength 1) (fun _ -> false) else acc.[i, *])
            |> array2D
        ) problem

    colsToKeep
    |> Array.map (fun colI -> reducedProblem[*, colI])
    |> array2D
    |> Array2D.transpose

let applyRowToBoard (matRowI: int) (board: SolvingSudokuBoard) =
    let rowI, colI, valueIndex = 
        matRowI |> destructureValueIndex |> fun boardValue -> boardValue.row, boardValue.col, boardValue.valueIndex

    board
    |> Array2D.mapi (fun i j  x -> 
        match x with 
        | UnsolvedCell ns when i = rowI && j = colI -> SolvedCell (valueIndex + 1)
        | SolvedCell n when i = rowI && j = colI -> x
        | _ -> x
    )

let solveWithAlgorithmX (genericConstraintMatrix: bool array2d) (board: SolvingSudokuBoard) =
    let rec solve (acc: int list) (problem: bool[,]) : int list option =
        // no constraints exists to be satisfied
        if problem.GetLength 0 = 0 || problem.GetLength 1 = 0 then Some acc else

        let colWithFewestOnes = problem |> getColumnSumOfTrues |> Array.minIndexBy id
        let rowIndices = problem[*, colWithFewestOnes] |> Array.filterReturnIndex id // |> Array.sortBy (fun x -> problem.[x, *] |> Array.sumBy boolToInt)
        
        rowIndices
        |> Array.tryPick (fun rowChosenI -> 
            rowChosenI |> (reduceProblem problem >> solve acc >> Option.bind (fun newAcc -> Some (rowChosenI::newAcc)))
        )
    
    match board |> applyBoardToConstraintMatrix genericConstraintMatrix |> solve [] with
    | Some solution -> 
        List.fold (fun board matRowI -> applyRowToBoard matRowI board) board solution
    | None -> failwith "No solution"
