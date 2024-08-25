module SudokuSolver.DLXAlgorithm

open SudokuSolver.Domain
open SudokuSolver.Util
open SudokuSolver.AlgorithmX

// ---
// Types
// ---


type DLXNode = {
    // leave this out?
    mutable row: int
    mutable col: int

    mutable left: int
    mutable right: int
    mutable up: int
    mutable down: int
}

type ColumnNode = {
    mutable size: int
    mutable rowStartIndex: int
}

type DLXProblem = {
    mutable columns: Map<int, ColumnNode>
    mutable nodeMap: Map<(int * int), DLXNode>
}

// ---
// Util
// ---

// let applyBoardToDLXMatrix (board: SolvingSudokuBoard) (matrix: DLXMatrix) : DLXMatrix =

let getKeys = Map.toSeq >> Seq.map fst >> Seq.toArray

let circularIndex i (arr: 'a array) =
    let l = arr.Length
    if i < 0 then l - 1 else if i >= l then 0 else i

let accessCircularly2D ij  (mat: 'a array array) =
    let i, j = ij
    let d1,d2 = mat.GetLength 0, mat[0].GetLength 0
    let i' = if i < 0 then d1 - 1 else if i >= d1 then 0 else i
    let j' = if j < 0 then d2 - 1 else if j >= d2 then 0 else j

    mat[i'][j']

let printDLXProblem (problem: DLXProblem) =
    let columns = problem.columns |> Map.toArray |> Array.map snd
    let columnStartIndices = columns |> Array.map (fun col -> col.rowStartIndex)
    let columnSizes = columns |> Array.map (fun col -> col.size)

    let nodeKeys = getKeys problem.nodeMap

    let nodeValues = nodeKeys |> Array.map (fun key -> problem.nodeMap.[key])

    let nodeValuesStr = nodeValues |> Array.map (fun node -> sprintf "at (%d,%d) l: (%d,%d), r: (%d,%d), u: (%d,%d), d: (%d,%d)" node.row node.col node.row node.left node.row node.right node.up node.col node.down node.col)

    printfn "Column start indices: %A" columnStartIndices
    printfn "Column sizes: %A" columnSizes
    printfn "Node values: %A" nodeValuesStr

// ---
// Conversion
// ---

let contraintMatrixToDLXMatrix (matrix: bool array2d) : DLXProblem = 
    let rowIndex = 
        [| 0..matrix.GetLength 0 - 1 |]
        |> Array.map (fun rowI ->
            matrix[rowI, *]
            |> Array.filterReturnIndex id
        )
        
    let colIndex = 
        [| 0..matrix.GetLength 1 - 1 |]
        |> Array.map (fun colI ->
            matrix[*, colI]
            |> Array.filterReturnIndex id
        )

    let matrixNodeIndices = 
        matrix
        |> Array2D.filterReturnIndex id

    let nodeMap =
        matrixNodeIndices
        |> Array.map(fun (origRowI, origColI) ->
            {
                row = origRowI
                col = origColI

                left = accessCircularly2D (origColI-1,origRowI) colIndex
                right = accessCircularly2D (origColI+1,origRowI) colIndex
                up = accessCircularly2D (origRowI-1,origColI) rowIndex
                down = accessCircularly2D (origRowI+1,origColI) rowIndex
            }
        )
        |> Array.map (fun node -> ((node.row, node.col), node))
        |> Map.ofArray

    let columnStartIndex =
        colIndex
        |> Array.map (fun col -> Array.min col)

    let nodeKeys = getKeys nodeMap
    
    let columns =
        columnStartIndex
        |> Array.mapi (fun column startI ->
            {
                size = nodeKeys |> Array.filter (fun (rowI, colI) -> colI = column) |> Array.length
                rowStartIndex = startI
            }
        )
        |> Array.mapi (fun i col -> i, col)
        |> Map.ofArray

    {
        columns = columns
        nodeMap = nodeMap
    }


// ---
// DLX Algorithm
// ---

let iterateOverRow (node: DLXNode) = (node.right, node.col), node.col
let iterateOverColumn (node: DLXNode) = (node.down, node.row), node.row
let iterateOverColumnAccumulatingNodes (node: DLXNode) = (node.down, node.row), node

let accumulateNodesOfRing problem iterationFn startNode =
    let rec accumulateNodesOfRingInner matrix iterationFn startNode acc currentNode =
        if currentNode = startNode then acc else

        let nextNodeCoords, nodeIdx = iterationFn currentNode
        let nextNode = matrix.nodeMap[nextNodeCoords]
        accumulateNodesOfRingInner matrix iterationFn startNode (nodeIdx::acc) nextNode

    accumulateNodesOfRingInner problem iterationFn startNode [] startNode

let getAvailableRowsForColumn problem (columnI: int) : DLXNode list =
    let startNode = problem.nodeMap[(problem.columns[columnI].rowStartIndex, columnI)]
    accumulateNodesOfRing problem iterateOverColumnAccumulatingNodes startNode

let removeColumn (problem: DLXProblem) (colI: int) =
    let mutable problem = problem

    let coordsBefore = (problem.columns[colI].rowStartIndex, colI)
    let startNode = problem.nodeMap[coordsBefore]

    let rowIndices = accumulateNodesOfRing problem iterateOverColumn startNode

    rowIndices
    |> List.iter (fun rowI -> 
        let node = problem.nodeMap[(rowI, colI)]
        problem.nodeMap[(rowI, node.left)].right <- node.right
        problem.nodeMap[(rowI, node.right)].left <- node.left

        problem.nodeMap <- problem.nodeMap.Remove ((rowI, colI))
    )

    problem.columns <- problem.columns.Remove colI 
    problem

let removeRow (problem: DLXProblem) (rowI: int) =
    let mutable problem = problem

    let startNode = problem.nodeMap[(rowI, 0)]
    let colIndices = accumulateNodesOfRing problem iterateOverRow startNode

    colIndices
    |> List.iter (fun colI -> 
        let node = problem.nodeMap[(rowI, colI)]
        problem.nodeMap[(node.up, colI)].down <- node.down
        problem.nodeMap[(node.down, colI)].up <- node.up

        problem.columns[colI].size <- problem.columns[colI].size - 1
    )

    problem
    
let reduceProblemDLX (problem: DLXProblem) (rowNode: DLXNode) : DLXProblem = 
    let mutable problem = problem

    let columnsOfSelectedRow = accumulateNodesOfRing problem iterateOverRow rowNode
    let rowsOfSelectedColumns = 
        columnsOfSelectedRow 
        |> List.map (getAvailableRowsForColumn problem)
        |> List.concat

    columnsOfSelectedRow |> List.iter (fun colI -> problem <- removeColumn problem colI)
    rowsOfSelectedColumns |> List.iter (fun rowNode -> problem <- removeRow problem rowNode.row)

    problem

let solveWithAlgorithmXUsingDLX (genericConstraintMatrix: bool array2d) (board: SolvingSudokuBoard) =
    let rec solve (acc: int list) (problem: DLXProblem) : int list option =
        let mutable problem = problem

        // no constraints exists to be satisfied
        if problem.columns.Count = 0 then Some acc else

        let rowIndices = 
            problem.columns 
            |> Map.toArray 
            |> Array.minBy (fun (colI, node) -> node.size) 
            |> fst 
            |> getAvailableRowsForColumn problem
        
        rowIndices
        |> List.tryPick (fun rowNodeChosen -> 
            rowNodeChosen |> (reduceProblemDLX problem >> solve acc >> Option.bind (fun newAcc -> Some (rowNodeChosen.row::newAcc)))
        )
    
    let maybeSolution = board |> applyBoardToConstraintMatrix genericConstraintMatrix |> contraintMatrixToDLXMatrix |> solve []
    // for later
    // let maybeSolution = board |> applyBoardToDLXMatrix genericDLXMatrix |> solve []

    match maybeSolution with
    | Some solution -> 
        List.fold (fun board matRowI -> applyRowToBoard matRowI board) board solution
    | None -> failwith "No solution"