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
    let d1,d2 = mat.Length, mat[i].Length

    mat[modPosRepr i d1][modPosRepr j d2]

let printDLXProblem (problem: DLXProblem) =
    let columns = problem.columns |> Map.toArray
    let columnInfo = columns |> Array.map (fun (i, col) -> i, col.size, col.rowStartIndex) |> Array.map (fun (i, size, rowStartIndex) -> sprintf "%d, s: %d, startI: %d" i size rowStartIndex)

    let nodeKeys = getKeys problem.nodeMap

    let nodeValues = nodeKeys |> Array.map (fun key -> problem.nodeMap.[key])

    let nodeValuesStr = nodeValues |> Array.map (fun node -> sprintf "at (%d,%d) l: (%d,%d), r: (%d,%d), u: (%d,%d), d: (%d,%d)" node.row node.col node.row node.left node.row node.right node.up node.col node.down node.col)

    printfn "Node values: \n%s" (nodeValuesStr |> String.concat "\n")
    printfn "columnInfo: \n%s" (columnInfo |> String.concat "\n")

let nestedArrayToIndexDictArray index =
        index
        |> Array.map (Array.mapi (fun i x -> x, i))
        |> Array.map Map.ofArray

let testColumnSizesMatch problem =
    let nodeKeys = getKeys problem.nodeMap
    problem.columns
    |> Map.toArray
    |> Array.map (fun (colI, col) -> 
        let nodes = nodeKeys |> Array.filter (fun (_, colI') -> colI' = colI)
        col.size, nodes.Length
    )
    |> Array.forall (fun (size1, size2) -> size1 = size2)

// ---
// Conversion
// ---

let contraintMatrixToDLXMatrix (matrix: bool array2d) : DLXProblem = 
    // col,row array of indices with truth values
    let rowIndex = 
        [| 0..matrix.GetLength 0 - 1 |]
        |> Array.map (fun rowI ->
            matrix[rowI, *]
            |> Array.filterReturnIndex id
        )
    
    // row,col array of indices with truth values
    let colIndex = 
        [| 0..matrix.GetLength 1 - 1 |]
        |> Array.map (fun colI ->
            matrix[*, colI]
            |> Array.filterReturnIndex id
        )

    let rowDicts = nestedArrayToIndexDictArray rowIndex
    let colDicts = nestedArrayToIndexDictArray colIndex

    let matrixNodeIndices = Array2D.filterReturnIndex id matrix

    if matrixNodeIndices.Length = 0 then failwith "Empty matrix"

    let nodeMap =
        matrixNodeIndices
        |> Array.map(fun (origRowI, origColI) ->
            let rowTruthIndexOfThisCol = colDicts[origColI].TryFind origRowI |> Option.get
            let colTruthIndexOfThisRow = rowDicts[origRowI].TryFind origColI |> Option.get

            let left = accessCircularly2D (origRowI, colTruthIndexOfThisRow - 1) rowIndex
            let right = accessCircularly2D (origRowI, colTruthIndexOfThisRow + 1) rowIndex
            let up = accessCircularly2D (origColI, rowTruthIndexOfThisCol - 1) colIndex
            let down = accessCircularly2D (origColI, rowTruthIndexOfThisCol + 1) colIndex
            {
                row = origRowI
                col = origColI

                left = left
                right = right
                up = up
                down = down
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
                size = nodeKeys |> Array.filter (fun (_, colI) -> colI = column) |> Array.length
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

let iterateOverRow (node: DLXNode) = (node.row, node.right), node.col
let iterateOverColumn (node: DLXNode) = (node.down, node.col), node.row
let iterateOverColumnAccumulatingNodes (node: DLXNode) = (node.down, node.col), node

let accumulateNodesOfRing problem iterationFn startNode =
    let rec accumulateNodesOfRingInner problem iterationFn startNode acc currentNode =
        //printfn "Current node: %A" currentNode
        let nextNodeCoords, currentNodeAcc = iterationFn currentNode
        let acc' = currentNodeAcc::acc

        let nextNode = problem.nodeMap[nextNodeCoords]
        if nextNode = startNode then acc' else

        accumulateNodesOfRingInner problem iterationFn startNode acc' nextNode

    accumulateNodesOfRingInner problem iterationFn startNode [] startNode

let getAvailableRowsForColumn problem (columnI: int) : DLXNode list =
    let maybeStartNode = problem.nodeMap.TryFind(problem.columns[columnI].rowStartIndex, columnI)
    if maybeStartNode.IsNone then [] else
    accumulateNodesOfRing problem iterateOverColumnAccumulatingNodes maybeStartNode.Value

let removeColumn (problemIn: DLXProblem) (colI: int) =
    let mutable problem = problemIn

    let coordsStartNode = (problem.columns[colI].rowStartIndex, colI)
    let startNode = problem.nodeMap[coordsStartNode]

    let rowIndices = accumulateNodesOfRing problem iterateOverColumn startNode

    rowIndices
    |> List.iter (fun rowI -> 
        let node = problem.nodeMap[(rowI, colI)]
        problem.nodeMap[(rowI, node.left)].right <- node.right
        problem.nodeMap[(rowI, node.right)].left <- node.left

        problem.nodeMap <- problem.nodeMap.Remove (rowI, colI)
    )

    problem.columns <- problem.columns.Remove colI

    problem

let removeRow (problemIn: DLXProblem) (rowI: int) =
    let mutable problem = problemIn

    let maybeStartNode = getKeys problem.nodeMap |> Array.tryPick (fun (row, col) -> if row = rowI then Some problem.nodeMap[(row, col)] else None)
    // no nodes in row - nothing to remove
    if maybeStartNode.IsNone then problem else

    let startNode = maybeStartNode.Value
    let colIndices = accumulateNodesOfRing problem iterateOverRow startNode

    colIndices
    |> List.iter (fun colI -> 
        let node = problem.nodeMap[(rowI, colI)]
        problem.nodeMap[(node.up, colI)].down <- node.down
        problem.nodeMap[(node.down, colI)].up <- node.up

        problem.nodeMap <- problem.nodeMap.Remove (rowI, colI)
        problem.columns[colI].size <- problem.columns[colI].size - 1
        if problem.columns[colI].rowStartIndex = rowI then problem.columns[colI].rowStartIndex <- node.down
    )

    problem
    
let reduceProblemDLX (problemIn: DLXProblem) (rowNode: DLXNode) : DLXProblem = 
    let mutable problem = problemIn

    printfn "-----------------"
    printfn "BEFORE REDUCTION"
    printDLXProblem problem
    printfn "columns match %b" (testColumnSizesMatch problem)
    printfn "rowNode: %A" rowNode
    printfn "col size: %i" (problem.columns[rowNode.col].size)

    let columnsOfSelectedRow = accumulateNodesOfRing problem iterateOverRow rowNode

    let rowsOfSelectedColumns = 
        columnsOfSelectedRow 
        |> List.map (getAvailableRowsForColumn problem)
        |> List.concat
        |> List.distinctBy (fun node -> node.row)
    
    printfn "Removing columns %A" columnsOfSelectedRow
    printfn "Removing rows %A" (rowsOfSelectedColumns |> List.map (fun node -> node.row))

    columnsOfSelectedRow |> List.iter (fun colI -> problem <- removeColumn problem colI)
    rowsOfSelectedColumns |> List.iter (fun rowNode -> problem <- removeRow problem rowNode.row)

    problem

let rec selectRows (problem: DLXProblem) : int list option =
    // no constraints exists to be satisfied
    if problem.columns.Count = 0 then Some [] else

    printfn "columns match: %b" (testColumnSizesMatch problem)
    
    let (colI, col) = 
        problem.columns 
        |> Map.toArray 
        |> Array.minBy (fun (colI, node) -> node.size) 
    
    if col.size = 0 then None else

    let nextProblems = 
        getAvailableRowsForColumn problem colI 
        |> List.map (fun node -> node.row) 
        // error here
        |> List.map (fun rowI -> rowI, reduceProblemDLX problem problem.nodeMap[(rowI, colI)])

    nextProblems
    |> List.tryPick (fun (rowI, newProblem) ->         
        
        newProblem |> (selectRows >> Option.bind (fun newAcc -> Some (rowI::newAcc)))
    )

let solveWithAlgorithmXUsingDLX (genericConstraintMatrix: bool array2d) (board: SolvingSudokuBoard) =
    match board |> applyBoardToConstraintMatrix genericConstraintMatrix |> contraintMatrixToDLXMatrix |> selectRows with
    | Some solution -> 
        List.fold (fun board matRowI -> applyRowToBoard matRowI board) board solution
    | None -> failwith "No solution"
