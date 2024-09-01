
open SudokuSolver.Domain
open SudokuSolver.Util
open SudokuSolver.Algorithms
open SudokuSolver.AlgorithmX
open SudokuSolver.DLXAlgorithm
open SudokuSolver.Testing
open SudokuSolver.Loading

let algsToCompare = [
    //("Direct Possibilities", solveSudokuStepwiseDirectP)
    ////("Direct possibilities measuring time", solveSudokuStepwiseDirectPMeasuringTime)
    ////("Direct and Indirect Possibilities chained", solveSudokuStepwiseIndirectChained relevantSquareGetters)
    //("Backtracking with second order possibilities", backTrack (solveSudokuStepwiseWithSecondOrder relevantSquareGetters))
    
    //("Backtracking (d&i)", chainedWithBacktrack relevantSquareGetters)
    //("Algorithm X", solveWithAlgorithmX getGenericBoardConstraintMatrix)
    ("Algorithm X using DLX", solveWithAlgorithmXUsingDLX getGenericBoardConstraintMatrix)
]

let settings = {
    printShortStatsIndividually = false
    printErrorIndividually = false
    printProgress = true
    rethrowExceptions = true
    printProgressInterval = 1
    datasetSize = 0 // will be overwritten by the dataset list
    datasetSkip = 0
}

let dataSets: (string * (UnsolvedSudokuBoard * SolvedSudokuBoard option) seq * int) list = [
    //("Easy Kaggle Datatset (1M)", loadFromKaggleDataSet "../sudoku.csv" settings.datasetSkip 100, 100)
    ("Hardest", laodFromCsvWithDotAs0 "datasets/HardestDatabase110626.txt" settings.datasetSkip 1 false, 1) // 375
]

let workThroughDatasets (dataSets: (string * DataSet * int) list) algsToCompare settings = 
    dataSets 
    |> List.map (fun (name: string, dataSet: DataSet, size) -> 
        let settings' = { settings with datasetSize = size; printProgressInterval = max (size / 100) 1}
        let stats = dataSet |> (compareSolvingAlgs settings' algsToCompare)
        (name, stats)
    )
    |> List.iter (fun (name, stats) -> 
        printfn "Dataset: %s" name
        stats |> List.iter printStats
    )

workThroughDatasets dataSets algsToCompare settings

//let solved = selectRows (testConstraintMatrix3 |> contraintMatrixToDLXMatrix)
//printfn "%A" solved

// testing a challenging board

//let challengingBoardForBacktracking : UnsolvedSudokuBoard = laodFromCsvWithDotAs0 "datasets/HardestDatabase110626.txt" settings.datasetSkip settings.datasetSize false |> Seq.head |> fst
//let (solved, runtime) = solveOnePuzzleForAnalysis challengingBoardForBacktracking (solveWithAlgorithmX getGenericBoardConstraintMatrix)