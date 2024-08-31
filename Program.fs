
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
    //("Second order possibilities", backTrack (solveSudokuStepwiseWithSecondOrder relevantSquareGetters))
    ("Backtracking (d&i)", chainedWithBacktrack relevantSquareGetters)
    ("Algorithm X using DLX", solveWithAlgorithmXUsingDLX getGenericBoardConstraintMatrix)
    ("Algorithm X", solveWithAlgorithmX getGenericBoardConstraintMatrix)
]

let settings = {
    printShortStatsIndividually = false
    printErrorIndividually = false
    printProgress = true
    printProgressInterval = 1
    datasetSize = 1
    datasetSkip = 0
}

let dataSets = [
    //("Easy Kaggle Datatset (1M)", loadFromKaggleDataSet "../sudoku.csv" settings.datasetSkip 999_999, 999_999)
    ("Hardest", laodFromCsvWithDotAs0 "datasets/HardestDatabase110626.txt" 0 20 false, 20) // 375
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

//let solved = selectRows [] (testMatrix2 |> contraintMatrixToDLXMatrix)
//printfn "%A" solved

// testing a challenging board

//let challengingBoardForBacktracking : UnsolvedSudokuBoard = laodFromCsvWithDotAs0 "datasets/HardestDatabase110626.txt" settings.datasetSkip settings.datasetSize false |> Seq.head |> fst
//let (solved, runtime) = solveOnePuzzleForAnalysis challengingBoardForBacktracking (solveWithAlgorithmX getGenericBoardConstraintMatrix)