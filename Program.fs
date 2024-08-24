
open SudokuSolver.Domain
open SudokuSolver.Util
open SudokuSolver.Algorithms
open SudokuSolver.DLXAlgorithm
open SudokuSolver.Testing
open SudokuSolver.Loading

let algsToCompare = [
    //("Fake solver (calling direct possibilities inside)", fakeSolver)
    ("Direct Possibilities", solveSudokuStepwiseDirectP)
    //("Direct possibilities measuring time", solveSudokuStepwiseDirectPMeasuringTime)
    //("Direct and Indirect Possibilities chained", solveSudokuStepwiseIndirectChained relevantSquareGetters)
    ("Second order possibilities", backTrack (solveSudokuStepwiseWithSecondOrder relevantSquareGetters))
    ("Backtracking (d&i)", chainedWithBacktrack relevantSquareGetters)
    //("Algorithm X", solveWithAlgorithmX getGenericBoardConstraintMatrix)
]

let settings = {
    printShortStatsIndividually = false
    printErrorIndividually = false
    printProgress = true
    printProgressInterval = 1_000
    datasetSize = 1_000
    datasetSkip = 0
}

let dataSets = [
    ("Easy Kaggle Datatset (1M)", loadFromKaggleDataSet "../sudoku.csv" settings.datasetSkip 900_000, 900_000)
    ("Hardest", laodFromCsvWithDotAs0 "datasets/HardestDatabase110626.txt" 0 375 false, 375)
]

let workThroughDatasets (dataSets: (string * DataSet * int) list) algsToCompare settings = 
    dataSets 
    |> List.map (fun (name: string, dataSet: DataSet, size) -> 
        let settings' = { settings with datasetSize = size; printProgressInterval = size / 100}
        let stats = dataSet |> (compareSolvingAlgs settings' algsToCompare)
        (name, stats)
    )
    |> List.iter (fun (name, stats) -> 
        printfn "Dataset: %s" name
        stats |> List.iter printStats
    )

workThroughDatasets dataSets algsToCompare settings

//let challengingBoardForBacktracking : UnsolvedSudokuBoard = laodFromCsvWithDotAs0 "datasets/HardestDatabase110626.txt" settings.datasetSkip settings.datasetSize false |> Seq.head |> fst
//let (solved, runtime) = solveOnePuzzleForAnalysis challengingBoardForBacktracking (solveWithAlgorithmX getGenericBoardConstraintMatrix)