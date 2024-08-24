module SudokuSolver.Testing

open SudokuSolver.Domain
open SudokuSolver.Util

let findSumOfSolutionDeltas (s1: SolvedSudokuBoard) (s2: SolvedSudokuBoard) =
    let s2Arr = s2 |> solvedSudokuBoardToIntArray
    s1
    |> solvedSudokuBoardToIntArray
    |> Array.mapi (fun i x -> if x = s2Arr.[i] then 0 else 1)
    |> Array.sum

let solveOnePuzzleForAnalysis problem alg =
    let input = convertToSolvingBoard problem
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let result = alg input
    stopWatch.Stop()
    (convertToSolvedBoard result, stopWatch.Elapsed.TotalMicroseconds)

let analyzeSolvingAlg settings index problem solution (alg: SolvingSudokuBoard -> SolvingSudokuBoard) : RuntimeStats =
    let (result, runtime) = solveOnePuzzleForAnalysis problem alg

    let success = 
        match result with
        | Ok s -> true
        | _ -> false
    let delta = 
        match (result, solution) with
        | Ok s, Some sol -> findSumOfSolutionDeltas s sol
        | _ -> 0
    let boardCompleteness =
        match result with
        | Ok s -> None
        | Error board -> float (countSolvedSquares board) / 81.0 |> Some

    if settings.printProgress && index % settings.printProgressInterval = 0 then printfn "%.2f%%" ((float index + 1.0) / float settings.datasetSize * 100.0)
    
    if settings.printErrorIndividually then
        match result with
            | Error board -> printfn "%s\n\n" (solvingBoardToString board)
            | _ -> ()

    if settings.printShortStatsIndividually then printfn "%f,%s,%i" runtime (if success then "T" else "F") delta

    { 
        success = success
        runtimeMicroS = runtime
        boardCompleteness = boardCompleteness
        deltaToSolution = delta
    }

let combineTestStats settings name tests : RuntimeStatsAgglomeration =   
    let testList = tests |> List.ofSeq |> List.tail // the first call seems inaccurate
    let testCount = testList.Length
    let averageSuccess = testList |> List.averageByOrZero (fun stats -> if stats.success then 1.0 else 0.0)
    let averageRuntime = testList |> List.map (fun stats -> stats.runtimeMicroS) |> List.averageOrZero
    let averageSuccessRuntime = testList |> List.map (fun stats -> if stats.success then stats.runtimeMicroS else 0.0) |> List.averageOrZero
    let averageDeltaFail = testList |> List.filter (fun stats -> not stats.success) |> List.averageByOrZero (fun stats -> float stats.deltaToSolution / 81.0)
    let averageDeltaSuccess = testList |> List.filter (fun (stats: RuntimeStats) -> stats.success) |> List.averageByOrZero (fun stats -> float stats.deltaToSolution / 81.0)
    let averageBoardCompleteness = testList |> List.choose (fun stats -> stats.boardCompleteness) |> List.averageOrZero

    {
        name = name
        successRatio = averageSuccess
        runtimeMS = averageRuntime
        boardCompleteness = averageBoardCompleteness
        successRuntimeMS = averageSuccessRuntime
        deltaToSolutionFail = averageDeltaFail
        deltaToSolutionSuccess = averageDeltaSuccess
        testStartIndex = settings.datasetSkip
        testCount = testCount
    }

let compareSolvingAlgs (settings: AnalysisSettings) (solveFns: solvingAlg list) (boardsDataset: (SudokuCell array2d * SolvedSudokuBoard option) seq) = 
    if settings.printShortStatsIndividually then printfn "T (ms),Success,Delta"

    if boardsDataset |> Seq.length < 1 then failwith "Dataset too small. Must be at least 2."

    solveFns
    |> List.map ( fun (name, solveFn) -> 
        printfn "Testing %s" name

        boardsDataset 
        |> List.ofSeq
        |> List.mapi (fun i (p, s) -> analyzeSolvingAlg settings i p s solveFn)
        |> combineTestStats settings name
    )

let printStats stats =
    printfn "### %s" stats.name
    printfn "- Success Ratio: **%.2f**" stats.successRatio
    printfn "- Average Runtime: **%.0f μs**" (stats.runtimeMS)
    printfn "- Average Successful Runtime: **%.0f μs**" (stats.successRuntimeMS)
    printfn "- Average Board Completeness (for failures): **%.2f%%**" (stats.boardCompleteness * 100.0)
    printfn "- Average Delta to Solution: **%.2f%%**" (stats.deltaToSolutionFail * 100.0)
    printfn "- Average Delta to Solution (for successes): **%.2f%%**" (stats.deltaToSolutionSuccess * 100.0)
    printfn "- Test Count: **%i** (Starting at %i)" stats.testCount stats.testStartIndex
