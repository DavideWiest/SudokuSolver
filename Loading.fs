module SudokuSolver.Loading

open SudokuSolver.Domain
open SudokuSolver.Util

let readLimitedLines (filePath: string) (skip: int) (limit: int) (skipFirstLine: bool) =
    use reader = System.IO.File.OpenText(filePath)
    let rec readLines acc count =
        if count >= limit || reader.EndOfStream then
            List.rev acc
        else
            let line = reader.ReadLine()
            readLines (line :: acc) (count + 1)

    let skip' = if skipFirstLine then skip + 1 else skip
    for i in 1..(skip') do
        reader.ReadLine() |> ignore

    readLines [] 0

let loadFromKaggleDataSet filePath skip limit =
    readLimitedLines filePath skip limit true
    |> Seq.map (fun line ->
        let parts = line.Split(',')
        (parts[0], parts[1])
    )
    |> Seq.map (fun (problem, solution) -> 
        let loadedProblem: UnsolvedSudokuBoard = Array2D.fromArray 9 9 (unsolvedBoardFromDigitArray problem (fun d -> d = '0'))
        let loadedSolution: SolvedSudokuBoard = Array2D.fromArray 9 9 (solvedBoardFromDigitArray solution)
        (loadedProblem, Some loadedSolution)
    )

let laodFromCsvWithDotAs0 filePath skip limit skipFirstLine =
    readLimitedLines filePath skip limit skipFirstLine
    |> Seq.map (fun line ->
        let parts = line.Split(',')
        parts.[0]
    )
    |> Seq.map (fun problem -> 
        let loadedProblem: UnsolvedSudokuBoard = Array2D.fromArray 9 9 (unsolvedBoardFromDigitArray problem (fun d -> d = '.'))
        (loadedProblem, None)
    )