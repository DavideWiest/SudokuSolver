module SudokuSolver.Domain 

type SudokuCell = 
    | Empty
    | Number of int

type SudokuRow = SudokuCell []
type SudokuBlock = SudokuCell [,]

type UnsolvedSudokuBoard = SudokuCell array2d

type SolvingSudokuSquare = 
    | SolvedCell of int
    | UnsolvedCell of int list

type SolvingSudokuBoard = SolvingSudokuSquare array2d

type SquareArrayGetter = SolvingSudokuBoard -> int -> int -> SolvingSudokuSquare array

type SolvedSudokuBoard = int [,]

type DataSet = (UnsolvedSudokuBoard * SolvedSudokuBoard option) seq

type RuntimeStats = {
    success: bool
    runtimeMicroS: float
    boardCompleteness: float option
    deltaToSolution: int
}

type RuntimeStatsAgglomeration = {
    name: string
    successRatio: float
    runtimeMS: float
    boardCompleteness: float
    successRuntimeMS: float
    deltaToSolutionFail: float
    deltaToSolutionSuccess: float
    testStartIndex: int
    testCount: int
}

type solvingAlg = string * (SolvingSudokuBoard -> SolvingSudokuBoard)

type AnalysisSettings = {
    printShortStatsIndividually: bool
    printErrorIndividually: bool
    printProgress: bool
    rethrowExceptions: bool
    printProgressInterval: int

    datasetSize: int
    datasetSkip: int
}