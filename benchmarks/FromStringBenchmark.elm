module FromStringBenchmark exposing (suite, main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigIntString)
import BigInt as BI


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "fromString"
        [ benchmark "a 32-digit number" (\_ -> BI.fromString bigIntString)
        ]
