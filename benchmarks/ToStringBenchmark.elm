module ToStringBenchmark exposing (suite, main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "toString"
        [ benchmark "32-digit number" (\_ -> BI.mul bigInt)
        ]
