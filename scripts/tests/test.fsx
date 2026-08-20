#!dotnet fsi
#r "nuget: Expecto, 10.2.1"
#load "Parsers/DataImageFormat.Test.fsx"
#load "Parsers/DataImage.Test.fsx"
open Expecto

open Twine.SugarCube.FSharp.DataImageFormat.Tests
open Twine.SugarCube.FSharp.DataImage.Tests

runTestsWithCLIArgs [] [||] (testList "all" [
    ``DataImageFormat.parser``
    ``DataImage.parser``
])
