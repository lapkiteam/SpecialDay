#!dotnet fsi
#r "nuget: Expecto, 10.2.1"

#load "Parsers/DataImageFormat.Test.fsx"
open Expecto

open Twine.SugarCube.FSharp.DataImageFormat.Tests

runTestsWithCLIArgs [] [||] (testList "all" [
    ``DataImageFormat.parser``
])
