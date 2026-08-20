#!dotnet fsi
#r "nuget: Expecto, 10.2.1"
#load "Parsers/DataImageFormat.Test.fsx"
#load "Parsers/DataImage.Test.fsx"
#load "Parsers/HtmlElementAttributeValue.Test.fsx"
open Expecto

open Twine.SugarCube.FSharp.DataImageFormat.Tests
open Twine.SugarCube.FSharp.DataImage.Tests
open Twine.SugarCube.FSharp.HtmlElementAttributeValue.Tests

runTestsWithCLIArgs [] [||] (testList "all" [
    ``DataImageFormat.parser``
    ``DataImage.parser``
    ``HtmlElementAttributeValue.parser``
])
