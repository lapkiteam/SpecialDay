module Twine.SugarCube.FSharp.DataImageFormat.Tests
#r "nuget: Expecto, 10.2.1"
#load "../../Twine.SugarCube.FSharp.fsx"
open Expecto
open FsharpMyExtension.Serialization.Deserializers.FParsec

open Twine.SugarCube.FSharp

[<Tests>]
let ``DataImageFormat.parser`` =
    let parse = runResult DataImageFormat.parser
    testList "DataImageFormat.parser" [
        testCase "webp" <| fun () ->
            Expect.equal
                (parse "webp;")
                (Result.Ok DataImageFormat.Webp)
                ""
    ]
