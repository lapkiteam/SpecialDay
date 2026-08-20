module Twine.SugarCube.FSharp.DataImage.Tests
#r "nuget: Expecto, 10.2.1"
#load "../../Twine.SugarCube.FSharp.fsx"
open Expecto
open FsharpMyExtension.Serialization.Deserializers.FParsec

open Twine.SugarCube.FSharp

[<Tests>]
let ``DataImage.parser`` =
    let parse = runResult DataImage.parser
    testList "DataImage.parser" [
        testCase "webp" <| fun () ->
            Expect.equal
                (parse "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQAAAQABAAD/== class=\"\"")
                (Result.Ok {
                    Format = DataImageFormat.Jpeg
                    Data = "/9j/4AAQSkZJRgABAQAAAQABAAD/=="
                })
                ""
    ]
