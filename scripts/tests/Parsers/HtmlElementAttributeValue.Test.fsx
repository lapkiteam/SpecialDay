module Twine.SugarCube.FSharp.HtmlElementAttributeValue.Tests
#r "nuget: Expecto, 10.2.1"
#load "../../Twine.SugarCube.FSharp.fsx"
open Expecto
open FsharpMyExtension.Serialization.Deserializers.FParsec

open Twine.SugarCube.FSharp

[<Tests>]
let ``HtmlElementAttributeValue.parser`` =
    let parse = runResult HtmlElementAttributeValue.parser
    testList "HtmlElementAttributeValue.parser" [
        testCase "rawString>" <| fun () ->
            Expect.equal
                (parse "rawString>")
                (Result.Ok (HtmlElementAttributeValue.Raw "rawString"))
                ""
        testCase "data image" <| fun () ->
            Expect.equal
                (parse "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQAAAQABAAD/== ")
                (Result.Ok (HtmlElementAttributeValue.DataImage {
                    Format = DataImageFormat.Jpeg
                    Data = "/9j/4AAQSkZJRgABAQAAAQABAAD/=="
                }))
                ""
    ]
