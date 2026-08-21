module Twine.SugarCube.FSharp.HtmlElementAttribute.Tests
#r "nuget: Expecto, 10.2.1"
#load "../../Twine.SugarCube.FSharp.fsx"
open Expecto
open FsharpMyExtension.Serialization.Deserializers.FParsec

open Twine.SugarCube.FSharp

[<Tests>]
let ``HtmlElementAttribute.parser`` =
    let parse = runResult HtmlElementAttribute.parser
    testList "HtmlElementAttribute.parser" [
        testCase "key=value" <| fun () ->
            Expect.equal
                (parse "key=value")
                (Result.Ok ("key", HtmlElementAttributeValue.Raw "value"))
                ""
    ]
