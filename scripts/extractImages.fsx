#!/usr/bin/env -S dotnet fsi
#r "nuget: FSharpMyExt, 2.0.0-prerelease.11"
#r "nuget: Twine.Twee.Fsharp, 0.5.0"
open System.IO
open FsharpMyExtension
open FsharpMyExtension.Containers
open Twine.Twee.FSharp
open Twine.Twee.FSharp.Parser
open Twine.Twee.FSharp.Printer

module Result =
    // refactor: use function from FsharpMyExtension (see https://github.com/gretmn102/FsharpMyExtension/issues/20)
    let defaultWith defThunk (result: Result<'T, 'Error>) =
        match result with
        | Ok x -> x
        | Error msg -> defThunk msg

module ParserCommon =
    open FParsec

    let ptag : Parser<_, unit> =
        many1Satisfy (isNoneOf " \t\n")

type HtmlElementAttribute = (string * string)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module HtmlElementAttribute =
    open FParsec

    open ParserCommon

    let pvalue: Parser<_, unit> =
        let quote =
            between
                (skipChar '"')
                (skipChar '"')
                (manySatisfy ((<>) '"'))
        let raw =
            manySatisfy (isNoneOf " >")
        quote <|> raw

    let parser: Parser<_, unit> =
        tuple2 (ptag .>> skipChar '=') pvalue

type HtmlElementAttributes = HtmlElementAttribute list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module HtmlElementAttributes =
    let tryFind key (attributes: HtmlElementAttributes) =
        attributes
        |> List.tryFind (fun (k, v) -> k = key)

    open FParsec

    let parser: Parser<_, unit> =
        many (HtmlElementAttribute.parser .>> spaces)

type HtmlElement = {
    Tag: string
    Attributes: HtmlElementAttributes
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module HtmlElement =
    open FParsec
    open FsharpMyExtension.Serialization.Deserializers.FParsec

    open ParserCommon

    let parser: Parser<_, unit> =
        between
            (skipChar '<' >>. spaces)
            (skipChar '>')
            (pipe2
                (ptag .>> spaces)
                HtmlElementAttributes.parser
                (fun tag attributes ->
                    {
                        Tag = tag
                        Attributes = attributes
                    }
                )
            )

    let parse =
        runResult parser

do
    let input = "src/game.twee"
    let document =
        Document.rawParseFile input
        |> Result.defaultWith (
            failwithf "%A"
        )

    let document =
        let mapPassageBody (passageBody: PassageBody) =
            passageBody
            |> List.mapFold (fun line ->
                let f (htmlElement: HtmlElement) =
                    if htmlElement.Tag <> "img" then None
                    else
                        let src =
                            htmlElement.Attributes
                            |> HtmlElementAttributes.tryFind "src"
                        sdf
                Result.builder {
                    let! src = HtmlElement.parse line
                    if src.Tag <> "img" then
                        ok
                        todo
                    return src
                }
            )
        document
        |> List.map (fun passage ->
            { passage with
                Body =
                    mapPassageBody passage.Body
            }
        )

    let rawDocument =
        document
        |> Document.toString
            PassageBody.shows
            NewlineType.CrLf
    File.WriteAllText(input, rawDocument)
