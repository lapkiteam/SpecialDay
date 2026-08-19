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

type HtmlElement = {
    Tag: string
    Attributes: (string * string) list
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module HtmlElementParser =
    open FParsec
    open FsharpMyExtension.Serialization.Deserializers.FParsec

    let ptag : Parser<_, unit> =
        many1Satisfy (isNoneOf " \t\n")

    let pattributeValue: Parser<_, unit> =
        let quote =
            between
                (skipChar '"')
                (skipChar '"')
                (manySatisfy ((<>) '"'))
        let raw =
            manySatisfy (isNoneOf " >")
        quote <|> raw

    let pattribute: Parser<_, unit> =
        tuple2 (ptag .>> skipChar '=') pattributeValue

    let parser: Parser<_, unit> =
        between
            (skipChar '<' >>. spaces)
            (skipChar '>')
            (pipe2
                (ptag .>> spaces)
                (many (pattribute .>> spaces))
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
            |> List.map (fun line ->
                Result.builder {
                    let! src = HtmlElementParser.parse line
                    if src.Tag = "img" then

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
