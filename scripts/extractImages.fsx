#!/usr/bin/env -S dotnet fsi
#r "nuget: FSharpMyExt, 2.0.0-prerelease.11"
#r "nuget: Twine.Twee.Fsharp, 0.5.0"
#load "Twine.SugarCube.FSharp.fsx"
open System.IO
open FsharpMyExtension
open FsharpMyExtension.Containers
open Twine.Twee.FSharp
open Twine.Twee.FSharp.Parser
open Twine.Twee.FSharp.Printer

open Twine.SugarCube.FSharp

module Result =
    // refactor: use function from FsharpMyExtension (see https://github.com/gretmn102/FsharpMyExtension/issues/20)
    let defaultWith defThunk (result: Result<'T, 'Error>) =
        match result with
        | Ok x -> x
        | Error msg -> defThunk msg

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
