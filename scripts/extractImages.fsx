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

module Option =
    type Builder() =
        member __.Bind(x, f) =
            Option.bind f x
        member __.Return x = Some x
        member __.ReturnFrom x = x

    let builder = Builder()

module Result =
    // refactor: use function from FsharpMyExtension (see https://github.com/gretmn102/FsharpMyExtension/issues/20)
    let defaultWith defThunk (result: Result<'T, 'Error>) =
        match result with
        | Ok x -> x
        | Error msg -> defThunk msg

[<RequireQualifiedAccess>]
type DataImageFormat =
    | Webp
    | Png
    | Jpeg
    | Unknown of string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module DataImageFormat =
    open FParsec

    let dic =
        [
            "webp", DataImageFormat.Webp
            "png" , DataImageFormat.Png
            "jpg" , DataImageFormat.Jpeg
            "jpeg", DataImageFormat.Jpeg
        ]

    let parser: Parser<_, unit> =
        let pcommon =
            dic
            |> Seq.sortByDescending (fun (rawName, _) -> rawName) // для жадности
            |> Seq.map (fun (rawName, name) ->
                pstringCI rawName >>% name
            )
            |> List.ofSeq
            |> choice
        let punknown =
            manySatisfy (isNoneOf "\n\";") |>> DataImageFormat.Unknown
        pcommon <|> punknown

type DataImage = {
    Format: DataImageFormat
    /// in base64 format
    Data: string
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module DataImage =
    open FParsec

    let pimageFormat: Parser<_, unit> =
        skipString "image" >>. skipChar '/'
        >>. DataImageFormat.parser

    // data:image/jpeg;base64,
    let parser: Parser<_, unit> =
        pipe2
            (skipString "data" >>. skipChar ':'
             >>. pimageFormat .>> skipChar ';')
            (pstring "base64" >>. skipChar ',' >>. todo)
            (fun imageFormat data -> {
                Format = imageFormat
                Data = data
            })

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
                    Option.builder {
                        do! if htmlElement.Tag <> "img" then None else Some ()
                        let! src =
                            htmlElement.Attributes
                            |> HtmlElementAttributes.tryFind "src"

                        return src
                    }

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
