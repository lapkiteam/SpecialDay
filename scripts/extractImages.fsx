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
    // refactor: use function from FsharpMyExtension (see https://github.com/gretmn102/FsharpMyExtension/issues/21)
    type Builder() =
        member __.Bind(x, f) =
            Option.bind f x
        member __.Return x = Some x
        member __.ReturnFrom x = x

    let builder = Builder()

module Result =
    // refactor: use `FsharpMyExtension.Containers.Result.defaultWith` (see https://github.com/gretmn102/FsharpMyExtension/issues/20)
    let defaultWith defThunk (result: Result<'T, 'Error>) =
        match result with
        | Ok x -> x
        | Error msg -> defThunk msg

    // refactor: use `FsharpMyExtension.Containers.Result.ofOption` (see https://github.com/gretmn102/FsharpMyExtension/issues/22)
    let ofOption error option =
        match option with
        | Some x -> Ok x
        | None -> Error error

    // refactor: use `FsharpMyExtension.Containers.Result.ofOptionWith` (see https://github.com/gretmn102/FsharpMyExtension/issues/22)
    let ofOptionWith errorThunk option =
        match option with
        | Some x -> Ok x
        | None -> Error (errorThunk ())

do
    let input = "src/game.twee"
    let imagesDir = "images"

    let document =
        Document.rawParseFile input
        |> Result.defaultWith (
            failwithf "%A"
        )

    let document, images =
        let newHtmlImgElement newImageId line =
            let getImage (htmlElement: HtmlElement) =
                Option.builder {
                    do! if htmlElement.Tag <> "img" then None else Some ()
                    let! _, srcValue =
                        htmlElement.Attributes
                        |> HtmlElementAttributes.tryFind "src"
                    let! dataImage =
                        match srcValue with
                        | HtmlElementAttributeValue.DataImage dataImage ->
                            Some dataImage
                        | _ -> None
                    return dataImage
                }

            let updateSrc imagePath htmlElement =
                { htmlElement with
                    Attributes =
                        htmlElement.Attributes
                        |> HtmlElementAttributes.set
                            "src"
                            (HtmlElementAttributeValue.Raw imagePath)
                }

            Result.builder {
                let! htmlElement = HtmlElement.parse line
                match getImage htmlElement with
                | None -> return None
                | Some dataImage ->
                    let imagePath =
                        let rawFormat =
                            DataImageFormat.toString dataImage.Format
                        $"%d{newImageId}.%s{rawFormat}"
                    return Some {|
                        Image = {|
                            Path = imagePath
                            Data = dataImage
                        |}
                        Element = updateSrc imagePath htmlElement
                    |}
            }

        let mapFoldPassageBody state (passageBody: PassageBody) : (PassageBody * _) =
            passageBody
            |> List.mapFold
                (fun (state: {| Id: int; DataImages: _ list |}) line ->
                    match newHtmlImgElement state.Id line with
                    | Ok (Some x) ->
                        let state =
                            {| state with
                                DataImages = x.Image :: state.DataImages
                                Id = state.Id + 1
                            |}
                        let line = HtmlElement.toString x.Element
                        line, state
                    | _ -> line, state
                )
                state

        document
        |> List.mapFold
            (fun state passage ->
                let passageBody, state =
                    mapFoldPassageBody state passage.Body
                let passage =
                    { passage with
                        Body = passageBody
                    }
                passage, state
            )
            {| Id = 0; DataImages = [] |}
        |> fun (document, result) ->
            (document: Document<_>), List.rev result.DataImages

    Directory.CreateDirectory imagesDir |> ignore
    do
        images
        |> List.iter (fun image ->
            let data = System.Convert.FromBase64String image.Data.Data
            File.WriteAllBytes(
                $"%s{imagesDir}/%s{image.Path}",
                data
            )
        )

    do
        document
        |> Document.toString
            PassageBody.shows
            NewlineType.CrLf
        |> fun rawDocument ->
            File.WriteAllText(input, rawDocument)
