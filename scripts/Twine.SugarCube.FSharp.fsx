#r "nuget: FSharpMyExt, 2.0.0-prerelease.11"
namespace Twine.SugarCube.FSharp

module ParserCommon =
    open FParsec

    let ptag : Parser<_, unit> =
        many1Satisfy (isNoneOf " \t\n")

[<RequireQualifiedAccess>]
type DataImageFormat =
    | Webp
    | Png
    | Jpeg
    | Unknown of string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module DataImageFormat =
    let dic =
        [
            "webp", DataImageFormat.Webp
            "png" , DataImageFormat.Png
            "jpg" , DataImageFormat.Jpeg
            "jpeg", DataImageFormat.Jpeg
        ]

    let formatRaws =
        dic
        |> List.map (fun (k, v) -> v, k)
        |> Map.ofList

    open FsharpMyExtension.Serialization.Serializers.ShowList

    let show (format: DataImageFormat) : ShowS =
        match format with
        | DataImageFormat.Unknown unknownFormat ->
            showString unknownFormat
        | _ ->
            match Map.tryFind format formatRaws with
            | None ->
                showByToString format << showChar '?'
            | Some x ->
                showString x

    let toString (format: DataImageFormat) =
        let build = FsharpMyExtension.Serialization.Serializers.ShowList.show
        build (show format)

    open FParsec

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
    open FsharpMyExtension.Serialization.Serializers.ShowList

    let show (dataImage: DataImage) : ShowS =
        showString "data:image/" << DataImageFormat.show dataImage.Format
        << showString ";base64," << showString dataImage.Data

    open FParsec

    let pimageFormat: Parser<_, unit> =
        skipString "image" >>. skipChar '/'
        >>. DataImageFormat.parser

    let pbase64: Parser<_, unit> =
        // https://base64.guru/learn/base64-characters
        regex "[A-Za-z0-9+/]+={0,2}"

    /// ```
    /// data:image/jpeg;base64,
    /// ```
    let parser: Parser<_, unit> =
        pipe2
            (skipString "data" >>. skipChar ':'
             >>. pimageFormat .>> skipChar ';')
            (pstring "base64" >>. skipChar ',' >>. pbase64)
            (fun imageFormat data -> {
                Format = imageFormat
                Data = data
            })

[<RequireQualifiedAccess>]
type HtmlElementAttributeValue =
    | DataImage of DataImage
    | Raw of string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module HtmlElementAttributeValue =
    open FsharpMyExtension.Serialization.Serializers.ShowList

    let show (value: HtmlElementAttributeValue) : ShowS =
        match value with
        | HtmlElementAttributeValue.Raw raw ->
            showString raw
        | HtmlElementAttributeValue.DataImage dataImage ->
            DataImage.show dataImage

    open FParsec

    let praw: Parser<_, unit> =
        let quote =
            between
                (skipChar '"')
                (skipChar '"')
                (manySatisfy ((<>) '"'))
        let raw =
            manySatisfy (isNoneOf " >")
        quote <|> raw

    let parser: Parser<HtmlElementAttributeValue, unit> =
        (DataImage.parser |>> HtmlElementAttributeValue.DataImage)
        <|> (praw |>> HtmlElementAttributeValue.Raw)

type HtmlElementAttribute = (string * HtmlElementAttributeValue)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module HtmlElementAttribute =
    open FsharpMyExtension.Serialization.Serializers.ShowList

    let show ((key, value): HtmlElementAttribute) : ShowS =
        showString key << showChar '='
        << between
            (showChar '"')
            (showChar '"')
            (HtmlElementAttributeValue.show value)

    open FParsec

    open ParserCommon

    let parser: Parser<HtmlElementAttribute, unit> =
        tuple2 (ptag .>> skipChar '=') HtmlElementAttributeValue.parser

type HtmlElementAttributes = HtmlElementAttribute list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module HtmlElementAttributes =
    let tryFind key (attributes: HtmlElementAttributes) =
        attributes
        |> List.tryFind (fun (k, v) -> k = key)

    let set key newvalue (attributes: HtmlElementAttributes) =
        attributes
        |> List.map (fun (k, v) ->
            k, if k <> key then v else newvalue
        )

    open FsharpMyExtension.Serialization.Serializers.ShowList

    let show (attributes: HtmlElementAttributes) : ShowS =
        attributes
        |> List.map HtmlElementAttribute.show
        |> joinsEmpty showSpace

    open FParsec

    let parser: Parser<HtmlElementAttributes, unit> =
        many (HtmlElementAttribute.parser .>> spaces)

type HtmlElement = {
    Tag: string
    Attributes: HtmlElementAttributes
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module HtmlElement =
    open FsharpMyExtension.Serialization.Serializers.ShowList

    let show (htmlElement: HtmlElement) : ShowS =
        between
            (showChar '<')
            (showChar '>')
            (showString htmlElement.Tag << showSpace)

    let toString htmlElement =
        let build = FsharpMyExtension.Serialization.Serializers.ShowList.show
        build (show htmlElement)

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
