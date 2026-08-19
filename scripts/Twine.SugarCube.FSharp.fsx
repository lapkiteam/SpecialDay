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
