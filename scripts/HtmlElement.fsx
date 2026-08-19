#r "nuget: FSharpMyExt, 2.0.0-prerelease.11"

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
