#!dotnet fsi
#load "lib/ImageMagick.FSharp/src/Api.fsx"

"src/images"
|> Api.convertFolder {
    OutputDirectory = Some "public/images"
    FitSize = None
    Dry = false
    InputFormats = fun ext -> ext = ".png" || ext = ".jpeg"
    OutputFormat = "webp"
}
