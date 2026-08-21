#!dotnet fsi
#load "lib/ImageMagick.FSharp/src/Api.fsx"
#load "lib/DeployImages/src/script.fsx"

let imagesPath = "src/images"

imagesPath
|> Api.convertFolder {
    OutputDirectory = Some "public/images"
    FitSize = None
    Dry = false
    InputFormats = fun ext -> ext = ".png" || ext = ".jpeg"
    OutputFormat = "webp"
    Quality = Some 92
}

Script.Images.fromDir imagesPath
|> Script.Images.removePngIfHasClip
