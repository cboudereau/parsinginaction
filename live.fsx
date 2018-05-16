#I __SOURCE_DIRECTORY__
#r "packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "packages/FParsec/lib/net40-client/FParsec.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Drawing.dll"
#r "System.Numerics.dll"
open FParsec
open System

// spec : https://developers.google.com/protocol-buffers/docs/proto3
type Type = 
    | Scalar of string
    | Custom of string

type FieldSpec = 
    { Id : int
      Type: Type
      Name : string }

type Field = 
    | Required of FieldSpec
    | Optional of FieldSpec
    | Repeated of FieldSpec

type Message = 
    { Name : string
      Fields : Field list } 

module ScalarType = 
    let private mapping = 
        [ "double",  typeof<double>
          "float",   typeof<float>
          "int32",   typeof<int>
          "int64",   typeof<int64>
          "uint32",  typeof<uint32>
          "uint64",  typeof<uint64>
          "sint32",  typeof<int32>
          "sint64",  typeof<int64>
          "fixed32", typeof<uint32>
          "fixed64", typeof<uint64>
          "sfixed32",typeof<int32>
          "sfixed64",typeof<int64>
          "bool",    typeof<bool>
          "string",  typeof<string>
          "bytes",   typeof<byte[]> ]
        |> Map.ofList
    
    let parser = 
        mapping
        |> Map.toSeq
        |> Seq.map (fst >> pstring)
        |> Seq.fold (<|>) pzero

let ws = pchar ' ' |> manyChars
let pWord = (<>) ' ' |> satisfy |> manyChars

run pWord "hello"

run ScalarType.parser "hello"
run ScalarType.parser "bool"

let pType = 
    ScalarType.parser |>> Scalar
    <|> (pWord |>> Custom)

run pType "bool"
run pType "hello"

//STEP 1 : EXTRACTING THE NAME OF THE MESSAGE
let msgSample = "message CalculateInfo {"
let msgSample2 = "message CalculateInfo{"

let pMessageName = noneOf [' '; '{'] |> manyChars
let pMessage = pstringCI "message" >>. ws >>. pMessageName

run pMessage msgSample
run pMessage msgSample2

let fieldSample = "required string CalStarttime=1;"

let field fieldRules t n i = fieldRules { Type=t; Name=n; Id=i }

let pRequired = stringCIReturn "required" Required
let pOptional = stringCIReturn "optional" Optional
let pRepeated = stringCIReturn "repeated" Repeated

let pFieldRules = pRequired <|> pOptional <|> pRepeated

let (<!>) f x = x |>> f
let (<*>) f x = f >>= fun f' -> x >>= fun x' -> f' x' |> preturn

let pName = (<>) '=' |> satisfy |> manyChars

let pField = field <!> pFieldRules .>> ws <*> pType .>> ws <*> pName .>> ws .>> pchar '=' .>> ws <*> pint32
let pFields = 
    let pSep = spaces >>. pchar ';' .>> spaces
    spaces >>. sepEndBy pField pSep
run pFields fieldSample

let message name fields = { Fields=fields; Name = name }

let pProtoMessage = 
    let pOpen = spaces >>. pchar '{'
    let pClose = spaces >>. pchar '}'
    let pMessages = between pOpen pClose pFields
    spaces >>. (message <!> pMessage <*> pMessages)

run pProtoMessage 

let pProtos = sepEndBy pProtoMessage spaces

let prettyM m = 
    let sb  = System.Text.StringBuilder()
    let append x = sb.AppendLine x |> ignore
    sprintf "message %s {" m.Name |> append

    m.Fields 
    |> List.iter (fun f -> 
        let typeName = function Scalar x | Custom x -> x 
        let fieldSpec x = sprintf "%s %s=%i" (typeName x.Type) x.Name x.Id
        match f with 
        | Required x -> fieldSpec x |> sprintf "required %s" 
        | Optional x -> fieldSpec x |> sprintf "optional %s"
        | Repeated x -> fieldSpec x |> sprintf "repeated %s"
        |> append
        append "}"
        )

    sb.ToString ()

fsi.AddPrinter<Message> prettyM
fsi.AddPrinter<Message list> (List.map prettyM >> String.concat System.Environment.NewLine)

run pProtos """
message CalculateInfo {
    required string CalStarttime=1;
    optional string CalEndtime=2;
    required string Smiles=3;
    optional string CAS=4;
    optional string ChName=5;
    optional string EnName=6;
    required string Param=7;
    required bytes Result=8;
    required bool IsFinished=9;
    required bool IsFinished=9; }"""
