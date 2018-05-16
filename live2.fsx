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

run ScalarType.parser "bool"
run ScalarType.parser "hello"

//STEP 1 : EXTRACTING THE NAME OF THE MESSAGE
let msgSample = "message CalculateInfo {"
let msgSample2 = "message CalculateInfo{"

let ws = pchar ' ' |> manyChars

let pMessageName = 
    let pMn = noneOf [' ';'{'] |> manyChars

    pstringCI "message" .>> ws >>. pMn

let message name fields = { Name=name; Fields=fields }

let (<!>) f x = x |>> f 

let fieldSample = "required string CalStarttime=1;"

let pRequired = stringCIReturn "required" Required
let pOptional = stringCIReturn "optional" Optional
let pRepeated = stringCIReturn "repeated" Repeated

let pFieldRules = pRequired <|> pOptional <|> pRepeated

let field fieldRules t n i = fieldRules { Type=t;Id=i;Name=n }


let pWord = (<>) ' ' |> satisfy |> manyChars

let pType = 
    ScalarType.parser |>> Scalar
    <|> (pWord |>> Custom)

let (<*>) f x = f >>= fun f' -> x >>= fun x' -> f' x' |> preturn

let pField = 
    let pFieldName = noneOf [ ' ';'=' ] |> manyChars
    field <!> pFieldRules .>> ws <*> pType .>> ws <*> pFieldName .>> ws .>> pchar '=' .>> ws <*> pint32

run pField fieldSample

let fieldsSample="""
    required string CalStarttime=1 ;
    optional string CalEndtime=2;
"""

let pFields = 
    let pSep = spaces >>. pchar ';' .>> spaces
    spaces >>. sepEndBy pField pSep

run pFields fieldsSample

let pMessage = 
    let pOpen = spaces >>. pchar '{' .>> spaces
    let pClose = spaces >>. pchar '}' .>> ws
    spaces >>. (message <!> pMessageName <*> (between pOpen pClose pFields))


let finalSample = """
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
    required bool IsFinished=9; }
    
    message GetAllCalulateResponse{
        required bool  isSuccessful = 1;
        required int32 Count=2;
        repeated CalculateInfo History=3; }
"""

run pMessage finalSample

let pMessages = sepEndBy pMessage spaces    
run pMessages finalSample
