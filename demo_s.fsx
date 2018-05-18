#I __SOURCE_DIRECTORY__
#r "packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "packages/FParsec/lib/net40-client/FParsec.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Drawing.dll"
#r "System.Numerics.dll"

open FParsec
// spec : https://developers.google.com/protocol-buffers/docs/proto3
type Type = 
    | Scalar of string
    | Custom of string

type FieldRule = 
    | Required
    | Optional
    | Repeated

type Field = 
    { Id : int
      Type: Type
      FieldRule : FieldRule
      Name : string }

type Message = 
    { Name : string
      Fields : Field list } 

//STEP 1 : EXTRACTING THE NAME OF THE MESSAGE
let msgSample = "message CalculateInfo {"

//STEP 1 Solution
let pMessage = spaces >>. pstring "message" .>> spaces >>. (noneOf [' '; '{'] |> many1Chars)

run pMessage msgSample
run pMessage "message CalculateInfo{"

//STEP 2 : PARSE 1 FIELD

//STEP 2.1 : Parse field rule (ie: required)
let fieldSample = "required string CalStarttime=1"

let pRequired = stringReturn "required" Required
let pOptional = stringReturn "optional" Optional
let pRepeated = stringReturn "repeated" Repeated

run pRequired fieldSample
run pOptional "optional string CalStarttime=1"
run pRepeated "repeated string CalStarttime=1"

let pFieldRule = pRequired <|> pOptional <|> pRepeated

run pFieldRule fieldSample
run pFieldRule "optional string CalStarttime=1"
run pFieldRule "repeated string CalStarttime=1"

//STEP 2.2 : Parse field type (ie: string)
let pScalar = 
    [ "string"
      "boolean" ] 
    |> List.map pstring |> List.fold (<|>) pzero

run pScalar "string"
run pScalar "boolean"
run pScalar "hello"

module ScalarType = 
    let types = 
        [ "double"
          "float"
          "int32"
          "int64"
          "uint32"
          "uint64"
          "sint32"
          "sint64"
          "fixed32"
          "fixed64"
          "sfixed32"
          "sfixed64"
          "bool"
          "string"
          "bytes" ]

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

//STEP 2.2 :Refactor

let pType = 
    let pScalar = ScalarType.parser |>> Scalar
    let pCustom = (<>) ' ' |> satisfy |> manyChars |>> Custom
    pScalar <|> pCustom

run pType "string"
run pType "bool"
run pType "hello"

//Step 2.3 : parse field name
let pName = noneOf [' '; '='] |> many1Chars

run pName "CalStarttime =1"

//Step 2.4 : parse field Id
let pId = pchar '=' >>. spaces >>. pint32

run pId "=1"
run pId "= 1"

//3 : Compose to parse a entire field
let (<!>) f x = x |>> f
let (<*>) f x = f >>= fun f' -> x >>= fun x' -> preturn (f' x')

//3.1 : Write the function that have the same flow
let fieldSpec fr t n i = { Field.Id=i; Type=t; Name=n; FieldRule=fr }

let pField = 
    fieldSpec
    <!> (spaces >>. pFieldRule)
    <*> (spaces >>. pType)
    <*> (spaces >>. pName)
    <*> (spaces >>. pId)

run pField fieldSample
run pField "required string CalStarttime = 1"

//4 : Compose Field to get field list
let pFields = sepEndBy pField (spaces >>. pchar ';' .>> spaces)

run pFields "required string CalStarttime = 1;required string Toto = 2"
run pFields "required string CalStarttime = 1;
required string Toto = 2"

let sample = "message CalculateInfo {
    required string CalStarttime=1;
    optional string CalEndtime=2;
    required string Smiles=3;
    optional string CAS=4;
    optional string ChName=5;
    optional string EnName=6;
    required string Param=7;
    required bytes Result=8;
    required bool IsFinished=9; }"

run pMessage sample

//5 write message
let message name fields = { Name = name; Fields = fields }

let pProtoMessage = 
    message
    <!> pMessage
    <*> between (spaces .>> pchar '{') (spaces .>> pchar '}') pFields 

run pProtoMessage sample

let pProtos = sepEndBy pProtoMessage spaces

run pProtos "    message CalculateInfo {
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
        repeated CalculateInfo History=3; }"