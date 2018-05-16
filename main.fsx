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

//STEP 1 : EXTRACTING THE NAME OF THE MESSAGE
let msgSample = "message CalculateInfo {"

let pMessage = spaces >>. pstringCI "message" >>. spaces >>. (noneOf "{ " |> manyChars) 

run pMessage msgSample

//STEP 2 : PARSE 1 FIELD
let fieldSample = "required string CalStarttime=1"

let fieldSpec f t n i = f { FieldSpec.Id=i; Type=t; Name=n }
let pRequired = stringCIReturn "required" (fieldSpec Required)

run pRequired fieldSample

let ws = pchar ' ' |> manyChars |>> ignore

let pOptional = stringCIReturn "optional" (fieldSpec Optional)
let pRepeated = stringCIReturn "repeated" (fieldSpec Repeated)
let pFieldRules = ws >>. (pRequired <|> pOptional <|> pRepeated)

run pFieldRules "required string CalStarttime=1" //Success: <fun:pRequired@40-8>
run pFieldRules "repeated string CalStarttime=1" //Success: <fun:pRepeated@42-6>
run pFieldRules "optional string CalStarttime=1" //Success: <fun:pOptional@41-6>

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

run ScalarType.parser "double" //ParserResult<string,unit> = Success: "double"

//Cases with error
run ScalarType.parser "hello" (*val it : ParserResult<string,unit> =
  Failure:
Error in Ln: 1 Col: 1
hello
^
Expecting: 'bool', 'bytes', 'double', 'fixed32', 'fixed64', 'float', 'int32',
'int64', 'sfixed32', 'sfixed64', 'sint32', 'sint64', 'string', 'uint32' or
'uint64'*)

let pWord = (<>) ' ' |> satisfy |> manyChars

let pType = 
    (ScalarType.parser |>> Scalar)
    <|> (ws >>. pWord |>> Custom)

run pType "hello" //ParserResult<Type,unit> = Success: Custom "hello"

let (<*>) f x = f >>= fun f' -> x >>= fun x' -> preturn (f' x')

let pField = 
    let pName = ws >>. (noneOf " =" |> manyChars)
    let pId = ws >>. pstring "=" >>. spaces >>. pint32

    pFieldRules <*> (ws >>. pType) <*> (ws >>. pName) <*> pId

run pField "required string CalStarttime=1" 

//STEP 3 : PARSING N FIELDS
let pFields = spaces >>. sepEndBy pField (spaces >>. pchar ';' .>> spaces)

run pFields """required string CalStarttime=1;optional string CalEndtime=2"""

//with more samples
run pFields """required string CalStarttime=1; optional string CalEndtime=2;"""
run pFields """required string CalStarttime=1 ; optional string CalEndtime=2;"""
run pFields """
required string CalStarttime=1 ; optional string CalEndtime=2;"""
run pFields """
 required string CalStarttime=1 ; optional string CalEndtime=2;"""
run pFields """
    required string CalStarttime=1 ; optional string CalEndtime=2;"""
run pFields """
    required string CalStarttime=1;

    optional string CalEndtime=2;"""
run pFields """
    required string CalStarttime=1;
    optional string CalEndtime=2;
    
    """
run pFields """


    required string CalStarttime=1;
    optional string CalEndtime=2;
    
    """
run pFields """


    required string CalStarttime=1;

    optional string CalEndtime=2;
    
    """

//STEP FINAL : THE FULL PARSER
let message name fields = { Fields=fields; Name=name }

//let pProtoMessage = pMessage |>> message .>> spaces .>> pchar '{' .>> spaces <*> pFields .>> spaces .>> pchar '}'
let pProtoMessage = 
    let pOpen = spaces .>> pchar '{'
    let pClose = spaces .>> pchar '}'
    pMessage |>> message <*> between pOpen pClose pFields

let pProtos = sepEndBy pProtoMessage spaces

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
    required bool IsFinished=9; }

    message GetAllCalulateResponse{
        required bool  isSuccessful = 1;
        required int32 Count=2;
        repeated CalculateInfo History=3; }
"""