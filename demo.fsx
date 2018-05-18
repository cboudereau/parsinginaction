//https://github.com/cboudereau/parsinginaction
























#I __SOURCE_DIRECTORY__
#r "packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "packages/FParsec/lib/net40-client/FParsec.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Drawing.dll"
#r "System.Numerics.dll"

open FParsec
// spec : https://developers.google.com/protocol-buffers/docs/proto3
let (<!>) f x = x |>> f
let (<*>) f x = f >>= fun f' -> x >>= fun x' -> preturn (f' x')
























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
let pMessage : Parser<string,unit> = failwith "not yet implemented" 

run pMessage "message CalculateInfo {"
run pMessage "message CalculateInfo{"























//STEP 2 : PARSE 1 FIELD

//STEP 2.1 : Parse field rule (ie: required)
let pRequired : Parser<FieldRule, unit> = failwith "not yet implemented" 
run pRequired "required string CalStarttime=1"

let pOptional : Parser<FieldRule, unit> = failwith "not yet implemented" 
run pOptional "optional string CalStarttime=1"

let pRepeated : Parser<FieldRule, unit> = failwith "not yet implemented" 
run pRepeated "repeated string CalStarttime=1"

let pFieldRule : Parser<FieldRule, unit> = failwith "not yet implemented" 

run pFieldRule "required string CalStarttime=1"
run pFieldRule "optional string CalStarttime=1"
run pFieldRule "repeated string CalStarttime=1"



























//STEP 2.2 : Parse field type (ie: string)
let pScalar : Parser<string, unit> = failwith "not yet implemented" 
run pScalar "string"
run pScalar "boolean"
run pScalar "hello"






























//STEP 2.2 :Refactor
let pType : Parser<Type, unit> = failwith "not yet implemented" 

run pType "string" 
run pType "bool"
run pType "hello"




























//Step 2.3 : parse field name
let pName : Parser<string, unit> = failwith "not yet implemented" 
run pName "CalStarttime =1"

























//Step 2.4 : parse field Id
let pId : Parser<int, unit> = failwith "not yet implemented" 
run pId "=1"
run pId "= 1"


























//3 : Compose to parse a entire field
//3.1 : Write the function that have the same flow
let fieldSpec fr t n i = { Field.Id=i; Type=t; Name=n; FieldRule=fr }

let pField : Parser<Field, unit> = failwith "not yet implemented" 

run pField "required string CalStarttime=1"
run pField "required string CalStarttime = 1"



























//4 : Compose Field to get field list
let pFields : Parser<Field list, unit> = failwith "not yet implemented" 

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

































//5 write message
let message name fields = { Name = name; Fields = fields }

let pProtoMessage : Parser<Message, unit> = failwith "not yet implemented" 

run pProtoMessage sample

let pProtos : Parser<Message list, unit> = failwith "not yet implemented" 

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