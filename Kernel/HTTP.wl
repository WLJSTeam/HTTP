(* ::Package:: *)

(* ::Chapter:: *)
(*HTTP*)


(*
    message - ByteArray passed from client side
    request - Association parsed from message
    response - Null | String | ByteArray for further sending to the client
*)


(* ::Program:: *)
(*+-----------------------------------------------+*)
(*|                HTTP HANDLER                   |*)
(*|                                               |*)
(*|              (reseive request)                |*)
(*|                      |                        |*)
(*|           [parse request to assoc]            |*)
(*|                      |                        |*)
(*|              <select pipeline>                |*)
(*|     /       /        |        \         \     |*)
(*|    ..   [get..]  [post..]  [delete..]   ..    |*)
(*|             \        |        /               |*)
(*|          [create string response]             |*)
(*|                      |                        |*)
(*|               {return to tcp}                 |*)
(*+-----------------------------------------------+*)


(* ::Section:: *)
(*Begin packge*)


BeginPackage["WLJS`HTTP`", {
    "WLJS`Objects`"
}];


(* ::Section::Closed:: *)
(*Names*)


ClearAll["`*"];


HTTPPacketQ::usage =
"HTTPPacketQ[packet] check that message was sent via HTTP protocol.";


HTTPPacketLength::usage =
"HTTPPacketLength[packet] returns expected message length.";


HTTPHandler::usage =
"HTTPHandler[opts] mutable type for the handling HTTP request.";


(* ::Section::Closed:: *)
(*Begin private context*)


Begin["`Private`"];


(* ::Section::Closed:: *)
(*HTTPPacketQ*)


HTTPPacketQ[___] := False;


byteArrayContainsQ[byteArray_ByteArray, substring_String] :=
StringContainsQ[ByteArrayToString[byteArray, "ISOLatin1"], substring];


byteArrayContainsQ[byteArray_ByteArray, subbyteArray_ByteArray] :=
byteArrayContainsQ[byteArray, ByteArrayToString[subbyteArray, "ISOLatin1"]];


byteArrayExtractString[dataByteArray_ByteArray, separatorByteArray_ByteArray -> n_Integer] :=
With[{
    data = ByteArrayToString[dataByteArray, "ISOLatin1"],
    separator = ByteArrayToString[separatorByteArray, "ISOLatin1"]
},
    StringExtract[data, separator -> n]
];


byteArrayStringMatchQ[byteArray_ByteArray, substring_] :=
StringMatchQ[ByteArrayToString[byteArray, "ISOLatin1"], substring, IgnoreCase -> True];


HTTPPacketQ[packet_Association?AssociationQ] /; KeyExistsQ[packet, "DataByteArray"] :=
With[{dataByteArray = packet["DataByteArray"]},
    byteArrayContainsQ[dataByteArray, $httpEndOfHead] &&
    byteArrayStringMatchQ[dataByteArray, StartOfString ~~ $httpMethods ~~ " /" ~~ __]
];


(* ::Section::Closed:: *)
(*HTTPPacketLength*)


HTTPPacketLength[packet_Association] :=
With[{dataByteArray = packet["DataByteArray"]},
    Module[{head},
        head = byteArrayExtractString[dataByteArray, $httpEndOfHead -> 1];

        (*Return: _Integer*)
        Which[
            StringContainsQ[head, "Content-Length: ", IgnoreCase -> True],
                StringLength[head] + 4 +
                ToExpression[StringTrim[StringExtract[ToLowerCase[head], "content-length: " -> 2, "\r\n" -> 1]]],
            True,
                Length[dataByteArray]
        ]
    ]
];


(* ::Section:: *)
(*HTTPHandler*)


(* ::Section::Closed:: *)
(*Default message handler*)


CreateType[HTTPHandler, {
    "MessageHandler" -> <||>,
    "DefaultMessageHandler" -> Function[<|"Code" -> 404, "Body" -> "NotFound"|>],
    "Deserializer" -> <||>,
    "DefaultDeserializer" -> $deserializer,
    "Serializer" -> <||>,
    "DefaultSerializer" -> $serializer,
    "Logger" -> None,
    "Icon" -> Import[FileNameJoin[Join[FileNameSplit[$InputFileName][[ ;; -3]], {"Images", "http-logo.png"}]]]
}];


handler_HTTPHandler[packet_Association] :=
With[{dataByteArray = packet["DataByteArray"]},
    Module[{request, response, result,
        deserializer, defaultDeserializer, serializer, defaultSerializer,
        messageHandler, defaultMessageHandler
    },
        deserializer = handler["Deserializer"];
        defaultDeserializer = handler["DefaultDeserializer"];
        serializer = handler["Serializer"];
        defaultSerializer = handler["DefaultSerializer"];
        messageHandler = handler["MessageHandler"];
        defaultMessageHandler = handler["DefaultMessageHandler"];

        (*Request: _Association*)
        request = parseRequest[dataByteArray, deserializer, defaultDeserializer];

        (*Result: _String | _Association*)
        result = ConditionApply[messageHandler, defaultMessageHandler][request];

        (*Result: HTTPResponse[]*)
        response = createResponse[result, serializer, defaultSerializer];

        (*Return: _String | ByteArray[]*)
        Which[
            StringQ @ response["Body"], ExportString[response, "HTTPResponse", CharacterEncoding -> "UTF-8"],
            True, ExportByteArray[response, "HTTPResponse", CharacterEncoding -> "UTF-8"]
        ]
    ]
];


(* ::Section::Closed:: *)
(*Add HTTPHandler*)


HTTPHandler /: AddTo[tcp_, http_HTTPHandler] := (
    tcp["CompleteHandler", "HTTP"] = HTTPPacketQ -> HTTPPacketLength;
    tcp["MessageHandler", "HTTP"] = HTTPPacketQ -> http;
    tcp
);


(* ::Section::Closed:: *)
(*Internal*)


$httpMethods = {"GET", "PUT", "DELETE", "HEAD", "POST", "CONNECT", "OPTIONS", "TRACE", "PATCH"};


$httpEndOfHead = StringToByteArray["\r\n\r\n"];


$errorResponse = <|"Code" -> 404, "Body" -> "Not found"|>;


parseRequest[dataByteArray_ByteArray, deserializer_, defaultDeserializer_] :=
Module[{request, head, body, bodyByteArray, encoding},
    head = byteArrayExtractString[dataByteArray, $httpEndOfHead -> 1];
    body = byteArrayExtractString[dataByteArray, $httpEndOfHead -> 2];

    request = First @ StringCases[
        StringExtract[head, "\r\n" -> 1],
        method__ ~~ " " ~~ url__ ~~ " " ~~ version__ :> Join[
            <|"Method" -> method|>,

            MapAt[Association, Key["Query"]] @
            MapAt[URLBuild, Key["Path"]] @
            <|URLParse[url]|>[[{"Path", "Query"}]],

            <|"Version" -> version|>
        ],
        IgnoreCase -> True
    ];

    request["Headers"] = Association[
        Map[Rule[#1, StringRiffle[{##2}, ":"]]& @@ Map[StringTrim]@StringSplit[#, ":"] &]@
        StringExtract[head, "\r\n\r\n" -> 1, "\r\n" -> 2 ;; ]
    ];

    encoding = getCharsetEncoding[getContentType[request]];

    With[{$bodyByteArray = StringToByteArray[body], $encoding = encoding},
        request["BodyByteArray"] := $bodyByteArray;
        request["BodyBytes"] := Normal[$bodyByteArray];
        request["Body"] := ByteArrayToString[$bodyByteArray, $encoding];
    ];

    With[{$data = conditionApply[deserializer, defaultDeserializer][request]},
        request["Data"] := $data;
    ];

    (*Return: <|
        "Metod" -> "GET" | "POST" | ..,
        "Path" -> "/path/to/resource",
        "Query" -> <|"key1" -> "value1"|>,
        "Version" -> "1.1",
        "Headers" -> <|"Connection" -> "keep-alive"|>,
        "BodyByteArray" :> ByteArray[{}],
        "BodyBytes" :> Normal[ByteArray[{}]],
        "Body" :> ByteArrayToString[ByteArray[{}], "UTF-8"],
        "Data" :> expr[..]
    |>*)
    Echo @ request
];


conditionApply[conditionAndFunctions: _?AssociationQ: <||>, defalut_: Function[Null], ___] :=
Function[
    With[{selected = SelectFirst[conditionAndFunctions, Function[f, First[f][##]], {defalut}]},
        selected[[-1]][##]
    ]
];


getCharsetEncoding[contentType_String] :=
If[StringContainsQ[contentType, "charset="],
    If[MissingQ[#], "ISOLatin1", #]& @
    $charsetToEncoding @
    ToLowerCase @
    StringTrim @
    First @
    StringSplit[StringExtract[contentType, "charset=" -> 2], ";"],
(*Else*)
    "ISOLatin1"
];


getContentType[request_Association] :=
First @ KeySelect[request["Headers"], StringMatchQ[#, "content-type", IgnoreCase -> True]&];


$charsetToEncoding = <|
    "utf-8" -> "UTF-8",
    "utf8" -> "UTF8",
    "iso-8859-1" -> "ISO8859-1",
    "iso-8859-2" -> "ISO8859-2",
    "iso-8859-3" -> "ISO8859-3",
    "iso-8859-4" -> "ISO8859-4",
    "iso-8859-5" -> "ISO8859-5",
    "iso-8859-6" -> "ISO8859-6",
    "iso-8859-7" -> "ISO8859-7",
    "iso-8859-8" -> "ISO8859-8",
    "iso-8859-9" -> "ISO8859-9",
    "iso-8859-10" -> "ISO8859-10",
    "iso-8859-11" -> "ISO8859-11",
    "iso-8859-13" -> "ISO8859-13",
    "iso-8859-14" -> "ISO8859-14",
    "iso-8859-15" -> "ISO8859-15",
    "iso-8859-16" -> "ISO8859-16",
    "windows-1251" -> "WindowsCyrillic",
    "windows-1252" -> "WindowsANSI",
    "windows-1250" -> "WindowsEastEurope",
    "windows-1253" -> "WindowsGreek",
    "windows-1254" -> "WindowsTurkish",
    "windows-1255" -> "MacintoshHebrew",
    "windows-1256" -> "MacintoshArabic",
    "windows-1257" -> "WindowsBaltic",
    "windows-874" -> "WindowsThai",
    "us-ascii" -> "ASCII",
    "ascii" -> "PrintableASCII",
    "cp850" -> "IBM-850",
    "cp437" -> "PrintableASCII",
    "cp936" -> "CP936",
    "cp949" -> "CP949",
    "cp950" -> "CP950",
    "koi8-r" -> "koi8-r",
    "euc-jp" -> "EUC-JP",
    "euc-kr" -> "EUC",
    "shift_jis" -> "ShiftJIS",
    "macroman" -> "MacintoshRoman",
    "big5" -> "MacintoshChineseTraditional",
    "gb2312" -> "MacintoshChineseSimplified"
|>;


getContentLength[data_] :=
Which[
    AssociationQ[data] && KeyExistsQ[data, "Body"],
        If[ByteArrayQ[data["Body"]],
            Length[data["Body"]],
        (*Else*)
            StringLength[data["Body"]]
        ],

    StringQ[data],
        StringLength[data],
    ByteArrayQ[data],
        Length[data]
];


createResponse[assoc_Association, serializer_, defaultSerializer_] :=
Module[{data, body, headers, metadata},
    data = ConditionApply[serializer, defaultSerializer][assoc];

    metadata = <|
        "ContentType" -> "text/html; charset=utf-8",
        "Headers" -> <|
            "Content-Length" -> getContentLength[data]
        |>,
        "StatusCode" -> 200
    |>;

    If[AssociationQ[data],
        If[KeyExistsQ[data, "ContentType"], metadata["ContentType"] = data["ContentType"]];
        If[KeyExistsQ[data, "Headers"], metadata["Headers"] = data["Headers"] ~ Join ~ metadata["Headers"]];
        If[KeyExistsQ[data, "StatusCode"], metadata["StatusCode"] = data["StatusCode"]];
        If[KeyExistsQ[data, "Body"], body = data["Body"]];
    ];

    If[StringQ[data] || ByteArrayQ[data],
        body = data
    ];

    (*Return: HTTPResponse[]*)
    HTTPResponse[body, metadata]
];


(* ::Section::Closed:: *)
(*Serialization*)


$deserializer[request_Association?AssociationQ] :=
request["Body"];


$serializer[expr_] :=
ExportString[expr, "ExpressionJSON"];


$serializer[assoc_Association] :=
ExportString[assoc, "RawJSON"];


$serializer[list_List] :=
ExportString[list, "RawJSON"];


$serializer[image_Image] :=
ExportString[image, "PNG"];


$serializer[image_Graphics] :=
ExportString[image, "SVG"];


$serializer[text_String] :=
text;

$serializer[bytes_ByteArray] :=
bytes;


(* ::Section::Closed:: *)
(*End private context*)


End[(*`Private`*)];


(* ::Section::Closed:: *)
(*End packet*)


EndPackage[(*Kirill`HTTP`*)];