(* ::Package:: *)

(* ::Chapter:: *)
(*Extensions*)


BeginPackage["WLJS`HTTP`Extensions`", {
    "WLJS`Internal`",
    "WLJS`HTTP`Handler`"
}];


AddHTTPHandler::usage =
"AddHTTPHandler[tcp, http] adds HTTP Handler to TCP Server.";


HypertextProcess::usage =
"HypertextProcess[request] import file as WSP and process it.";


GetFileRequestQ::usage =
"GetFileRequestQ[fileType] returns function for checking that is a Get request and path contatins file.";


URLPathToFileName::usage =
"URLPathToFileName[request] to file path.";


FileNameToURLPath::usage =
"FileNameToURLPath[file] to url.";


ImportFile::usage =
"ImportFileAsText[request] import file from request as text data.";


GetMIMEType::usage =
"GetMIMEType[request] returns mime type based on a file extension.";


GetPOSTRequestQ::usage =
"GetPOSTRequestQ[fileType] returns function for checking that is a Post request and path contatins file.";


ProcessMultipart::usage =
"ProcessMultipart[request] returns processed request with a POST data decrypted.";


SetMIMETable::usage =
"SetMIMETable[table_Association] sets a custom MIME types table"

Begin["`Private`"];


AddHTTPHandler[tcp_TCPServer, key_String: "HTTP", http_HTTPHandler] := (
    tcp["CompleteHandler", key] = HTTPPacketQ -> HTTPPacketLength;
    tcp["MessageHandler", key] = HTTPPacketQ -> http;
);


GetFileRequestQ[fileType: _String | {__String} | _StringExpression] :=
AssocMatchQ[<|"Method" -> "GET", "Path" -> "/" ~~ ___ ~~ "." ~~ fileType|>];

GetPOSTRequestQ[fileType: _String | {__String} | _StringExpression] :=
AssocMatchQ[<|"Method" -> "POST", "Path" -> "/" ~~ ___ ~~ "." ~~ fileType|>];

URLPathToFileName[urlPath_String] :=
FileNameJoin[FileNameSplit[StringTrim[urlPath // URLDecode, "/"]]];


URLPathToFileName[request_Association] :=
URLPathToFileName[request["Path"]];


FileNameToURLPath[fileName_String] :=
URLBuild[FileNameSplit[StringTrim[fileName, StartOfString ~~ Directory[]]]];


Options[ImportFile] = {"Base" :> {Directory[]}, "StringOutput"->False}
Options[importFile] = Options[ImportFile]


importFile[path_String, opts:OptionsPattern[]] := With[{body = ReadByteArray[path]},
      If[!OptionValue["StringOutput"],
        <|
            "Body" -> body,
            "Code" -> 200,
            "Headers" -> <|
                "Content-Type" -> GetMIMEType[path],
                "Content-Length" -> Length[body],
                "Connection"-> "Keep-Alive",
                "Keep-Alive" -> "timeout=5, max=1000",
                "Cache-Control" -> "max-age=60480"
            |>
        |>
      ,
        body // ByteArrayToString
      ]
    ]

ImportFile[file_String, opts:OptionsPattern[]] := Module[{paths = Flatten[OptionValue["Base"]]},
With[{path = FileNameJoin[{#, file}]},
    If[FileExistsQ[path],
        Return[importFile[path, opts], Module]
    ]]& /@ paths;

    <|"Code" -> 404|>
];


ImportFile[request_Association, opts: OptionsPattern[]] :=
ImportFile[URLPathToFileName[request["Path"]], opts]


ImportFileAsText = ImportFile


GetMIMEType[file_String] := Module[{type},
    type = $mimeTypes[file // FileExtension];
	If[!StringQ[type], type = "application/octet-stream"];
    type
];


$directory =
DirectoryName[$InputFileName];


$mimeTypes =
Get[FileNameJoin[{$directory, "MIMETypes.wl"}]];

SetMIMETable[assoc_Association] := $mimeTypes = assoc

GetMIMEType[request_Association] :=
GetMIMEType[URLPathToFileName[request["Path"]]];


$DeserializeUrlencoded[request_Association, body_ByteArray] := (
    <|<|URLParse["/?" <> (body//ByteArrayToString)]|>["Query"]|>
);


DecryptField[line_String] :=
 Module[{stream = StringToStream[line], header, body},
  (* Convert a large string to a stream due to performance isssues *)

    With[{extracted = StringExtract[
      (* Read only the header *)
      ReadString[stream, "\r\n\r\n"],
       "\r\n\r\n" -> 1, "\r\n" -> 2 ;;]},

   (* Extract parameters *)

   header = (Association[
      Map[
        Rule[#1, StringRiffle[{##2}, ":"]] & @@
          Map[StringTrim]@StringSplit[#, ":"] &]@extracted]);

   header["Content-Disposition"] =
    Join @@ (Map[
        StringCases[#,
          RegularExpression["(\\w*)=\"(.*)\""] :> <|
            "$1" -> "$2"|>] &, (StringTrim /@
          StringSplit[header["Content-Disposition"], ";"][[2 ;;]])] //
        Flatten);

   (* Skip gaps *)
   ReadString[stream, "\n"];
   ReadString[stream, "\n"];

   (* The rest is our payload *)

   body = ReadString[stream, EndOfFile];

   (* remove \r\n in the beginng and in the end *)
   body = StringDrop[StringDrop[body, -4], 1];

   Close[stream];

   <|"Body" -> body, "Headers" -> header|>
]];


$DeserializeMultipart[request_Association, body_ByteArray] := Module[{boundary, stream, buffer, field, data},
    boundary = StringCases[request["Headers"]["Content-Type"], RegularExpression["boundary=(.*)"] :> "$1"] // First;

    stream = StringToStream[body//ByteArrayToString];
    buffer = {};

    field = ReadString[stream, boundary];

    While[StringQ[field = ReadString[stream, boundary]],
        If[StringTake[field, 2] === "--", Break[]];
        buffer = {buffer, field // DecryptField};
    ];

    buffer = buffer // Flatten;

    (* Association, where all fields will be stored *)
    data = <||>;

    Function[assoc,

        If[KeyExistsQ[assoc["Headers"], "Content-Type"],
        (* 90% probabillity that this is a file *)

            If[!KeyExistsQ[data, assoc["Headers", "Content-Disposition", "name"]],
                data[assoc["Headers", "Content-Disposition", "name"]] = <||>
            ];

            (* name -> filename -> data *)
            data[assoc["Headers", "Content-Disposition", "name"]][assoc["Headers", "Content-Disposition", "filename"]] = assoc["Body"];
        ,
        (* 90% probabillity that this is a regualar field *)

            (* name -> value *)
            data[assoc["Headers", "Content-Disposition", "name"]] = assoc["Body"]
        ]
    ] /@ buffer;

    data
];


ProcessMultipart[request_Association, OptionsPattern[]] := Module[{},
    (* Return request_Association *)

    If[StringMatchQ[request["Headers"]["Content-Type"], "application/x-www-form-urlencoded" ~~ ___],
        Return[Join[request, <|"Data"->$DeserializeUrlencoded[request, request["Body"]], "Body"->{}|>]]
    ];

    If[StringMatchQ[request["Headers"]["Content-Type"], "multipart/form-data" ~~ ___],
        Return[Join[request, <|"Data"->$DeserializeMultipart[request, request["Body"]], "Body"->{}|>]]
    ];

];


Options[HypertextProcess] = {
    "Base" :> {Directory[]}
};


HypertextProcess[request_Association, OptionsPattern[]] := Module[{body},
With[{file = URLPathToFileName[request]},
    Block[{Global`$CurrentRequest = request},

        body = LoadPage[file, {}, "Base" -> First@Flatten@{OptionValue["Base"]}];

        (* handle special case for redirect *)

        If[KeyExistsQ[Global`$CurrentRequest, "Redirect"],
            Return[<|  "Code"->201, "Body"->body,
                "Headers"-> <|"Content-Location" -> Global`$CurrentRequest["Redirect"], "Content-Length" -> StringLength[body]|>
            |>]
        ];

        body
    ]
]]


HypertextProcess[request_Association, filename_String, OptionsPattern[]] := Module[{body},
With[{file = filename},
    Block[{Global`$CurrentRequest = <||>},
        Global`$CurrentRequest = request;
        body = LoadPage[file, {}, "Base"->First@Flatten@{OptionValue["Base"]}];

        (* handle special case for redirect *)

        If[KeyExistsQ[Global`$CurrentRequest, "Redirect"],
            Return[<|  "Code"->201, "Body"->body,
                "Headers"-> <|"Content-Location" -> Global`$CurrentRequest["Redirect"], "Content-Length" -> StringLength[body]|>
            |>]
        ];

        body
    ]
]]


End[];


EndPackage[];
