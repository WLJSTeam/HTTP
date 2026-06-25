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


HTTPGETFileQ::usage =
"HTTPGETFileQ[{ext}][request] check is /path/to/file.ext";


HTTPGETFile::usage =
"HTTPGETFile[request] return HTTPResponse with the file.";


(* ::Section::Closed:: *)
(*Begin private context*)


Begin["`Private`"];


(* ::Section::Closed:: *)
(*HTTPPacketQ*)


HTTPPacketQ[___] := False;


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

        Echo[request, "REQUEST: "];

        (*Result: _String | Association[] | ByteArray[] *)
        result = conditionApply[messageHandler, defaultMessageHandler][request];

        Echo[result, "RESULT: "];

        (*Result: HTTPResponse[]*)
        response = createResponse[result, serializer, defaultSerializer];

        Echo[response, "RESPONSE: "];

        (*Return*)
        ExportByteArray[response, "HTTPResponse"]
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
    request
];


HTTPGETFileQ[request_Association, extensions: {__String}] :=
With[{httpMethod = request["Method"], path = request["Path"]},
    httpMethod === "GET" &&
    StringMatchQ[path, __ ~~ "." ~~ extensions, IgnoreCase -> True]
];


HTTPGETFile[request_Association] :=
With[{path = urlPathToFilePath[request["Path"]]},
    <|
        "Body" -> ReadByteArray[path],
        "ContentType" -> (ToLowerCase[FileExtension[path]] /. $MIMETypes)
    |>
];


urlPathToFilePath[path_String] :=
FileNameJoin[StringSplit[StringTrim[path, "/"], "/"]];


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
If[Length[#] > 0, #[[1]], "utf-8"]& @
KeySelect[request["Headers"], StringMatchQ[#, "content-type", IgnoreCase -> True]&];


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
Module[{data, body, metadata},
    data = conditionApply[serializer, defaultSerializer][assoc["Body"]];

    metadata = <|
        "ContentType" -> If[KeyExistsQ[assoc, "ContentType"], assoc["ContentType"], "text/html; charset=utf-8"],
        "Headers" -> Join[<|
            "Content-Length" -> getContentLength[data]
        |>, If[KeyExistsQ[assoc, "Headers"], assoc["Headers"], <||>]],
        "StatusCode" -> If[KeyExistsQ[assoc, "StatusCode"], assoc["StatusCode"], 200]
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


$MIMETypes = <|
    "ai" -> "application/postscript",
    "aif" -> "audio/x-aiff",
    "aifc" -> "audio/x-aiff",
    "aiff" -> "audio/x-aiff",
    "asc" -> "text/plain",
    "asf" -> "video/x-ms-asf",
    "asp" -> "text/asp",
    "asx" -> "video/x-ms-asf",
    "au" -> "audio/basic",
    "avi" -> "video/avi",
    "bmp" -> "image/bmp",
    "bsp" -> "text/html",
    "btf" -> "image/prs.btif",
    "btif" -> "image/prs.btif",
    "c" -> "text/plain",
    "cc" -> "text/plain",
    "cgm" -> "image/cgm",
    "cpp" -> "text/plain",
    "css" -> "text/css",
    "dcr" -> "application/x-director",
    "der" -> "application/x-x509-ca-cert",
    "doc" -> "application/msword",
    "docm" -> "application/vnd.ms-word.document.macroenabled.12",
    "docx" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    "dot" -> "application/msword",
    "dotm" -> "application/vnd.ms-word.template.macroenabled.12",
    "dotx" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.template",
    "dtd" -> "text/xml",
    "dvi" -> "application/x-dvi",
    "eps" -> "application/postscript",
    "fpx" -> "image/vnd.fpx",
    "gif" -> "image/gif",
    "gz" -> "application/x-gzip",
    "h" -> "text/plain",
    "hh" -> "text/plain",
    "hlp" -> "application/winhelp",
    "hpp" -> "text/plain",
    "htm" -> "text/html",
    "html" -> "text/html",
    "ico" -> "image/ico",
    "ics" -> "text/calendar",
    "ief" -> "image/ief",
    "iges" -> "model/iges",
    "igs" -> "model/iges",
    "ini" -> "text/plain",
    "jar" -> "application/java-archive",
    "jpe" -> "image/jpeg",
    "jpeg" -> "image/jpeg",
    "jpg" -> "image/jpeg",
    "js" -> "application/x-javascript",
    "jsp" -> "text/html",
    "latex" -> "application/x-latex",
    "mesh" -> "model/mesh",
    "mid" -> "audio/mid",
    "midi" -> "audio/mid",
    "mif" -> "application/mif",
    "mov" -> "video/quicktime",
    "mp3" -> "audio/mpeg",
    "mpe" -> "video/mpeg",
    "mpeg" -> "video/mpeg",
    "mpf" -> "text/vnd.ms-mediapackage",
    "mpg" -> "video/mpeg",
    "mpp" -> "application/vnd.ms-project",
    "mpx" -> "application/vnd.ms-project",
    "msh" -> "model/mesh",
    "oda" -> "application/oda",
    "p7m" -> "application/pkcs7-mime",
    "p7s" -> "application/pkcs7-signature",
    "pdf" -> "application/pdf",
    "pl" -> "application/x-perl",
    "png" -> "image/png",
    "potm" -> "application/vnd.ms-powerpoint.template.macroenabled.12",
    "potx" -> "application/vnd.openxmlformats-officedocument.presentationml.template",
    "ppa" -> "application/vnd.ms-powerpoint",
    "ppam" -> "application/vnd.ms-powerpoint.addin.macroenabled.12",
    "pps" -> "application/vnd.ms-powerpoint",
    "ppsm" -> "application/vnd.ms-powerpoint.slideshow.macroenabled.12",
    "ppsx" -> "application/vnd.openxmlformats-officedocument.presentationml.slideshow",
    "ppt" -> "application/vnd.ms-powerpoint",
    "pptm" -> "application/vnd.ms-powerpoint.presentation.macroenabled.12",
    "pptx" -> "application/vnd.openxmlformats-officedocument.presentationml.presentation",
    "ppz" -> "application/vnd.ms-powerpoint",
    "ps" -> "application/postscript",
    "qt" -> "video/quicktime",
    "ra" -> "audio/x-pn-realaudio",
    "ram" -> "audio/x-pn-realaudio",
    "rgb" -> "image/x-rgb",
    "rm" -> "audio/x-pn-realaudio",
    "rtf" -> "application/rtf",
    "rtx" -> "text/richtext",
    "sap" -> "application/x-sapshortcut",
    "scm" -> "application/x-screencam",
    "silo" -> "model/mesh",
    "sim" -> "application/vnd.sap_kw.itutor",
    "sit" -> "application/x-stuffit",
    "sl" -> "text/vnd.wap.sl",
    "snd" -> "audio/basic",
    "spl" -> "application/x-futuresplash",
    "svg" -> "image/svg+xml",
    "swa" -> "application/x-director",
    "swf" -> "application/x-shockwave-flash",
    "tar" -> "application/x-tar",
    "tex" -> "application/x-tex",
    "tht" -> "text/thtml",
    "thtm" -> "text/thtml",
    "thtml" -> "text/thtml",
    "tif" -> "image/tiff",
    "tiff" -> "image/tiff",
    "tsf" -> "application/vnd.ms-excel",
    "txt" -> "text/plain",
    "vcf" -> "text/x-vcard",
    "vcs" -> "text/x-vcalendar",
    "vdo" -> "video/vdo",
    "viv" -> "video/vnd.vivo",
    "vrml" -> "model/vrml",
    "vsd" -> "application/vnd.visio",
    "wav" -> "audio/x-wav",
    "wbmp" -> "text/vnd.wap.wbmp",
    "wmf" -> "application/x-msmetafile",
    "wml" -> "text/vnd.wap.wml",
    "wmls" -> "text/vnd.wap.wmlscript",
    "wp5" -> "application/wordperfect5.1",
    "wrl" -> "model/vrml",
    "xap" -> "application/x-silverlight-app",
    "xbm" -> "image/x-xbitmap",
    "xif" -> "image/vnd.xiff",
    "xlam" -> "application/vnd.ms-excel.addin.macroenabled.12",
    "xls" -> "application/vnd.ms-excel",
    "xlsb" -> "application/vnd.ms-excel.sheet.binary.macroenabled.12",
    "xlsm" -> "application/vnd.ms-excel.sheet.macroenabled.12",
    "xlsx" -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "xltm" -> "application/vnd.ms-excel.template.macroenabled.12",
    "xltx" -> "application/vnd.openxmlformats-officedocument.spreadsheetml.template",
    "xml" -> "text/xml",
    "xsd" -> "text/xml",
    "xsl" -> "text/xml",
    "zip" -> "application/x-zip-compressed",
    "wsp" -> "text/html"
|>;


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


(* ::Section::Closed:: *)
(*End private context*)


End[(*`Private`*)];


(* ::Section::Closed:: *)
(*End packet*)


EndPackage[(*Kirill`HTTP`*)];