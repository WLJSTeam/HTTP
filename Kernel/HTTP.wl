(* ::Package:: *)

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


BeginPackage["WLJS`HTTP`", {
    "WLJS`Objects`",
    "WLJS`Internal`Console`"
}];


ClearAll["`*"];


HTTPPacketQ::usage =
"HTTPPacketQ[packet] check that message was sent via HTTP protocol.";


HTTPPacketLength::usage =
"HTTPPacketLength[packet] returns expected message length.";


HTTPHandler::usage =
"HTTPHandler[opts] mutable type for the handling HTTP request.";


HTTPGETFileQ::usage =
"HTTPGETFileQ[request, {ext}] check is /path/to/file.ext";


HTTPGETFile::usage =
"HTTPGETFile[request] return HTTPResponse with the file.";


HTTPRequestMatchQ::usage =
"HTTPRequestMatchQ[request, requestPattern]";


HTTPGETQ::usage =
"HTTPGETQ[request, pathPattern]";


HTTPRegisterMimeType::usage =
"HTTPRegisterMimeType[type, mime]";


Begin["`Private`"];


HTTPPacketQ[___] := False;


HTTPPacketQ[packet_Association?AssociationQ] /; KeyExistsQ[packet, "DataByteArray"] :=
With[{dataByteArray = packet["DataByteArray"]},
    byteArrayContainsQ[dataByteArray, $httpEndOfHead] &&
    byteArrayStringMatchQ[dataByteArray, StartOfString ~~ $httpMethods ~~ " /" ~~ __]
];


HTTPPacketLength[packet_Association] :=
With[{dataByteArray = packet["DataByteArray"]},
    Module[{head},
        head = ByteArrayToString[byteArrayExtract[dataByteArray, $httpEndOfHead -> 1]];

        (*Return: _Integer*)
        Which[
            StringContainsQ[head, "Content-Length: ", IgnoreCase -> True],
                StringLength[head] + 4 +
                ToExpression[StringTrim[StringExtract[ToLowerCase[head], "content-length: " -> 2, $httpEndOfHeader -> 1]]],
            True,
                Length[dataByteArray]
        ]
    ]
];


CreateType[HTTPHandler, {
    "MessageHandler" -> <||>,
    "DefaultMessageHandler" -> Function[<|"Code" -> 404, "Body" -> "NotFound"|>],
    "Deserializer" -> <||>,
    "DefaultDeserializer" -> deserializeRequestBody,
    "Serializer" -> <||>,
    "DefaultSerializer" -> $serializer,
    "Logger" -> None,
    "Icon" -> Import[FileNameJoin[Join[FileNameSplit[$InputFileName][[ ;; -3]], {"Images", "http-logo.png"}]]]
}];


handler_HTTPHandler[packet_Association] :=
With[{client = packet["SourceSocket"], dataByteArray = packet["DataByteArray"]},
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
        request = parseRequest[client, dataByteArray, deserializer, defaultDeserializer];

        ConsoleEcho["REQUEST"][request];

        (*Result: _String | Association[] | ByteArray[] *)
        result = conditionApply[messageHandler, defaultMessageHandler][request];

        ConsoleEcho["RESULT"][result];

        (*Result: HTTPResponse[]*)
        response = createResponse[result, serializer, defaultSerializer];

        ConsoleEcho["RESPONSE"][response];

        (*Return*)
        ExportByteArray[response, "HTTPResponse"]
    ]
];


HTTPHandler /: AddTo[service_, http_HTTPHandler] :=
With[{$service = service},
    $service["Accumulator"] = If[AssociationQ[$service["Accumulator"]],
        Join[$service["Accumulator"], <|"HTTP" -> HTTPPacketQ -> HTTPPacketLength|>],
    (*Else*)
        <|
            "HTTP" -> HTTPPacketQ -> HTTPPacketLength,
            "" -> Function[True] -> $service["Accumulator"]
        |>
    ];

    If[KeyExistsQ[$service["Accumulator"], ""],
        $service["Accumulator"] = Append[$service["Accumulator"], "" -> $service["Accumulator", ""]]
    ];

    $service["Received"] = If[AssociationQ[$service["Received"]],
        Join[$service["Received"], <|"HTTP" -> HTTPPacketQ -> http|>],
    (*Else*)
        <|
            "HTTP" -> HTTPPacketQ -> http,
            "" -> Function[True] -> $service["Received"]
        |>
    ];

    If[KeyExistsQ[$service["Received"], ""],
        $service["Received"] = Append[$service["Received"], "" -> $service["Received", ""]]
    ];

    $service
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


HTTPRequestMatchQ[request_?AssociationQ, requestPattern_?AssociationQ] :=
With[{
    $requestPattern =<|KeyValueMap[ToLowerCase[#] -> #2&, requestPattern]|>,
    $request = <|KeyValueMap[ToLowerCase[#] -> #2&, request]|>
},
    SubsetQ[Keys[$request], Keys[$requestPattern]] &&
    And @@ Table[
        Which[
            StringQ[$request[k]], StringMatchQ[$request[k], $requestPattern[k]],
            AssociationQ[$request[k]], HTTPRequestMatchQ[$request[k], $requestPattern[k]],
            True, MatchQ[$request[k], $requestPattern[k]]
        ],
        {k, Keys[$requestPattern]}
    ]
];


HTTPRegisterMimeType[type_String, mime_String] :=
$MIMETypes[type] = mime;


$httpMethods = {"GET", "PUT", "DELETE", "HEAD", "POST", "CONNECT", "OPTIONS", "TRACE", "PATCH"};


$httpEndOfHead = {"\r\n\r\n", "\n\n"};


$httpEndOfHeader = {"\r\n", "\n"};


$errorResponse = <|"Code" -> 404, "Body" -> "Not found"|>;


parseRequest[client_, dataByteArray_ByteArray, deserializer_, defaultDeserializer_] :=
Module[{request, head, headLength, bodyPosition, bodyByteArray,
    headline, method, url, version, headers, body, encoding},

    request = <|
        "Client" -> client,
        "DataByteArray" -> dataByteArray,
        "Method" -> Null,
        "Path" -> Null,
        "Query" -> <||>,
        "Version" -> Null,
        "Headers" -> <||>,
        "ContentType" -> Null,
        "ContentEncoding" -> Null,
        "ContentCharset" -> Null,
        "TransferEncoding" -> Null,
        "BodyByteArray" :> Null,
        "BodyBytes" :> Null,
        "Body" :> Null,
        "Content" :> Null
    |>;

    head = byteArrayExtractString[dataByteArray, $httpEndOfHead -> 1];
    headLength = StringLength[head];

    headline = StringExtract[head, $httpEndOfHeader -> 1];
    headers = Map[StringTrim] @ StringExtract[head, $httpEndOfHeader -> 2 ;; ];

    bodyPosition = If[dataByteArray[[headLength + 1]] == 13, headLength + 5, headLength + 3];
    bodyByteArray = dataByteArray[[bodyPosition ;; ]];

    {method, url, version} = First @ StringCases[headline,
        $method__ ~~ " " ~~ $url__ ~~ " " ~~ $version__ :> {$method, URLParse[$url], $version}
    ];

    request["Method"] = method;
    request["Version"] = version;
    request["Path"] = StringRiffle[url["Path"], "/"];
    request["Query"] = Map[If[Length[#] > 1, #[[All, -1]], #[[1, -1]]] &] @ GroupBy[First] @ url["Query"];

    request["Headers"] =
        Association @
        Map[StringTrim[#[[1]]] -> StringTrim[StringRiffle[#[[2 ;; ]], ":"]]&] @
        Map[StringSplit[#, ":" ]&] @
        headers;

    request["ContentEncoding"] =
        If[Length[#] > 0, #[[1]], Null]& @
        KeySelect[StringMatchQ[#, "content-encoding", IgnoreCase -> True]&] @
        request["Headers"];

    request["ContentType"] :=
        If[Length[#] > 0, StringSplit[#[[1]], ";"][[1]], Null]& @
        KeySelect[StringMatchQ[#, "content-type", IgnoreCase -> True]&] @
        request["Headers"];

    request["ContentCharset"] =
        If[# === Null || !StringContainsQ[#, "charset=", IgnoreCase -> True], Null,
            StringTrim @ StringExtract[#, "charset=" -> 2, ";" -> 1]
        ]& @
        If[Length[#] > 0, #[[1]], Null]& @
        KeySelect[StringMatchQ[#, "content-type", IgnoreCase -> True]&] @
        request["Headers"];

    With[{
        $bodyByteArray = decodeBody[bodyByteArray, request["ContentEncoding"]],
        $contentCharset = request["ContentCharset"]
    },
        request["BodyByteArray"] := $bodyByteArray;
        request["BodyBytes"] := Normal[$bodyByteArray];
        request["Body"] := ByteArrayToString[$bodyByteArray, $contentCharset];
    ];

    With[{$content = conditionApply[deserializer, defaultDeserializer][request]},
        request["Content"] := $content;
    ];

    request
];


urlPathToFilePath[path_String] :=
FileNameJoin[StringSplit[StringTrim[path, "/"], "/"]];


byteArrayContainsQ[byteArray_ByteArray, substring_] :=
StringContainsQ[ByteArrayToString[byteArray, "ISOLatin1"], substring];


byteArrayContainsQ[byteArray_ByteArray, subbyteArray_ByteArray] :=
byteArrayContainsQ[byteArray, ByteArrayToString[subbyteArray, "ISOLatin1"]];


byteArrayExtract[dataByteArray_ByteArray, separator_ -> n_Integer] :=
With[{
    data = ByteArrayToString[dataByteArray, "ISOLatin1"]
},
    StringToByteArray[StringExtract[data, separator -> n]]
];


byteArrayExtractString[dataByteArray_ByteArray, separator_ -> n_Integer] :=
With[{
    data = ByteArrayToString[dataByteArray, "ISOLatin1"]
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


decodeBody[body_, Null] :=
body;


decodeBody[body_, contentEncoding_] :=
Which[
    StringQ[body] && StringMatchQ[contentEncoding, "gzip", IgnoreCase -> True],
        ByteArrayToString[ImportByteArray[StringToByteArray[body], "GZIP"], "UTF-8"],
    ByteArrayQ[body] && StringMatchQ[contentEncoding, "gzip", IgnoreCase -> True],
        ImportByteArray[body, "GZIP"],
    True,
        body
];


deserializeRequestBody[request_Association?AssociationQ] :=
With[{
    contentEncoding = request["ContentEncoding"],
    contentType = request["ContentType"],
    bodyByteArray = request["BodyByteArray"],
    body = request["Body"]
},
    If[Length[bodyByteArray] == 0 || StringLength[body] == 0,
        Null,
    (*Else*)
        With[{
            $bodyByteArray = decodeBody[bodyByteArray, contentEncoding],
            $body = decodeBody[body, contentEncoding]
        },
            Switch[contentType,
                "application/json", ImportString[$bodyString, "RawJSON"],
                "application/x-www-form-urlencoded", Association @ URLQueryDecode[$bodyString]
            ]
        ]
    ];
];


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
    "htm" -> "text/html; charset=utf-8",
    "html" -> "text/html; charset=utf-8",
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


End[(*`Private`*)];


EndPackage[(*Kirill`HTTP`*)];