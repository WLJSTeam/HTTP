(* ::Package:: *)

(* HTTPHandler transforms a complete request packet into response bytes.
    The transport owns reads, buffering, Result assignment and writes. *)


(* ::Program:: *)


(*+-----------------------------------------------+*)
(*|                HTTP HANDLER                   |*)
(*|                                               |*)
(*|          (request packet: Association)        |*)
(*|                      |                        |*)
(*|           [parse request to assoc]            |*)
(*|                      |                        |*)
(*|            [deserialize content]              |*)
(*|                      |                        |*)
(*|              <select pipeline>                |*)
(*|     /       /        |        \         \     |*)
(*|    ..   [get..]  [post..]  [delete..]   ..    |*)
(*|             \        |        /               |*)
(*|             [serialize response]              |*)
(*|                      |                        |*)
(*|             {return ByteArray}                |*)
(*|                      |                        |*)
(*|       CSocketHandler: Result -> send          |*)
(*+-----------------------------------------------+*)


BeginPackage["WLJS`HTTP`",
    {
        "WLJS`Objects`"
    }
];


ClearAll["`*"];


HTTPPacketQ::usage =
"HTTPPacketQ[packet] check that message was sent via HTTP protocol.";


HTTPPacketLength::usage =
"HTTPPacketLength[packet] returns expected message length.";


HTTPHandler::usage =
"HTTPHandler[opts] creates a configurable request processor. handler[packet] parses packet[\"DataByteArray\"] and returns HTTP response bytes without socket I/O.";


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


HTTPSerialize::usage =
"HTTPSerialize[body, contentType: Automatic] returns encoded Body bytes and ContentType.";


HTTPDeserialize::usage =
"HTTPDeserialize[bytes, contentType] decodes HTTP content. HTTPDeserialize[request] decodes a parsed request body.";


Begin["`Private`"];


HTTPPacketQ[___] :=
False;


HTTPPacketQ[packet_Association?AssociationQ] /; KeyExistsQ[packet, "DataByteArray"] :=
With[{dataByteArray = packet["DataByteArray"]},
    byteArrayContainsQ[dataByteArray, $httpEndOfHead] && byteArrayStringMatchQ[dataByteArray, StartOfString ~~ $httpMethods ~~ " /" ~~ __]
];


HTTPPacketLength[packet_Association] :=
Module[{data = ByteArrayToString[packet["DataByteArray"], "ISOLatin1"], position, head, lengths},
    position = StringPosition[data, $httpEndOfHead, 1];
    If[position === {}, Return[$Failed]];
    head = StringTake[data, position[[1, 1]] - 1];
    lengths = StringCases[
        head,
        RegularExpression["(?im)^content-length:[ \t]*([0-9]+)[ \t]*\r?$"] :> "$1"
    ];
    position[[1, 2]] + If[lengths === {}, 0, FromDigits[First[lengths]]]
];


CreateType[
    HTTPHandler,
    {
        "MessageHandler" -> <||>,
        "DefaultMessageHandler" -> Function[<|"StatusCode" -> 404, "Body" -> "Not found"|>],
        "Deserializer" -> <||>,
        "DefaultDeserializer" -> HTTPDeserialize,
        "Serializer" -> <||>,
        "DefaultSerializer" -> HTTPSerialize,
        "Icon" -> Import[FileNameJoin[Join[FileNameSplit[$InputFileName][[;; - 3]], {"Images", "http-logo.png"}]]]
    }
];


handler_HTTPHandler[packet_Association] :=
Module[{request, result, response},
    If[!ByteArrayQ[Lookup[packet, "DataByteArray", Null]] || !HTTPPacketQ[packet],
        Message[HTTPHandler::packet];
        Return[$Failed]
    ];
    request = parseRequest[packet, handler["Deserializer"], handler["DefaultDeserializer"]];
    If[request === $Failed, Return[$Failed]];
    result = conditionApply[handler["MessageHandler"], handler["DefaultMessageHandler"]][request];
    response = createResponse[result, handler["Serializer"], handler["DefaultSerializer"]];
    If[response === $Failed, $Failed, ExportByteArray[response, "HTTPResponse"]]
];


HTTPHandler::packet =
"Expected an association containing a complete HTTP request in DataByteArray.";


HTTPGETFileQ[request_Association, extensions: {__String}] :=
With[{httpMethod = request["Method"], path = request["Path"]},
    httpMethod === "GET" && StringMatchQ[path, __ ~~ "." ~~ extensions, IgnoreCase -> True]
];


HTTPGETFile[request_Association] :=
With[{path = urlPathToFilePath[request["Path"]]},
    <|
        "Body" -> File[path]
    |>
];


HTTPRequestMatchQ[request_?AssociationQ, requestPattern_?AssociationQ] :=
With[{
    $requestPattern = <|KeyValueMap[ToLowerCase[#] -> #2&, requestPattern]|>,
    $request = <|KeyValueMap[ToLowerCase[#] -> #2&, request]|>
},
    SubsetQ[Keys[$request], Keys[$requestPattern]] && And @@ Table[
        Which[
            StringQ[$request[k]],
            StringMatchQ[$request[k], $requestPattern[k]],
            AssociationQ[$request[k]],
            HTTPRequestMatchQ[$request[k], $requestPattern[k]],
            True,
            MatchQ[$request[k], $requestPattern[k]]
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


parseRequest[packet_Association, deserializer_, defaultDeserializer_] :=
Module[{
    dataByteArray = packet["DataByteArray"],
    request,
    head,
    headLength,
    bodyPosition,
    bodyByteArray,
    headline,
    method,
    url,
    version,
    headers
},
    request = Join[
        packet,
        <|
            "Method" -> Null,
            "Path" -> Null,
            "Query" -> <||>,
            "Version" -> Null,
            "Headers" -> <||>,
            "ContentType" -> <||>,
            "ContentEncoding" -> Null,
            "TransferEncoding" -> Null,
            "BodyByteArray" :> Null,
            "BodyBytes" :> Null,
            "Body" :> Null,
            "Content" :> Null
        |>
    ];
    head = byteArrayExtractString[dataByteArray, $httpEndOfHead -> 1];
    headLength = StringLength[head];
    headline = StringExtract[head, $httpEndOfHeader -> 1];
    headers = Map[StringTrim] @ StringExtract[head, $httpEndOfHeader -> 2 ;;];
    bodyPosition = If[dataByteArray[[headLength + 1]] == 13, headLength + 5, headLength + 3];
    bodyByteArray = dataByteArray[[bodyPosition ;;]];
    {method, url, version} = First @ StringCases[
        headline,
        $method__ ~~ " " ~~ $url__ ~~ " " ~~ $version__ :> {$method, URLParse[$url], $version}
    ];
    request["Method"] = method;
    request["Version"] = version;
    request["Path"] = StringRiffle[url["Path"], "/"];
    request["Query"] = Map[If[Length[#] > 1, #[[All, - 1]], #[[1, - 1]]]&] @ GroupBy[First] @ url["Query"];
    request["Headers"] = Association @ Map[StringTrim[#[[1]]] -> StringTrim[StringRiffle[#[[2 ;;]], ":"]]&] @ Map[StringSplit[#, ":"]&] @ headers;
    request["ContentEncoding"] = getHeaderValue[request["Headers"], "content-encoding"];
    request["TransferEncoding"] = getHeaderValue[request["Headers"], "transfer-encoding"];
    request["ContentType"] = getHeaderValue[request["Headers"], "content-type"];
    request["ContentCharset"] = If[StringQ[request["ContentType"]],
        getCharsetEncoding[request["ContentType"]],
        "UTF-8"
    ];
    With[{
        $bodyByteArray = decodeBody[bodyByteArray, request["ContentEncoding"]],
        $contentCharset = request["ContentCharset"]
    },
        If[$bodyByteArray === $Failed, Return[$Failed]];
        request["BodyByteArray"] := $bodyByteArray;
        request["BodyBytes"] := Normal[$bodyByteArray];
        request["Body"] := ByteArrayToString[$bodyByteArray, $contentCharset];
    ];
    With[{$content = conditionApply[deserializer, defaultDeserializer][request]},
        If[$content === $Failed, Return[$Failed]];
        request["Content"] := $content;
    ];
    request
];


getHeaderValue[headers_Association?AssociationQ, header_String] :=
If[Length[#] > 0, #[[1]], Null]& @ KeySelect[StringMatchQ[#, header, IgnoreCase -> True]&] @ headers;


getHeaderParameter[headers_Association, header_String, parameter_String] :=
With[{value = getHeaderValue[headers, header]},
    If[StringQ[value], Lookup[headerParameters[value], ToLowerCase[parameter], Null], Null]
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
    StringToByteArray[StringExtract[data, separator -> n], "ISOLatin1"]
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
        selected[[- 1]][##]
    ]
];


getCharsetEncoding[contentType_String] :=
With[{charset = getHeaderParameter[<|"Content-Type" -> contentType|>, "content-type", "charset"]},
    If[StringQ[charset], Lookup[$charsetToEncoding, ToLowerCase[charset], "UTF-8"], "UTF-8"]
];


createResponse[$Failed, _, _] :=
$Failed;


createResponse[response_HTTPResponse, _, _] :=
response;


createResponse[assoc_Association, serializer_, defaultSerializer_] :=
Module[{data, body, metadata, headers, type, fallback, encoded, encoding},
    type = Lookup[assoc, "ContentType", getHeaderValue[Lookup[assoc, "Headers", <||>], "content-type"]];
    If[!StringQ[type], type = Automatic];
    fallback = If[defaultSerializer === HTTPSerialize, Function[HTTPSerialize[#, type]], defaultSerializer];
    data = conditionApply[serializer, fallback][Lookup[assoc, "Body", ""]];
    If[data === $Failed, Return[$Failed]];
    metadata = If[AssociationQ[data], KeyDrop[data, "Body"], <||>];
    metadata = Join[metadata, KeyDrop[assoc, {"Body", "Code", "ContentEncoding"}]];
    If[defaultSerializer === HTTPSerialize && AssociationQ[data] && KeyExistsQ[data, "ContentType"],
        metadata["ContentType"] = data["ContentType"]
    ];
    body = If[AssociationQ[data], Lookup[data, "Body", ""], data];
    If[!ByteArrayQ[body],
        encoded = HTTPSerialize[body, Lookup[metadata, "ContentType", type]];
        If[encoded === $Failed, Return[$Failed]];
        body = encoded["Body"];
        metadata["ContentType"] = encoded["ContentType"]
    ];
    headers = Join[
        If[AssociationQ[data], Lookup[data, "Headers", <||>], <||>],
        Lookup[assoc, "Headers", <||>]
    ];
    encoding = Lookup[assoc, "ContentEncoding", getHeaderValue[headers, "content-encoding"]];
    body = encodeBody[body, encoding];
    If[body === $Failed, Return[$Failed]];
    If[StringQ[encoding],
        headers = KeySelect[headers, ToLowerCase[#] =!= "content-encoding"&];
        headers["Content-Encoding"] = encoding
    ];
    headers = KeySelect[headers, !MemberQ[{"content-length", "content-type"}, ToLowerCase[#]]&];
    metadata["Headers"] = Append[headers, "Content-Length" -> Length[body]];
    metadata["ContentType"] = Lookup[metadata, "ContentType", "application/octet-stream"];
    metadata["StatusCode"] = Lookup[metadata, "StatusCode", Lookup[assoc, "Code", 200]];
    HTTPResponse[body, metadata]
];


createResponse[body_, serializer_, defaultSerializer_] :=
createResponse[<|"Body" -> body|>, serializer, defaultSerializer];


HTTPHandler::encoding =
"Unsupported or invalid HTTP Content-Encoding: `1`.";


decodeBody[body_ByteArray, encoding_] :=
transformBodyEncoding[body, encoding, False];


encodeBody[body_ByteArray, encoding_] :=
transformBodyEncoding[body, encoding, True];


transformBodyEncoding[body_ByteArray, encoding_, encode: True | False] :=
Module[{result = body, encodings},
    If[encoding === Null, Return[body]];
    If[!StringQ[encoding], Message[HTTPHandler::encoding, encoding]; Return[$Failed]];
    encodings = ToLowerCase /@ StringTrim /@ StringSplit[encoding, ","];
    If[!encode, encodings = Reverse[encodings]];
    result = Quiet[
        Check[
            Fold[
                Function[{bytes, method},
                    Which[
                        bytes === $Failed,
                        $Failed,
                        method === "identity",
                        bytes,
                        method === "gzip",
                        If[encode,
                            ExportByteArray[Normal[bytes], {"GZIP", "Byte"}],
                            ByteArray[ImportByteArray[bytes, {"GZIP", "Byte"}]]
                        ],
                        True,
                        $Failed
                    ]
                ],
                body,
                encodings
            ],
            $Failed
        ]
    ];
    If[!ByteArrayQ[result], Message[HTTPHandler::encoding, encoding]; Return[$Failed]];
    result
];


$MIMETypes = <|
    "json" -> "application/json",
    "csv" -> "text/csv",
    "tsv" -> "text/tab-separated-values",
    "webp" -> "image/webp",
    "wasm" -> "application/wasm",
    "woff" -> "font/woff",
    "woff2" -> "font/woff2",
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


(* ::Section:: *)


(*Content serialization*)


HTTPSerialize::format =
"Cannot serialize the body as `1`.";


HTTPDeserialize::format =
"Cannot deserialize HTTP content of type `1`.";


(* Parameter separators inside quoted strings are not delimiters. *)


headerSegments[value_String] :=
Module[{parts = {}, start = 1, quoted = False, escaped = False, char},
    Do[
        char = StringTake[value, {i}];
        Which[
            escaped,
            escaped = False,
            quoted && char === "\\",
            escaped = True,
            char === "\"",
            quoted = !quoted,
            char === ";" && !quoted,
            AppendTo[parts, StringTake[value, {start, i - 1}]];
            start = i + 1
        ],
        {i, StringLength[value]}
    ];
    Append[parts, StringDrop[value, start - 1]]
];


headerParameters[value_String] :=
Association @ Cases[
    StringSplit[#, "=", 2]& /@ Rest[headerSegments[value]],
    {key_String, val_String} :> (
        ToLowerCase[StringTrim[key]] -> StringReplace[StringTrim[StringTrim[val], "\""], {"\\\"" -> "\"", "\\\\" -> "\\"}]
    )
];


mimeType[value_String] :=
ToLowerCase[StringTrim[First[headerSegments[value]]]];


mimeType[_] :=
"application/octet-stream";


jsonMimeQ[mime_String] :=
mime === "application/json" || StringEndsQ[mime, "+json"];


xmlMimeQ[mime_String] :=
MemberQ[{"application/xml", "text/xml"}, mime] || StringEndsQ[mime, "+xml"];


$mediaFormats = <|
    "image/png" -> "PNG",
    "image/jpeg" -> "JPEG",
    "image/gif" -> "GIF",
    "image/bmp" -> "BMP",
    "image/tiff" -> "TIFF",
    "image/svg+xml" -> "SVG",
    "text/csv" -> "CSV",
    "text/tab-separated-values" -> "TSV"
|>;


inferContentType[body_] :=
Which[
    MatchQ[body, _File],
    Lookup[$MIMETypes, ToLowerCase[FileExtension[First[body]]], "application/octet-stream"],
    ByteArrayQ[body],
    "application/octet-stream",
    StringQ[body],
    "text/html; charset=utf-8",
    MatchQ[body, _Image],
    "image/png",
    MatchQ[body, _Graphics],
    "image/svg+xml",
    MatchQ[body, _XMLElement | XMLObject[_][___]],
    "application/xml; charset=utf-8",
    AssociationQ[body] || ListQ[body] || NumberQ[body] || MemberQ[{True, False, Null}, body],
    "application/json; charset=utf-8",
    True,
    "application/vnd.wolfram.expression+json"
];


HTTPSerialize[body_, contentType: (_String | Automatic): Automatic] :=
Module[{type = contentType, mime, bytes, result},
    If[type === Automatic, type = inferContentType[body]];
    mime = mimeType[type];
    If[mime === "multipart/form-data" && !ByteArrayQ[body], Return[serializeMultipart[body, type]]];
    bytes = Quiet[
        Check[
            Which[
                MatchQ[body, _File],
                ReadByteArray[First[body]],
                ByteArrayQ[body],
                body,
                mime === "application/x-www-form-urlencoded" && !StringQ[body],
                StringToByteArray[encodeForm[body], getCharsetEncoding[type]],
                mime === "application/vnd.wolfram.expression+json",
                ExportByteArray[body, "ExpressionJSON"],
                jsonMimeQ[mime] && !StringQ[body],
                ExportByteArray[body, "RawJSON"],
                xmlMimeQ[mime] && !StringQ[body] && mime =!= "image/svg+xml",
                StringToByteArray[ExportString[body, "XML"], getCharsetEncoding[type]],
                KeyExistsQ[$mediaFormats, mime] && !StringQ[body],
                If[MemberQ[{"text/csv", "text/tab-separated-values"}, mime],
                    StringToByteArray[ExportString[body, $mediaFormats[mime]], getCharsetEncoding[type]],
                    ExportByteArray[body, $mediaFormats[mime]]
                ],
                StringQ[body],
                StringToByteArray[body, getCharsetEncoding[type]],
                True,
                $Failed
            ],
            $Failed
        ]
    ];
    If[!ByteArrayQ[bytes], Message[HTTPSerialize::format, type]; Return[$Failed]];
    <|"Body" -> bytes, "ContentType" -> type|>
];


(* Lists of rules preserve repeated form keys. List-valued association entries
    expand to repeated keys, matching the result of decodeForm. *)


formRules[body_Association] :=
Flatten[KeyValueMap[Function[{key, value}, Thread[key -> If[ListQ[value], value, {value}]]], body]];


formRules[body_List] :=
body;


encodeForm[body_] :=
URLQueryEncode[formRules[body]];


decodeForm[text_String] :=
Map[
    If[Length[#] === 1, First[#], #]&,
    GroupBy[URLQueryDecode[text], First -> Last]
];


HTTPDeserialize[request_Association] :=
HTTPDeserialize[
    request["BodyByteArray"],
    Replace[
        Lookup[request, "ContentType", getHeaderValue[Lookup[request, "Headers", <||>], "content-type"]],
        Except[_String] -> "application/octet-stream"
    ]
];


HTTPDeserialize[bytes_ByteArray, contentType_String: "application/octet-stream"] :=
Module[{mime = mimeType[contentType], result, text},
    If[Length[bytes] === 0, Return[Null]];
    result = Quiet[
        Check[
            Which[
                mime === "multipart/form-data",
                deserializeMultipart[bytes, contentType],
                mime === "application/vnd.wolfram.expression+json",
                ImportByteArray[bytes, "ExpressionJSON"],
                jsonMimeQ[mime],
                ImportByteArray[
                    StringToByteArray[ByteArrayToString[bytes, getCharsetEncoding[contentType]], "UTF-8"],
                    "RawJSON"
                ],
                mime === "application/x-www-form-urlencoded",
                decodeForm[ByteArrayToString[bytes, getCharsetEncoding[contentType]]],
                KeyExistsQ[$mediaFormats, mime],
                If[MemberQ[{"text/csv", "text/tab-separated-values"}, mime],
                    ImportString[ByteArrayToString[bytes, getCharsetEncoding[contentType]], $mediaFormats[mime]],
                    ImportByteArray[bytes, $mediaFormats[mime]]
                ],
                xmlMimeQ[mime],
                ImportByteArray[bytes, "XML"],
                StringStartsQ[mime, "text/"] || MemberQ[{"application/javascript", "application/x-javascript", "application/ecmascript"}, mime],
                ByteArrayToString[bytes, getCharsetEncoding[contentType]],
                True,
                bytes
            ],
            $Failed
        ]
    ];
    If[result === $Failed, Message[HTTPDeserialize::format, contentType]];
    result
];


quoteParameter[value_String] :=
"\"" <> StringReplace[value, {"\\" -> "\\\\", "\"" -> "\\\"", "\r" -> "", "\n" -> ""}] <> "\"";


multipartParts[fields_Association] :=
Flatten[
    KeyValueMap[
        Function[{name, values},
            Map[
                If[AssociationQ[#], Join[#, <|"Name" -> name|>], <|"Name" -> name, "Body" -> #|>]&,
                If[ListQ[values], values, {values}]
            ]
        ],
        fields
    ]
];


multipartParts[parts_List] :=
parts;


serializeMultipart[fields_, contentType_String] :=
Catch[
    Module[{parts, boundary, requested, encoded = {}, part, data, type, body, headers, name, file, wire},
        parts = multipartParts[fields];
        If[!ListQ[parts] || !AllTrue[parts, AssociationQ[#] && StringQ[Lookup[#, "Name", Null]]&],
            Message[HTTPSerialize::format, contentType];
            Throw[$Failed, $multipartSerialize]
        ];
        requested = Lookup[headerParameters[contentType], "boundary", Automatic];
        Do[
            name = part["Name"];
            body = Lookup[part, "Body", Lookup[part, "Data", ""]];
            file = Lookup[part, "FileName", If[MatchQ[body, _File], FileNameTake[First[body]], Null]];
            type = Lookup[part, "ContentType", If[StringQ[body], "text/plain; charset=utf-8", Automatic]];
            data = HTTPSerialize[body, type];
            If[data === $Failed, Throw[$Failed, $multipartSerialize]];
            headers = Lookup[part, "Headers", <||>];
            headers = KeySelect[headers, !MemberQ[{"content-disposition", "content-type", "content-length"}, ToLowerCase[#]]&];
            headers = Join[
                <|
                    "Content-Disposition" -> (
                        "form-data; name=" <> quoteParameter[name] <> If[StringQ[file], "; filename=" <> quoteParameter[file], ""]
                    ),
                    "Content-Type" -> data["ContentType"]
                |>,
                headers
            ];
            AppendTo[
                encoded,
                Join[
                    StringToByteArray[
                        StringRiffle[KeyValueMap[# <> ": " <> ToString[#2]&, headers], "\r\n"] <> "\r\n\r\n",
                        "UTF-8"
                    ],
                    data["Body"]
                ]
            ],
            {part, parts}
        ];
        boundary = If[requested === Automatic, "WLJS-" <> CreateUUID[], requested];
        If[!StringQ[boundary] || !StringMatchQ[boundary, RegularExpression["[0-9A-Za-z'()+_,./:=? -]{1,70}"]] || StringEndsQ[boundary, " "],
            Message[HTTPSerialize::format, contentType];
            Throw[$Failed, $multipartSerialize]
        ];
        If[AnyTrue[encoded, StringContainsQ[ByteArrayToString[#, "ISOLatin1"], boundary]&],
            If[requested =!= Automatic,
                Message[HTTPSerialize::format, contentType];
                Throw[$Failed, $multipartSerialize]
            ];
            While[AnyTrue[encoded, StringContainsQ[ByteArrayToString[#, "ISOLatin1"], boundary]&],
                boundary = "WLJS-" <> CreateUUID[]
            ]
        ];
        wire = Join[
            If[encoded === {},
                ByteArray[{}],
                Join @@ Map[Join[StringToByteArray["--" <> boundary <> "\r\n"], #, StringToByteArray["\r\n"]]&, encoded]
            ],
            StringToByteArray["--" <> boundary <> "--\r\n"]
        ];
        <|"Body" -> wire, "ContentType" -> ("multipart/form-data; boundary=" <> quoteParameter[boundary])|>
    ],
    $multipartSerialize
];


(* Scan a Latin1 view to keep byte offsets exact, including arbitrary file data.
    Only delimiter lines count; boundary-like bytes inside a file remain data. *)


deserializeMultipart[bytes_ByteArray, contentType_String] :=
Catch[
    Module[{
        boundary,
        data,
        markers,
        parts = {},
        marker,
        next,
        start,
        finish,
        raw,
        split,
        headers,
        disposition,
        type,
        body,
        part
    },
        boundary = Lookup[headerParameters[contentType], "boundary", Null];
        If[!StringQ[boundary] || boundary === "" || StringContainsQ[boundary, {"\r", "\n"}],
            Throw[$Failed, $multipartDeserialize]
        ];
        data = ByteArrayToString[bytes, "ISOLatin1"];
        markers = Select[
            StringPosition[data, "--" <> boundary],
            Function[pos,
                (pos[[1]] === 1 || (pos[[1]] > 2 && StringTake[data, {pos[[1]] - 2, pos[[1]] - 1}] === "\r\n")) && (
                    StringStartsQ[StringDrop[data, pos[[2]]], "\r\n"] || StringDrop[data, pos[[2]]] === "--" || StringStartsQ[StringDrop[data, pos[[2]]], "--\r\n"]
                )
            ]
        ];
        If[markers === {}, Throw[$Failed, $multipartDeserialize]];
        Do[
            marker = markers[[i]];
            If[StringTake[data, {marker[[2]] + 1, Min[marker[[2]] + 2, StringLength[data]]}] === "--",
                Throw[parts, $multipartDeserialize]
            ];
            If[i === Length[markers], Throw[$Failed, $multipartDeserialize]];
            next = markers[[i + 1]];
            start = marker[[2]] + 3;
            finish = next[[1]] - 3;
            raw = Take[bytes, {start, finish}];
            split = StringPosition[ByteArrayToString[raw, "ISOLatin1"], "\r\n\r\n", 1];
            If[split === {}, Throw[$Failed, $multipartDeserialize]];
            headers = Association @ Map[
                Function[line,
                    With[{pair = StringSplit[line, ":", 2]},
                        If[Length[pair] === 2, StringTrim[pair[[1]]] -> StringTrim[pair[[2]]], Nothing]
                    ]
                ],
                StringSplit[ByteArrayToString[Take[raw, split[[1, 1]] - 1], "UTF-8"], "\r\n"]
            ];
            disposition = getHeaderValue[headers, "content-disposition"];
            If[!StringQ[disposition] || mimeType[disposition] =!= "form-data",
                Throw[$Failed, $multipartDeserialize]
            ];
            disposition = headerParameters[disposition];
            If[!KeyExistsQ[disposition, "name"], Throw[$Failed, $multipartDeserialize]];
            type = getHeaderValue[headers, "content-type"];
            If[!StringQ[type],
                type = If[KeyExistsQ[disposition, "filename"], "application/octet-stream", "text/plain; charset=utf-8"]
            ];
            body = Drop[raw, split[[1, 2]]];
            part = <|
                "Name" -> disposition["name"],
                "Headers" -> headers,
                "ContentType" -> type,
                "Body" -> body,
                "Content" -> If[Length[body] === 0,
                    If[StringStartsQ[mimeType[type], "text/"], "", ByteArray[{}]],
                    HTTPDeserialize[body, type]
                ]
            |>;
            If[KeyExistsQ[disposition, "filename"], part["FileName"] = disposition["filename"]];
            AppendTo[parts, part],
            {i, Length[markers]}
        ];
        $Failed
    ],
    $multipartDeserialize
];


End[];


EndPackage[];


(* ::Chapter:: *)


(*Extensions*)


BeginPackage["WLJS`HTTP`Extensions`",
    {
        "WLJS`HTTP`"
    }
];


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
"SetMIMETable[table_Association] sets a custom MIME types table";


Begin["`Private`"];


AddHTTPHandler[tcp_TCPServer, key_String: "HTTP", http_HTTPHandler] :=
(
    tcp["CompleteHandler", key] = HTTPPacketQ -> HTTPPacketLength;
    tcp["MessageHandler", key] = HTTPPacketQ -> http;
);


GetFileRequestQ[fileType: _String | {__String} | _StringExpression] :=
Function[HTTPRequestMatchQ[#, <|"Method" -> "GET", "Path" -> "/" ~~ ___ ~~ "." ~~ fileType|>]];


GetPOSTRequestQ[fileType: _String | {__String} | _StringExpression] :=
Function[HTTPRequestMatchQ[#, <|"Method" -> "POST", "Path" -> "/" ~~ ___ ~~ "." ~~ fileType|>]];


URLPathToFileName[urlPath_String] :=
FileNameJoin[FileNameSplit[StringTrim[urlPath // URLDecode, "/"]]];


URLPathToFileName[request_Association] :=
URLPathToFileName[request["Path"]];


FileNameToURLPath[fileName_String] :=
URLBuild[FileNameSplit[StringTrim[fileName, StartOfString ~~ Directory[]]]];


Options[ImportFile] = {"Base" :> {Directory[]}, "StringOutput" -> False};


Options[importFile] = Options[ImportFile];


importFile[path_String, opts: OptionsPattern[]] :=
With[{body = ReadByteArray[path]},
    If[!OptionValue["StringOutput"],
        <|
            "Body" -> body,
            "Code" -> 200,
            "Headers" -> <|
                "Content-Type" -> GetMIMEType[path],
                "Content-Length" -> Length[body],
                "Connection" -> "Keep-Alive",
                "Keep-Alive" -> "timeout=5, max=1000",
                "Cache-Control" -> "max-age=60480"
            |>
        |>,
        body // ByteArrayToString
    ]
];


ImportFile[file_String, opts: OptionsPattern[]] :=
Module[{paths = Flatten[OptionValue["Base"]]},
    With[{path = FileNameJoin[{#, file}]},
        If[FileExistsQ[path],
            Return[importFile[path, opts], Module]
        ]
    ]& /@ paths;
    <|"Code" -> 404|>
];


ImportFile[request_Association, opts: OptionsPattern[]] :=
ImportFile[URLPathToFileName[request["Path"]], opts];


ImportFileAsText = ImportFile;


GetMIMEType[file_String] :=
Module[{type},
    type = $mimeTypes[file // FileExtension];
    If[!StringQ[type], type = "application/octet-stream"];
    type
];


$directory = DirectoryName[$InputFileName];


$mimeTypes = WLJS`HTTP`Private`$MIMETypes;


SetMIMETable[assoc_Association] :=
$mimeTypes = assoc;


GetMIMEType[request_Association] :=
GetMIMEType[URLPathToFileName[request["Path"]]];


$DeserializeUrlencoded[request_Association, body_ByteArray] :=
(
    <|<|URLParse["/?" <> (body // ByteArrayToString)]|>["Query"]|>
);


DecryptField[line_String] :=
Module[{stream = StringToStream[line], header, body},
    (* Convert a large string to a stream due to performance isssues *)
    With[{
        extracted = StringExtract[
            (* Read only the header *)
            ReadString[stream, "\r\n\r\n"],
            "\r\n\r\n" -> 1,
            "\r\n" -> 2 ;;
        ]
    },
        (* Extract parameters *)
        header = (
            Association[
                Map[
                    Rule[#1, StringRiffle[{##2}, ":"]]& @@ Map[StringTrim] @ StringSplit[#, ":"]&
                ] @ extracted
            ]
        );
        header["Content-Disposition"] = Join @@ (
            Map[
                StringCases[
                    #,
                    RegularExpression["(\\w*)=\"(.*)\""] :> <|
                        "$1" -> "$2"
                    |>
                ]&,
                (
                    StringTrim /@ StringSplit[header["Content-Disposition"], ";"][[2 ;;]]
                )
            ] // Flatten
        );
        (* Skip gaps *)
        ReadString[stream, "\n"];
        ReadString[stream, "\n"];
        (* The rest is our payload *)
        body = ReadString[stream, EndOfFile];
        (* remove \r\n in the beginng and in the end *)
        body = StringDrop[StringDrop[body, - 4], 1];
        Close[stream];
        <|"Body" -> body, "Headers" -> header|>
    ]
];


$DeserializeMultipart[request_Association, body_ByteArray] :=
Module[{boundary, stream, buffer, field, data},
    boundary = StringCases[request["Headers"]["Content-Type"], RegularExpression["boundary=(.*)"] :> "$1"] // First;
    stream = StringToStream[body // ByteArrayToString];
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
            data[assoc["Headers", "Content-Disposition", "name"]][assoc["Headers", "Content-Disposition", "filename"]] = assoc["Body"];,
            (* 90% probabillity that this is a regualar field *)

            (* name -> value *)
            data[assoc["Headers", "Content-Disposition", "name"]] = assoc["Body"]
        ]
    ] /@ buffer;
    data
];


ProcessMultipart[request_Association, OptionsPattern[]] :=
Module[{},
    (* Return request_Association *)
    If[StringMatchQ[request["Headers"]["Content-Type"], "application/x-www-form-urlencoded" ~~ ___],
        Return[Join[request, <|"Data" -> $DeserializeUrlencoded[request, request["Body"]], "Body" -> {}|>]]
    ];
    If[StringMatchQ[request["Headers"]["Content-Type"], "multipart/form-data" ~~ ___],
        Return[Join[request, <|"Data" -> $DeserializeMultipart[request, request["Body"]], "Body" -> {}|>]]
    ];
];


Options[HypertextProcess] = {
    "Base" :> {Directory[]}
};


HypertextProcess[request_Association, OptionsPattern[]] :=
Module[{body},
    With[{file = URLPathToFileName[request]},
        Block[{Global`$CurrentRequest = request},
            body = LoadPage[file, {}, "Base" -> First @ Flatten @ {OptionValue["Base"]}];
            (* handle special case for redirect *)
            If[KeyExistsQ[Global`$CurrentRequest, "Redirect"],
                Return[
                    <|
                        "Code" -> 201,
                        "Body" -> body,
                        "Headers" -> <|
                            "Content-Location" -> Global`$CurrentRequest["Redirect"],
                            "Content-Length" -> StringLength[body]
                        |>
                    |>
                ]
            ];
            body
        ]
    ]
];


HypertextProcess[request_Association, filename_String, OptionsPattern[]] :=
Module[{body},
    With[{file = filename},
        Block[{Global`$CurrentRequest = <||>},
            Global`$CurrentRequest = request;
            body = LoadPage[file, {}, "Base" -> First @ Flatten @ {OptionValue["Base"]}];
            (* handle special case for redirect *)
            If[KeyExistsQ[Global`$CurrentRequest, "Redirect"],
                Return[
                    <|
                        "Code" -> 201,
                        "Body" -> body,
                        "Headers" -> <|
                            "Content-Location" -> Global`$CurrentRequest["Redirect"],
                            "Content-Length" -> StringLength[body]
                        |>
                    |>
                ]
            ];
            body
        ]
    ]
];


End[];


EndPackage[];
