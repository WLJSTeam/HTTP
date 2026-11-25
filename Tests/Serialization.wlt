TestReport[
    {
        VerificationTest[
            Module[{data = <|"text" -> "\:041f\:0440\:0438\:0432\:0435\:0442", "items" -> {1, True, Null}|>, encoded},
                encoded = HTTPSerialize[data];
                {
                    StringStartsQ[encoded["ContentType"], "application/json"],
                    HTTPDeserialize[encoded["Body"], encoded["ContentType"]] === data
                }
            ],
            {True, True},
            TestID -> "json-automatic-roundtrip"
        ],
        VerificationTest[
            HTTPDeserialize[StringToByteArray["{\"ok\":true}"], "Application/Problem+JSON; charset=UTF-8"],
            <|"ok" -> True|>,
            TestID -> "structured-json-suffix-and-case"
        ],
        VerificationTest[
            Module[{
                encoded = HTTPSerialize["\:041f\:0440\:0438\:0432\:0435\:0442", "text/plain; charset=windows-1251"]
            },
                {Length[encoded["Body"]], HTTPDeserialize[encoded["Body"], encoded["ContentType"]]}
            ],
            {6, "\:041f\:0440\:0438\:0432\:0435\:0442"},
            TestID -> "text-charset-roundtrip"
        ],
        VerificationTest[
            HTTPDeserialize[
                StringToByteArray["a=1&a=2&empty=&space=hello+world&plus=%2B&word=%D0%AF"],
                "application/x-www-form-urlencoded"
            ],
            <|"a" -> {"1", "2"}, "empty" -> "", "space" -> "hello world", "plus" -> "+", "word" -> "\:042f"|>,
            TestID -> "urlencoded-browser-form"
        ],
        VerificationTest[
            Module[{data = <|"a" -> {"1", "2"}, "empty" -> "", "text" -> "x + & = \:042f"|>, encoded},
                encoded = HTTPSerialize[data, "application/x-www-form-urlencoded"];
                HTTPDeserialize[encoded["Body"], encoded["ContentType"]]
            ],
            <|"a" -> {"1", "2"}, "empty" -> "", "text" -> "x + & = \:042f"|>,
            TestID -> "urlencoded-serialization"
        ],
        VerificationTest[
            Module[{rows = {{"a,b", "quoted\"text"}, {"line\nbreak", "\:042f"}}, encoded},
                encoded = HTTPSerialize[rows, "text/csv; charset=utf-8"];
                HTTPDeserialize[encoded["Body"], encoded["ContentType"]]
            ],
            {{"a,b", "quoted\"text"}, {"line\nbreak", "\:042f"}},
            TestID -> "csv-quoting-unicode-roundtrip"
        ],
        VerificationTest[
            Module[{encoded = HTTPSerialize[{{"a", "b"}, {"c", "d"}}, "text/tab-separated-values"]},
                HTTPDeserialize[encoded["Body"], encoded["ContentType"]]
            ],
            {{"a", "b"}, {"c", "d"}},
            TestID -> "tsv-roundtrip"
        ],
        VerificationTest[
            Module[{xml = XMLElement["root", {}, {XMLElement["child", {}, {"value"}]}], encoded},
                encoded = HTTPSerialize[xml];
                Cases[
                    HTTPDeserialize[encoded["Body"], encoded["ContentType"]],
                    XMLElement["child", {}, {"value"}],
                    Infinity
                ]
            ],
            {XMLElement["child", {}, {"value"}]},
            TestID -> "xml-roundtrip"
        ],
        VerificationTest[
            Module[{encoded = HTTPSerialize[Image[{{0., 1.}, {1., 0.}}]]},
                {
                    encoded["ContentType"],
                    Take[Normal[encoded["Body"]], 8],
                    ImageDimensions[HTTPDeserialize[encoded["Body"], encoded["ContentType"]]]
                }
            ],
            {"image/png", {137, 80, 78, 71, 13, 10, 26, 10}, {2, 2}},
            TestID -> "png-binary-export-and-import"
        ],
        VerificationTest[
            Module[{path = CreateTemporary[], encoded, bytes = ByteArray[{0, 128, 255}]},
                BinaryWrite[path, bytes];
                Close[path];
                encoded = HTTPSerialize[File[path]];
                DeleteFile[path];
                {encoded["Body"], encoded["ContentType"]}
            ],
            {ByteArray[{0, 128, 255}], "application/octet-stream"},
            TestID -> "file-serialization"
        ],
        VerificationTest[
            Module[{encoded, parts, body = ByteArray[{0, 128, 255, 13, 10}]},
                encoded = HTTPSerialize[
                    {
                        <|"Name" -> "tag", "Body" -> "one"|>,
                        <|"Name" -> "tag", "Body" -> "two"|>,
                        <|"Name" -> "upload", "FileName" -> "test;file.bin", "Body" -> body|>,
                        <|"Name" -> "empty", "Body" -> ""|>
                    },
                    "multipart/form-data"
                ];
                parts = HTTPDeserialize[encoded["Body"], encoded["ContentType"]];
                {
                    Lookup[parts, "Name"],
                    Lookup[Take[parts, 2], "Content"],
                    parts[[3]]["FileName"],
                    parts[[3]]["Body"] === body,
                    parts[[4]]["Body"] === ByteArray[{}]
                }
            ],
            {{"tag", "tag", "upload", "empty"}, {"one", "two"}, "test;file.bin", True, True},
            TestID -> "multipart-repeat-fields-binary-file-and-empty-part"
        ],
        VerificationTest[
            Module[{bytes, parts, binary = Join[ByteArray[{0, 255}], StringToByteArray["\r\n--abcX\r\nkeep"]]},
                bytes = Join[
                    StringToByteArray[
                        "--abc\r\nContent-Disposition: form-data; name=\"file\"; filename=\"a.bin\"\r\nContent-Type: application/octet-stream\r\n\r\n"
                    ],
                    binary,
                    StringToByteArray["\r\n--abc--\r\n"]
                ];
                parts = HTTPDeserialize[bytes, "multipart/form-data; boundary=\"abc\""];
                {Length[parts], parts[[1]]["Body"] === binary}
            ],
            {1, True},
            TestID -> "multipart-delimiters-not-substrings"
        ],
        VerificationTest[
            Module[{encoded = HTTPSerialize[<|"x" -> {"a", "b"}|>, "multipart/form-data; boundary=custom"]},
                Lookup[HTTPDeserialize[encoded["Body"], encoded["ContentType"]], "Content"]
            ],
            {"a", "b"},
            TestID -> "multipart-association-input"
        ],
        VerificationTest[
            HTTPDeserialize[StringToByteArray["--abc--\r\n"], "multipart/form-data; boundary=abc"],
            {},
            TestID -> "multipart-empty-form"
        ],
        VerificationTest[
            Quiet[HTTPDeserialize[StringToByteArray["bad"], "multipart/form-data"], HTTPDeserialize::format],
            $Failed,
            TestID -> "multipart-missing-boundary"
        ],
        VerificationTest[
            Quiet[
                HTTPDeserialize[
                    StringToByteArray["--abc\r\nContent-Disposition: form-data; name=\"x\"\r\n\r\na"],
                    "multipart/form-data; boundary=abc"
                ],
                HTTPDeserialize::format
            ],
            $Failed,
            TestID -> "multipart-truncated-input"
        ],
        VerificationTest[
            HTTPDeserialize[ByteArray[{0, 255}], "application/custom"],
            ByteArray[{0, 255}],
            TestID -> "unknown-content-preserves-bytes"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], response, head, body, pos},
                http["DefaultMessageHandler"] = Function[<|"Body" -> <|"x" -> 1|>, "StatusCode" -> 201|>];
                response = http[packet["GET / HTTP/1.1\r\n\r\n"]];
                pos = First[StringPosition[ByteArrayToString[response, "ISOLatin1"], "\r\n\r\n"]];
                head = ByteArrayToString[Take[response, pos[[1]] - 1]];
                body = Drop[response, pos[[2]]];
                {
                    StringStartsQ[head, "HTTP/1.1 201"],
                    StringContainsQ[head, "application/json"],
                    HTTPDeserialize[body, "application/json"]
                }
            ],
            {True, True, <|"x" -> 1|>},
            TestID -> "handler-infers-json-content-type"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], captured, encoded},
                encoded = HTTPSerialize[{<|"Name" -> "message", "Body" -> "hello"|>}, "multipart/form-data"];
                http["DefaultMessageHandler"] = Function[r, captured = r["Content"]; "ok"];
                http[
                    <|
                        "DataByteArray" -> Join[
                            StringToByteArray[
                                "POST / HTTP/1.1\r\nContent-Type: " <> encoded["ContentType"] <> "\r\nContent-Length: " <> ToString[Length[encoded["Body"]]] <> "\r\n\r\n"
                            ],
                            encoded["Body"]
                        ]
                    |>
                ];
                {Length[captured], captured[[1]]["Content"]}
            ],
            {1, "hello"},
            TestID -> "handler-decodes-multipart"
        ],
        VerificationTest[
            Module[{encoded = HTTPSerialize[Graphics[Circle[]]]},
                {encoded["ContentType"], StringContainsQ[ByteArrayToString[encoded["Body"]], "<svg"]}
            ],
            {"image/svg+xml", True},
            TestID -> "graphics-svg-export"
        ],
        VerificationTest[
            Module[{encoded = HTTPSerialize[Image[{{0., 1.}, {1., 0.}}], "image/jpeg"]},
                {
                    Take[Normal[encoded["Body"]], 2],
                    ImageDimensions[HTTPDeserialize[encoded["Body"], "image/jpeg"]]
                }
            ],
            {{255, 216}, {2, 2}},
            TestID -> "jpeg-export-and-import"
        ],
        VerificationTest[
            Module[{doc = XMLObject["Document"][{}, XMLElement["root", {}, {"value"}], {}], encoded},
                encoded = HTTPSerialize[doc];
                {
                    StringStartsQ[encoded["ContentType"], "application/xml"],
                    MatchQ[HTTPDeserialize[encoded["Body"], "application/xml"], XMLObject["Document"][___]]
                }
            ],
            {True, True},
            TestID -> "xml-document-inference"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], called = False},
                http["DefaultMessageHandler"] = Function[called = True; "ok"];
                Quiet[
                    http[packet["POST / HTTP/1.1\r\nContent-Type: application/json\r\nContent-Length: 1\r\n\r\n{"]],
                    HTTPDeserialize::format
                ];
                called
            ],
            False,
            TestID -> "malformed-json-does-not-invoke-application"
        ],
        VerificationTest[
            Quiet[HTTPSerialize[{}, "multipart/form-data; boundary=bad\r\nboundary"], HTTPSerialize::format],
            $Failed,
            TestID -> "multipart-invalid-boundary"
        ],
        VerificationTest[
            Module[{parts},
                parts = HTTPDeserialize[
                    StringToByteArray["--abc\r\nContent-Disposition: form-data; name=\"empty\"\r\n\r\n\r\n--abc--\r\n"],
                    "multipart/form-data; boundary=abc"
                ];
                parts[[1]]["Content"]
            ],
            "",
            TestID -> "multipart-empty-text-field"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], response, pos, body, head, decoded},
                http["DefaultMessageHandler"] = Function[<|"Body" -> ByteArray[{0, 128, 255}], "ContentEncoding" -> "gzip"|>];
                response = http[packet["GET / HTTP/1.1\r\n\r\n"]];
                pos = First[StringPosition[ByteArrayToString[response, "ISOLatin1"], "\r\n\r\n"]];
                head = ByteArrayToString[Take[response, pos[[1]] - 1]];
                body = Drop[response, pos[[2]]];
                decoded = ByteArray[ImportByteArray[body, {"GZIP", "Byte"}]];
                {
                    decoded,
                    StringContainsQ[head, "content-encoding: gzip"],
                    StringContainsQ[head, "content-length: " <> ToString[Length[body]]]
                }
            ],
            {ByteArray[{0, 128, 255}], True, True},
            TestID -> "gzip-response-raw-bytes-and-wire-length"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], captured},
                http["DefaultMessageHandler"] = Function[r, captured = r["Content"]; "ok"];
                http[
                    <|
                        "DataByteArray" -> Join[
                            StringToByteArray[
                                "POST / HTTP/1.1\r\nContent-Type: application/json\r\nContent-Encoding: gzip\r\nContent-Length: 31\r\n\r\n"
                            ],
                            ByteArray[
                                {
                                    31,
                                    139,
                                    8,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    2,
                                    255,
                                    171,
                                    86,
                                    202,
                                    207,
                                    86,
                                    178,
                                    42,
                                    41,
                                    42,
                                    77,
                                    173,
                                    5,
                                    0,
                                    144,
                                    95,
                                    212,
                                    167,
                                    11,
                                    0,
                                    0,
                                    0
                                }
                            ]
                        ]
                    |>
                ];
                captured
            ],
            <|"ok" -> True|>,
            TestID -> "gzip-request-external-fixture"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], called = False, result},
                http["DefaultMessageHandler"] = Function[called = True; "ok"];
                result = Quiet[
                    http[packet["POST / HTTP/1.1\r\nContent-Encoding: br\r\nContent-Length: 1\r\n\r\na"]],
                    HTTPHandler::encoding
                ];
                {result, called}
            ],
            {$Failed, False},
            TestID -> "unsupported-content-encoding"
        ],
        VerificationTest[
            HTTPDeserialize[StringToByteArray["const x = 1;"], "application/javascript"],
            "const x = 1;",
            TestID -> "javascript-text-content"
        ],
        VerificationTest[
            Module[{bytes, parts, binary = StringToByteArray["\r\n--abc--not-a-delimiter\r\nkeep"]},
                bytes = Join[
                    StringToByteArray["--abc\r\nContent-Disposition: form-data; name=\"file\"; filename=\"a.bin\"\r\n\r\n"],
                    binary,
                    StringToByteArray["\r\n--abc--\r\n"]
                ];
                parts = HTTPDeserialize[bytes, "multipart/form-data; boundary=abc"];
                parts[[1]]["Body"] === binary
            ],
            True,
            TestID -> "multipart-closing-delimiter-must-end-line"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], called = False, result},
                http["DefaultMessageHandler"] = Function[called = True; "ok"];
                result = Quiet[
                    http[packet["POST / HTTP/1.1\r\nContent-Encoding: gzip\r\nContent-Length: 3\r\n\r\nbad"]],
                    HTTPHandler::encoding
                ];
                {result, called}
            ],
            {$Failed, False},
            TestID -> "invalid-gzip-content"
        ]
    }
]
