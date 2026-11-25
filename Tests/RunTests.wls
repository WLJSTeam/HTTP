(* :Package: *)

Needs["MUnit`"];


$root = DirectoryName[$InputFileName, 2];


Scan[PacletDirectoryLoad[FileNameJoin[{DirectoryName[$root], #}]]&, {"Objects", "CSockets"}];


Get[FileNameJoin[{$root, "Kernel", "HTTP.wl"}]];


packet[text_String] :=
<|"DataByteArray" -> StringToByteArray[text, "UTF-8"]|>;


wire[bytes_ByteArray] :=
ByteArrayToString[bytes, "UTF-8"];


$report = TestReport[
    {
        VerificationTest[
            Module[{http = HTTPHandler[], response},
                response = http[packet["GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"]];
                {
                    ByteArrayQ[response],
                    StringStartsQ[wire[response], "HTTP/1.1 404"],
                    !MemberQ[$Packages, "WLJS`CSockets`"]
                }
            ],
            {True, True, True},
            TestID -> "standalone-no-socket-or-CSockets"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], captured, original},
                original = Join[
                    packet["GET /health?a=1&a=2 HTTP/1.1\r\nHost: localhost\r\n\r\n"],
                    <|"TraceID" -> 42, "Handler" -> "transport"|>
                ];
                http["DefaultMessageHandler"] = Function[request, captured = request; "ok"];
                http[original];
                {
                    captured["Method"],
                    captured["Path"],
                    captured["Query", "a"],
                    captured["TraceID"],
                    captured["Handler"],
                    !KeyExistsQ[original, "Result"]
                }
            ],
            {"GET", "/health", {"1", "2"}, 42, "transport", True},
            TestID -> "metadata-and-parsed-request"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], response},
                http["MessageHandler"] = <|
                    "health" -> ((HTTPRequestMatchQ[#, <|"Path" -> "/health"|>]&) -> Function["ok"])
                |>;
                response = wire[http[packet["GET /health HTTP/1.1\r\n\r\n"]]];
                {StringStartsQ[response, "HTTP/1.1 200"], StringEndsQ[response, "\r\n\r\nok"]}
            ],
            {True, True},
            TestID -> "routing-and-string-response"
        ],
        VerificationTest[
            Module[{
                http = HTTPHandler[],
                captured,
                body = "{\"name\":\"\:041f\:0440\:0438\:0432\:0435\:0442\"}",
                request
            },
                request = "POST /api HTTP/1.1\r\ncontent-type: application/json; charset=\"utf-8\"\r\nContent-Length: " <> ToString[Length[StringToByteArray[body]]] <> "\r\n\r\n" <> body;
                http["DefaultMessageHandler"] = Function[r, captured = r; "ok"];
                http[packet[request]];
                {captured["Content", "name"], captured["Body"], captured["ContentCharset"]}
            ],
            {
                "\:041f\:0440\:0438\:0432\:0435\:0442",
                "{\"name\":\"\:041f\:0440\:0438\:0432\:0435\:0442\"}",
                "UTF-8"
            },
            TestID -> "json-charset-and-body"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], captured},
                http["DefaultMessageHandler"] = Function[r, captured = r; "ok"];
                http[
                    packet[
                        "POST /form HTTP/1.1\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: 7\r\n\r\na=1&b=2"
                    ]
                ];
                captured["Content"]
            ],
            <|"a" -> "1", "b" -> "2"|>,
            TestID -> "form-content"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], captured, body = ByteArray[{0, 128, 255}]},
                http["DefaultMessageHandler"] = Function[r, captured = r["Content"]; "ok"];
                http[
                    <|
                        "DataByteArray" -> Join[
                            StringToByteArray[
                                "POST / HTTP/1.1\r\nContent-Type: application/octet-stream\r\nContent-Length: 3\r\n\r\n"
                            ],
                            body
                        ]
                    |>
                ];
                captured
            ],
            ByteArray[{0, 128, 255}],
            TestID -> "binary-request-without-text-decoding"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], response},
                http["DefaultMessageHandler"] = Function[
                    <|
                        "Code" -> 201,
                        "Body" -> "\:041f\:0440\:0438\:0432\:0435\:0442",
                        "Headers" -> <|"content-length" -> 1|>
                    |>
                ];
                response = wire[http[packet["GET / HTTP/1.1\r\n\r\n"]]];
                {
                    StringStartsQ[response, "HTTP/1.1 201"],
                    StringContainsQ[response, "content-length: 12"],
                    StringEndsQ[response, "\:041f\:0440\:0438\:0432\:0435\:0442"]
                }
            ],
            {True, True, True},
            TestID -> "utf8-byte-length-and-legacy-status"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], bytes = ByteArray[{0, 128, 255}], response},
                http["DefaultMessageHandler"] = Function[bytes];
                response = http[packet["GET /binary HTTP/1.1\r\n\r\n"]];
                Take[response, - 3]
            ],
            ByteArray[{0, 128, 255}],
            TestID -> "binary-response"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], response},
                http["DefaultMessageHandler"] = Function[<|"Body" -> 42|>];
                http["Serializer"] = <|"number" -> (NumberQ -> Function[<|"Body" -> "number", "ContentType" -> "text/plain"|>])|>;
                response = wire[http[packet["GET / HTTP/1.1\r\n\r\n"]]];
                {StringContainsQ[response, "content-type: text/plain"], StringEndsQ[response, "number"]}
            ],
            {True, True},
            TestID -> "custom-serializer"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], response},
                http["DefaultMessageHandler"] = Function[HTTPResponse["done", <|"StatusCode" -> 202|>]];
                response = wire[http[packet["GET / HTTP/1.1\r\n\r\n"]]];
                {StringStartsQ[response, "HTTP/1.1 202"], StringEndsQ[response, "done"]}
            ],
            {True, True},
            TestID -> "HTTPResponse-passthrough"
        ],
        VerificationTest[
            Module[{first = "POST / HTTP/1.1\r\ncontent-length:3\r\n\r\nabc", second = "GET / HTTP/1.1\r\n\r\n"},
                {
                    HTTPPacketLength[packet[first <> second]],
                    HTTPPacketLength[packet[second <> second]],
                    HTTPPacketLength[packet["POST / HTTP/1.1\nContent-Length: 3\n\nabc"]]
                }
            ],
            {40, 18, 38},
            TestID -> "framing-excludes-pipelined-data"
        ],
        VerificationTest[
            Quiet[HTTPHandler[][<||>], HTTPHandler::packet],
            $Failed,
            TestID -> "invalid-packet"
        ]
    }
];


Print["HTTP: ", $report["TestsSucceededCount"], " passed; ", $report["TestsFailedCount"], " failed"];


If[$report["TestsFailedCount"] > 0,
    Scan[
        Function[t, Print[{t["TestID"], t["ActualOutput"], t["ExpectedOutput"]}]],
        Values[$report["TestsFailed"]["TestsFailedWrongResults"]]
    ];
    Exit[1]
];


$serialization = Get[FileNameJoin[{$root, "Tests", "Serialization.wlt"}]];


If[!MatchQ[$serialization, _TestReportObject],
    Print["Serialization test file did not return a test report."];
    Exit[1]
];


Print[
    "Serialization: ",
    $serialization["TestsSucceededCount"],
    " passed; ",
    $serialization["TestsFailedCount"],
    " failed"
];


If[$serialization["TestsFailedCount"] > 0,
    Scan[
        Function[t, Print[{t["TestID"], t["ActualOutput"], t["ExpectedOutput"], t["ActualMessages"]}]],
        Join @@ (Values /@ Values[$serialization["TestsFailed"]])
    ];
    Exit[1]
];


Get["WLJS`CSockets`"];


$integration = TestReport[
    {
        VerificationTest[
            Module[{http = HTTPHandler[], handler, result, sent = {}, logged = {}, input},
                http["DefaultMessageHandler"] = Function[r, <|"Body" -> r["Path"]|>];
                handler = CSocketHandler[
                    "Accumulator" -> HTTPPacketLength,
                    "Received" -> http,
                    "Logger" -> Function[p, AppendTo[logged, p]]
                ];
                input = Join[
                    packet["GET /health HTTP/1.1\r\n\r\n"],
                    <|"Event" -> "Received", "SourceSocket" -> CSocketObject[12345]|>
                ];
                Block[{WLJS`CSockets`Private`socketSend = Function[{id, bytes}, AppendTo[sent, bytes]; Length[bytes]]},
                    result = handler[input]
                ];
                {
                    ByteArrayQ[result["Result"]],
                    sent === {result["Result"]},
                    logged === {result},
                    result["DataByteArray"] === input["DataByteArray"],
                    StringEndsQ[wire[result["Result"]], "/health"]
                }
            ],
            {True, True, True, True, True},
            TestID -> "CSocketHandler-owns-Result-and-send"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], handler, first, last, sent = {}, called = 0, base},
                http["DefaultMessageHandler"] = Function[r, called++; r["BodyByteArray"]];
                handler = CSocketHandler["Accumulator" -> HTTPPacketLength, "Received" -> http];
                base = <|"Event" -> "Received", "SourceSocket" -> CSocketObject[12346]|>;
                Block[{WLJS`CSockets`Private`socketSend = Function[{id, bytes}, AppendTo[sent, bytes]; Length[bytes]]},
                    first = handler[Join[base, packet["POST / HTTP/1.1\r\nContent-Length: 4\r\n\r\nab"]]];
                    last = handler[Join[base, packet["cd"]]];
                ];
                {
                    !first["Completed"],
                    !KeyExistsQ[first, "Result"],
                    called,
                    Length[sent],
                    StringEndsQ[wire[last["Result"]], "abcd"]
                }
            ],
            {True, True, 1, 1, True},
            TestID -> "CSockets-assembles-body-before-HTTP"
        ],
        VerificationTest[
            Module[{http = HTTPHandler[], handler, result, sent = {}},
                http["DefaultMessageHandler"] = Function[r, r["Path"]];
                handler = CSocketHandler["Accumulator" -> HTTPPacketLength, "Received" -> http];
                Block[{WLJS`CSockets`Private`socketSend = Function[{id, bytes}, AppendTo[sent, bytes]; Length[bytes]]},
                    result = handler[
                        Join[
                            packet["GET /one HTTP/1.1\r\n\r\nGET /two HTTP/1.1\r\n\r\n"],
                            <|"Event" -> "Received", "SourceSocket" -> CSocketObject[12347]|>
                        ]
                    ];
                ];
                {
                    Length[sent],
                    StringEndsQ[wire[First[sent]], "/one"],
                    StringEndsQ[wire[result["Result"]], "/two"]
                }
            ],
            {2, True, True},
            TestID -> "CSockets-dispatches-pipelined-requests"
        ]
    }
];


Print[
    "CSockets integration: ",
    $integration["TestsSucceededCount"],
    " passed; ",
    $integration["TestsFailedCount"],
    " failed"
];


If[$integration["TestsFailedCount"] > 0, Print[InputForm[$integration["TestsFailed"]]]];


Exit[If[$integration["TestsFailedCount"] === 0, 0, 1]];
