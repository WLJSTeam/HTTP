(* :Package: *)


(*```
    cd ~/Projects
    git clone https://github.com/WLJSTeam/Objects.git
    git clone https://github.com/WLJSTeam/Internal.git
    git clone https://github.com/WLJSTeam/HTTP.git
    git clone https://github.com/WLJSTeam/CSockets.git
```*)


projectsDirectory = FileNameJoin[{$HomeDirectory, "Projects"}];


pacletDirectories = Map[FileNameJoin[{projectsDirectory, #}]&] @ {
    "Objects",
    "Internal",
    "HTTP",
    "CSockets"
};


Map[PacletDirectoryLoad, pacletDirectories];


Get["WLJS`CSockets`"];
Get["WLJS`HTTP`"];
Get["WLJS`Objects`"];


(*low level API*)


server = CSocketOpen[8080];


http = HTTPHandler[];


handler = CSocketHandler[
    "Received" -> <|"HTTP" -> HTTPPacketQ -> http|>,
    "Accumulator" -> <|"HTTP" -> HTTPPacketQ -> HTTPPacketLength|>
];


img = Plot[Sin[x], {x, 1, 10}];


http["MessageHandler", "GETFile"] =
    HTTPGETFileQ[#, {"html", "txt", "png", "svg"}]& -> HTTPGETFile;


http["MessageHandler", "PlotApi"] =
    HTTPRequestMatchQ[#, <|"Path" -> "/plot", "Query" -> <|"func" -> __, "from" -> __, "to" -> __|>|>]& ->
        Function[With[{
            func = ToExpression @ #Query["func"],
            from = ToExpression @ #Query["from"],
            to = ToExpression @ #Query["to"]
        },
            <|"Body" -> Plot[func[x], {x, from, to}], "ContentType" -> "image/svg+xml"|>
        ]];


SocketListen[server, handler];


While[True,
    Pause[10];
];