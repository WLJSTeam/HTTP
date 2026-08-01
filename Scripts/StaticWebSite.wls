(* :Package: *)

(*```
    cd ~/Projects
    git clone https://github.com/WLJSTeam/Objects.git
    git clone https://github.com/WLJSTeam/HTTP.git
    git clone https://github.com/WLJSTeam/CSockets.git
```*)


SetDirectory[FileNameJoin[{DirectoryName[$InputFileName], "StaticWebSite"}]];


projectsDirectory = FileNameJoin[{$HomeDirectory, "Projects"}];


pacletDirectories = Map[FileNameJoin[{projectsDirectory, #}]&] @ {
    "Objects",
    "HTTP",
    "CSockets",
    "Internal"
};


Map[PacletDirectoryLoad, pacletDirectories];


Get["WLJS`Internal`Console`"];
Get["WLJS`CSockets`"];
Get["WLJS`Objects`"];
Get["WLJS`HTTP`"];


port = First @ Flatten[{SequenceCases[#, {"-p" | "--port", port_} :> ToExpression[port]]& @ $ScriptCommandLine, 8080}];


server = CSocketOpen[port];


handler = CSocketHandler[];


http = HTTPHandler[];


handler += http;


http["MessageHandler", "GETFile"] =
    HTTPGETFileQ[#, {"html", "txt", "png", "svg", "js", "css"}]& -> HTTPGETFile;


http["MessageHandler", "APICount"] =
    HTTPRequestMatchQ[#, <|"Path" -> "/api/" ~~ __|>]& -> Function[<|
        "Body" -> ToExpression[Last[StringSplit[#Path, "/"]]][#Query],
        "ContentType" -> "application/json"
    |>];


$count = 0;


count[_] := Echo[$count++, "COUNT: "];


SocketListen[CSocketList[{server}], handler];


UsingFrontEnd[SystemOpen["http://localhost:8080/file.html"]]


While[True,
    Pause[10];
];