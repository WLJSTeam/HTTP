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
    "CSockets"
};


Map[PacletDirectoryLoad, pacletDirectories];


Get["WLJS`CSockets`"];
Get["WLJS`Objects`"];
Get["WLJS`HTTP`"];


server = CSocketOpen[8080];


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


SocketListen[server, handler];


While[True,
    Pause[10];
];