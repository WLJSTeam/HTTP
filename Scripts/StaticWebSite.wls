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
Get["WLJS`Internal`"];
Get["WLJS`Objects`"];


(*low level API*)

server = CSocketOpen[8080];


handler = CSocketHandler[]


handler["DefaultHandler"] = Function[
    Echo[#Data];
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 12\r\n\r\nWLJS Server."
];


(*http level API*)


http = HTTPHandler[];


http["MessageHandler", "GETFILE"] = GetFileRequestQ["html"] -> ImportFile;


(*entry point*)


SocketListen[server, handler];


While[True,
    Pause[10];
];