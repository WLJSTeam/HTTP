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


http["MessageHandler", "GETFILE"] =
    Function[True] -> Function["HTTP/1.1 200 OK\r\nContent-Legnth: 4\r\n\r\nWLJS"];


SocketListen[server, (Echo[#Data]; handler[#])&];


While[True,
    Pause[10];
];