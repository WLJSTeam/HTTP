(* :Package: *)

(* Run from the repository: wolframscript -f Test.wls *)


projectsDirectory = DirectoryName[$InputFileName, 2];


Scan[PacletDirectoryLoad[FileNameJoin[{projectsDirectory, #}]]&, {"Objects", "HTTP", "CSockets"}];


Get["WLJS`CSockets`"];


Get["WLJS`HTTP`"];


http = HTTPHandler[];


http["DefaultMessageHandler"] = Function[<|"Body" -> "WLJS Server.", "ContentType" -> "text/plain; charset=utf-8"|>];


handler = CSocketHandler["Accumulator" -> HTTPPacketLength, "Received" -> http];


server = CSocketOpen["127.0.0.1", 8080];


listener = SocketListen[server, handler];


CheckAbort[While[True, Pause[0.1]], Null];


RemoveAsynchronousTask[listener];


Pause[1.1];


Close[server];
