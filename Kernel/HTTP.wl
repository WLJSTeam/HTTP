(* ::Package:: *)

Once[Map[If[Length[PacletFind[#]] === 0, PacletInstall[#]]&][{
    "KirillBelov/Objects", 
    "KirillBelov/Internal"
}]]; 


BeginPackage["KirillBelov`HTTP`"]; 


EndPackage[(*Kirill`HTTP`*)]; 


Get["KirillBelov`HTTP`Handler`"]; 