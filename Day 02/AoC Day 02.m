(* ::Package:: *)

(* ::Text:: *)
(*Written December 2nd, 2022.*)

(*Import*)

day = 2;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];
input=Import[inputPath,"Table"];


(*Part 1*)

Total[#[[2]] + 3 Mod[#[[2]] - #[[1]] + 1, 3] & /@
     (input /. {"A" -> 1, "B" -> 2, "C" -> 3, "X" -> 1, "Y" -> 2, "Z" -> 3})]



(*Part 2*)

Total[Mod[#[[1]] + #[[2]] + 1, 3, 1] + 3 (#[[2]] - 1) & /@
     (input /. {"A" -> 1, "B" -> 2, "C" -> 3, "X" -> 1, "Y" -> 2, "Z" -> 3})]
