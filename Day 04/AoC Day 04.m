(* ::Package:: *)

(* ::Text:: *)
(*Written December 4th, 2022.*)

(*Import*)

day = 4;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];
input=Import[inputPath,"Table"];

(*Part 1*)

Count[SubsetQ[Range[#[[1]], #[[2]]], Range[#[[3]], #[[4]]]] \[Or] 
      SubsetQ[Range[#[[3]], #[[4]]], Range[#[[1]], #[[2]]]] 
   & /@ input, True]


(*Part 2*)

Count[! DisjointQ[Range[#[[1]], #[[2]]], Range[#[[3]], #[[4]]]] & /@ 
   input, True]
