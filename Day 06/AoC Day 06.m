(* ::Package:: *)

(* ::Text:: *)
(*Written December 6th, 2022.*)

(*Import*)

day = 6;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];
input=Import[inputPath,"List"][[1]];

(*Setup*)

firstDuplicateFreeSet[n_] := 
        Position[
               Partition[Characters[input], n, 1], 
               _?(DuplicateFreeQ[#] &), 
               {1}, Heads -> False][[1, 1]
        ] + n - 1

(*Part 1*)

firstDuplicateFreeSet[4]

(*Part 2*)

firstDuplicateFreeSet[14]
