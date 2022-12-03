(* ::Package:: *)

(* ::Text:: *)
(*Written December 3rd, 2022.*)

(*Import*)

day = 3;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];
input=Import[inputPath,"Table"];

values = Thread[Join[CharacterRange["a", "z"], CharacterRange["A", "Z"]] -> Range[52]];

(*Part 1*)

Total[(Intersection @@ Characters[StringPartition[#, StringLength[#]/2]] & /@ input) /. values]


(*Part 2*)

Total[Intersection @@ Characters[#] & /@ Partition[input, 3] /. values]
