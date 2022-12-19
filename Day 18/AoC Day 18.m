(* ::Package:: *)

(* ::Text:: *)
(*Written December 18th, 2022.*)

(*Import*)

day = 18;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] && 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];
input = toExpression[
   StringSplit[#, Alternatives[",", " ", ":"]] & /@ 
    StringSplit[Import[inputPath], "\n"]];

(*Setup*)

dirs[cube_] := {cube + {1, 0, 0}, cube + {-1, 0, 0}, cube + {0, 1, 0},
    cube + {0, -1, 0}, cube + {0, 0, 1}, cube + {0, 0, -1}};
surfaces = dirs /@ input;

ClearAll@cube; cube[pos_] := False;
(cube[#] = True) & /@ input;
   
(*Part 1*)

DeleteDuplicates[{#[[1]] - {1, 0, 0}, Count[#, _?(! cube[#] &)]} & /@ 
    surfaces][[;; , -1]] // Total

(*Part 2*)

lims = {#[[1]] - {1, 1, 1}, #[[2]] + {1, 1, 1}} &@
   Transpose[MinMax /@ Transpose[input]];
outside = {lims[[1]]};
touching = {};
ClearAll@seen;  seen[cube_] := False;
While[
  Length[outside] >= 1,
  (seen[#] = True) & /@ outside;
  touching = 
   Join[touching, Select[outside, MemberQ[cube /@ dirs[#], True] &]];
  outside = DeleteDuplicates@Select[
     Flatten[dirs /@ outside, 1],
     !cube[#] && !seen[#] && !MemberQ[Thread[lims[[1]] <= # <= lims[[2]]], False] &
     ];
  ];

Length[Intersection[#, touching]] & /@ surfaces // Total
