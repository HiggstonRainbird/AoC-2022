(* ::Package:: *)

(* ::Text:: *)
(*Written December 12th, 2022.*)

(*Import*)

day = 12;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

input = Characters /@ toExpression[Import[inputPath, "Table"]];

(*Setup*)

map = Flatten[input /. Thread[CharacterRange["a", "z"] -> Range[26]], 1];
sPos = ToString[FirstPosition[map, "S"]];
ePos = ToString[FirstPosition[map, "E"]];
map = map /. {"S" -> 1, "E" -> 26};

g = Graph@Flatten[
    Table[
     {If[i + 1 <= Length[map] \[And] map[[i + 1, j]] - map[[i, j]] <= 1, 
       ToString[{i, j}] -> ToString[{i + 1, j}], Nothing],
      If[j + 1 <= Length[map[[i]]] \[And] map[[i, j + 1]] - map[[i, j]] <= 1, 
       ToString[{i, j}] -> ToString[{i, j + 1}], Nothing],
      If[i + 1 <= Length[map] \[And] map[[i, j]] - map[[i + 1, j]] <= 1, 
       ToString[{i + 1, j}] -> ToString[{i, j}], Nothing],
      If[j + 1 <= Length[map[[i]]] \[And] map[[i, j]] - map[[i, j + 1]] <= 1, 
       ToString[{i, j + 1}] -> ToString[{i, j}], Nothing]},
     {i, Length[map]}, {j, Length[map[[i]]]}]];

(*Part 1*)

GraphDistance[g, sPos, ePos]

(*Part 2*)

Min[GraphDistance[g, #, ePos] & /@ (ToString /@ Position[map, 1])]
