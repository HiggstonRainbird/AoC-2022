(* ::Package:: *)

(* ::Text:: *)
(*Written December 14th, 2022.*)

(*Import*)

day = 14;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

input = toExpression[
   StringSplit[#, {",", " -> "}] & /@ 
    StringSplit[Import[inputPath], {"\n"}]];
input = Partition[#, 2] & /@ input;

(*Setup*)

line[{start_, end_}] :=
  If[start[[1]] != end[[1]],
   Table[{x, start[[2]]}, {x, start[[1]], end[[1]], 
     Sign[end[[1]] - start[[1]]]}],
   Table[{start[[1]], y}, {y, start[[2]], end[[2]], 
     Sign[end[[2]] - start[[2]]]}]];

rockPositions = DeleteDuplicates[
   Flatten[Table[
     line /@ Partition[l, 2, 1],
     {l, input}], 2]];
depth = Max[rockPositions[[;; , 2]]];

ClearAll@map;
map[{x_, y_}] := 0;
Do[map[r] = 1, {r, rockPositions}];

generateSand[] :=
  Module[
   {pos = {500, 0}, falling = True},
   While[falling,
    If[pos[[2]] == depth + 1, Return[pos, Module]];
    If[map[pos + #] == 0, pos += #; Continue[]] & /@
     {{0, 
       1}, {-1, 1}, {1, 1}};
    falling = False;
    ];
   Return[pos]
   ];

(*Parts 1 & 2*)

sand = {0, 0};
count = 0;
part1 = {False, 0};
While[sand != {500, 0},
  sand = generateSand[];
  If[sand[[2]] >= depth \[And] ! part1[[1]], part1 = {True, count}];
  map[sand] = 2;
  count += 1];
{part1[[2]], count}
