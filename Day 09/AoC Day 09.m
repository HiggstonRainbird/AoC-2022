(* ::Package:: *)

(* ::Text:: *)
(*Written December 9th, 2022.*)

(*Import*)

day = 9;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

input = toExpression[Import[inputPath, "Table"]];

(*Parts 1 & 2*)

position = Table[{0, 0}, {i, 10}];
part1Positions = {};
part2Positions = {};

Do[
  Which[
   line[[1]] == "R", position[[1]] += {1, 0},
   line[[1]] == "L", position[[1]] += {-1, 0},
   line[[1]] == "U", position[[1]] += {0, 1},
   line[[1]] == "D", position[[1]] += {0, -1}];
  Do[
   If[Max[Abs[position[[i - 1]] - position[[i]]]] >= 2,
    If[Min[Abs[position[[i - 1]] - position[[i]]]] == 0,
     inc = 
      SelectFirst[{{1, 0}, {-1, 0}, {0, 1}, {0, -1}}, 
       Max[Abs[position[[i - 1]] - (# + position[[i]])]] == 1 &],
     inc = 
      SelectFirst[{{1, 1}, {-1, 1}, {1, -1}, {-1, -1}}, 
       Max[Abs[position[[i - 1]] - (# + position[[i]])]] == 1 &]
     ];
    position[[i]] += inc;
    ],
   {i, 2, 10}];
  AppendTo[part1Positions, position[[2]]];
  AppendTo[part2Positions, position[[-1]]],
  {line, input},
  {count, line[[2]]}];
Length /@ {DeleteDuplicates[part1Positions], DeleteDuplicates[part2Positions]}
