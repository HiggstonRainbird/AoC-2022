(* ::Package:: *)

(* ::Text:: *)
(*Written December 10th, 2022.*)

(*Import*)

day = 10;
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

newInput = input /. {"addx", x : _} :> Sequence[{"noop"}, {"addx", x}];
cycles = {20, 60, 100, 140, 180, 220};

strength = 1; part1 = 0;
screen = Table[0, {i, 240}];
Do[
  If[newInput[[i, 1]] == "addx", strength += newInput[[i, 2]]];
  If[Abs[Mod[i, 40] - strength] <= 1, screen[[i + 1]] = 1];
  If[MemberQ[cycles, i], part1 += strength*i];,
  {i, Length[newInput]}];

{part1, ArrayPlot[Partition[screen, 40]]}
