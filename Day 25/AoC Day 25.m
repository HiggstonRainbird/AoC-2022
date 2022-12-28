(* ::Package:: *)

(* ::Text:: *)
(*Written December 25th, 2022.*)

(*Import*)

day = 25;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] := 
  Map[If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &, 
   inputText, {Depth[inputText] - 1, Depth[inputText]}];

input = StringSplit[Import[inputPath], "\n"];
   
(*Part 1*)

sum = Total[
   Total[5^Range[Length[#] - 1, 0, -1]*#] & /@ (toExpression[
       Characters /@ input] /. {"-" -> -1, "=" -> -2})];
balancedBase[n_, b_] :=
 
 Module[{digits = Join[{0}, IntegerDigits[n, b]], pos},
  While[
   Max[digits] > Floor[b/2],
   pos = FirstPosition[
      digits, _?(# > Floor[b/2] &)][[1]];
   digits = 
    Join[digits[[;; pos - 2]], {digits[[pos - 1]] + 
       1}, {digits[[pos]] - b}, digits[[pos + 1 ;;]]]
   ];
  If[digits[[1]] == 0, digits = digits[[2 ;;]]];
  StringJoin[ToString /@ digits /. {"-1" -> "-", "-2" -> "="}]
  ]
balancedBase[sum, 5]
