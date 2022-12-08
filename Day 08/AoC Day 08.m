(* ::Package:: *)

(* ::Text:: *)
(*Written December 8th, 2022.*)

(*Import*)

day = 8;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

input=toExpression[Characters /@ StringSplit[Import[inputPath], "\n"]];

(*Setup*)

{x, y} = Dimensions[input];
trees[list_, cmp_] := FirstPosition[list, _?(# >= cmp &), {Length[list]}][[1]];

(*Part 1*)

visible = Table[
   If[
    row == x \[Or] column == y \[Or]
     input[[row, column]] >
      Min[
       Max[input[[row, ;; column - 1]]],
       Max[input[[row, column + 1 ;;]]],
       Max[input[[;; row - 1, column]]],
       Max[input[[row + 1 ;;, column]]]
      ], 1, 0],
   {row, x}, {column, y}];
Total[Total[visible]]

(*Part 2*)

scenic = Table[
   Times @@ {
     trees[input[[row, column - 1 ;; 1 ;; -1]], input[[row, column]]],
     trees[input[[row, column + 1 ;;]], input[[row, column]]],
     trees[input[[row - 1 ;; 1 ;; -1, column]], input[[row, column]]],
     trees[input[[row + 1 ;;, column]], input[[row, column]]]},
   {row, 2, x - 1}, {column, 2, y - 1}];
Max[Max[scenic]]
