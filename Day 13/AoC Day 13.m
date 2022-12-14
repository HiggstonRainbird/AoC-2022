(* ::Package:: *)

(* ::Text:: *)
(*Written December 13th, 2022.*)

(*Import*)

day = 13;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

input = toExpression[StringSplit[Import[inputPath], "\n"]];
input = Select[
   ToExpression[StringReplace[#, {"[" -> "{", "]" -> "}"}]] & /@ 
    input, # =!= Null &];

(*Setup*)

compare[left_, right_] :=
  Which[
   IntegerQ[left] && IntegerQ[right], Order[left, right],
   IntegerQ[left] && ListQ[right], compare[{left}, right],
   ListQ[left] && IntegerQ[right], compare[left, {right}],
   
   ListQ[left] && ListQ[right],
   FirstCase[
    Table[
     compare[left[[i]], right[[i]]], {i, 
      Min[Length /@ {left, right}]}],
    Except[0], Order[Length[left], Length[right]]]
   ];

(*Part 1*)

Total[Flatten[Position[compare @@ # & /@ Partition[input, 2], 1]]]

(*Part 2*)

sorted = Sort[Join[input, {{{2}}, {{6}}}], compare[#1, #2] &];
Position[sorted, {{6}}, {1}, Heads -> False][[1, 1]]*
Position[sorted, {{2}}, {1}, Heads -> False][[1, 1]]
