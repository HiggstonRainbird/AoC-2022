(* ::Package:: *)

(* ::Text:: *)
(*Written December 5th, 2022.*)

(*Import*)

day = 5;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];
input=Import[inputPath,"List"];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

(*Setup*)

cutoffLine = Position[input, _?(StringMatchQ[#, Alternatives[" ", DigitCharacter] ..] &), Heads -> False][[1, 1]];
stackCenters = StringPosition[input[[cutoffLine]], DigitCharacter][[;; , 1]];
instructions = toExpression[StringSplit[input[[cutoffLine + 2 ;;]]]];
stacks = Select[#, # != " " &] & /@ 
   Transpose[Characters[#][[stackCenters]] & /@ input[[;; cutoffLine - 1]]];
stacks2 = stacks;

(*Parts 1 & 2*)

Do[
  stacks1[[i[[6]]]] = Join[Reverse[stacks1[[i[[4]], ;; i[[2]]]]], stacks1[[i[[6]]]]];
  stacks1[[i[[4]]]] = Drop[stacks1[[i[[4]]]], i[[2]]];
  
  stacks2[[i[[6]]]] = Join[stacks2[[i[[4]], ;; i[[2]]]], stacks2[[i[[6]]]]];
  stacks2[[i[[4]]]] = Drop[stacks2[[i[[4]]]], i[[2]]];
  , {i, instructions}];
  
Print[StringJoin[stacks1[[;; , 1]]]]
Print[StringJoin[stacks2[[;;,1]]]]
