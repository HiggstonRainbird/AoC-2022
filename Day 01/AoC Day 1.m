(* ::Package:: *)

(* ::Text:: *)
(*Written December 1st, 2022.*)


(*Import*)

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

input=StringSplit[Import[inputPath],"\n\n"];
input=toExpression[StringSplit[#,"\n"]&/@input];


(*Part 1*)

Max[Total/@input]


(*Part 2*)

Total[Sort[Total/@input][[-3;;]]]
