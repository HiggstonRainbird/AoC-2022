(* ::Package:: *)

(* ::Text:: *)
(*Written December 20th, 2022.*)

(*Import*)

day = 20;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] && 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

input = toExpression[Import[inputPath, "List"]];

(*Setup*)

(* moveElement[] courtesy of https://mathematica.stackexchange.com/a/133809 *)
moveElement[l_, from_, to_] := Insert[Delete[l, from], l[[from]], to];
mixList[list_] :=
  Module[{pos, element, output = list},
   Do[
    pos = FirstPosition[output, {i, _}, Heads -> False][[1]];
    element = output[[pos]];
    output = 
     moveElement[output, pos, 
      Mod[pos + element[[2]], Length[output] - 1, 1]];
    globalWatch = {i, Length[output]},
    {i, Length[output]}];
   output];
   
(*Part 1*)

newInput = Table[{i, input[[i]]}, {i, Length[input]}];
newInput = mixList[newInput];

zero = Position[newInput, {_, 0}][[1, 1]];
Sum[newInput[[Mod[zero + 1000*i, Length[newInput]], -1]], {i, 1, 3}]

(*Part 2*)

newInput = Table[{i, input[[i]]*811589153}, {i, Length[input]}];
Do[
 newInput = mixList[newInput],
 {m, 1, 10}];
zero = Position[newInput, {_, 0}][[1, 1]];
Sum[newInput[[Mod[zero + 1000*i, Length[newInput]], -1]], {i, 1, 3}]
