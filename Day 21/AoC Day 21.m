(* ::Package:: *)

(* ::Text:: *)
(*Written December 21st, 2022.*)

(*Import*)

day = 21;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] && 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

input = toExpression[
   StringSplit[#, Alternatives[",", " ", ":", "."]] & /@ 
    StringSplit[Import[inputPath], "\n"]];
input = Select[#, # =!= "" &] & /@ input;
   
(*Part 1*)

constants = Select[input, Length[#] == 2 \[And] IntegerQ[#[[2]]] &];
ClearAll@memory;
Do[memory[c[[1]]] = c[[2]], {c, constants}];

operations = 
  Select[input, ! (Length[#] == 2 \[And] IntegerQ[#[[2]]]) &];
g = Graph[Join[
    Flatten[{#[[2]] -> #[[1]], #[[4]] -> #[[1]]} & /@ operations],
    #[[2]] -> #[[1]] & /@ constants]];
ClearAll@getOp;
Do[getOp[p[[1]]] = p, {p, operations}];

priority = 
  Select[TopologicalSort[g], MemberQ[operations[[;; , 1]], #] &];
Do[
  memory[line[[1]]] =
   Which[
    line[[3]] == "+", Plus[memory[line[[2]]], memory[line[[4]]]],
    line[[3]] == "-", Plus[memory[line[[2]]], -memory[line[[4]]]],
    line[[3]] == "*", Times[memory[line[[2]]], memory[line[[4]]]],
    line[[3]] == "/", Times[memory[line[[2]]], 1/memory[line[[4]]]]
    ],
  {line, getOp /@ priority}];
memory["root"]

(*Part 2*)

constants = Select[input, Length[#] == 2 \[And] IntegerQ[#[[2]]] &];
ClearAll@memory;
Do[memory[c[[1]]] = c[[2]], {c, constants}];
memory["humn"] = humn;

operations = 
  Select[input, ! (Length[#] == 2 \[And] IntegerQ[#[[2]]]) &];
g = Graph[Join[
    Flatten[{#[[2]] -> #[[1]], #[[4]] -> #[[1]]} & /@ operations],
    #[[2]] -> #[[1]] & /@ constants]];
ClearAll@getOp;
Do[getOp[p[[1]]] = p, {p, operations}];

priority = 
  Select[TopologicalSort[g], MemberQ[operations[[;; , 1]], #] &];
Do[
  memory[line[[1]]] =
   Which[
    line[[3]] == "+", Plus[memory[line[[2]]], memory[line[[4]]]],
    line[[3]] == "-", Plus[memory[line[[2]]], -memory[line[[4]]]],
    line[[3]] == "*", Times[memory[line[[2]]], memory[line[[4]]]],
    line[[3]] == "/", Times[memory[line[[2]]], 1/memory[line[[4]]]]
    ],
  {line, getOp /@ priority}];
Solve[memory[#[[2]]] == memory[#[[4]]] &@getOp["root"], humn]
