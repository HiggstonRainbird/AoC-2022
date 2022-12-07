(* ::Package:: *)

(* ::Text:: *)
(*Written December 7th, 2022.*)

(*Import*)

day = 7;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];
input=Import[inputPath,"List"][[1]];


**Setup**

directory = {}; level = {"/"};
Do[
  Which[
   line[[2]] == "cd",
   Which[
    line[[3]] == "/", level = {"/"},
    line[[3]] == "..", level = level[[;; -2]],
    True, level = Join[level, {line[[3]]}]
    ],
   
   IntegerQ[line[[1]]],
   AppendTo[directory, Join[level, {line[[2]]}] -> line[[1]]];
   ]
  , {line, input}];

ClearAll[fileSizes];
fileSizes[l_] := fileSizes[l] = 0;
Do[
  fileSizes[directory[[f, 1, ;; i]]] += directory[[f, 2]],
  {f, Length[directory]},
  {i, Length[directory[[f, 1]]] - 1}];

**Part 1**

Total[Select[DownValues[fileSizes][[;; , 2]], # <= 100000 &]]

**Part 2**

used = Max[DownValues[fileSizes][[;; , 2]]];
unused = 30000000 - (70000000 - used);
Min[Select[DownValues[fileSizes][[;; , 2]], # >= unused &]]
