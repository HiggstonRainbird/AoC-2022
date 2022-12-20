(* ::Package:: *)

(* ::Text:: *)
(*Written December 19th, 2022.*)

(*Import*)

day = 19;
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
input = Table[
   Select[SplitBy[line, # == "" &], Length[#] > 1 &], {line, input}];
blueprints = 
  Table[{{#[[2, 5]], 0, 0, 0}, {#[[3, 5]], 0, 0, 
       0}, {#[[4, 5]], #[[4, 8]], 0, 0}, {#[[5, 5]], 0, #[[5, 8]], 
       0}} &@line, {line, input}];

(*Setup*)

possibleIndex[resources_, blueprint_] := 
  Boole[Table[! MemberQ[Thread[resources >= b], False], {b, 
     blueprint}]];
indexSplit[robots_] := DiagonalMatrix[robots];
strictlyBetter[list_] := -Internal`ListMin[-list];
filterPoss2[poss_, minute_, max_] :=
  
  Module[{newPoss, best, timeLeft = max - minute, cumulative},
   cumulative = (timeLeft*(timeLeft - 1))/2;
   newPoss = Flatten[
     Table[
      Table[{p[[1, 1]], b}, {b, strictlyBetter[p[[;; , 2]]]}]
      , {p, GatherBy[poss, First]}], 1];
   newPoss = Flatten[
     Table[
      Table[{b, p[[1, 2]]}, {b, strictlyBetter[p[[;; , 1]]]}]
      , {p, GatherBy[poss, Last]}], 1];
   best = Max[#[[1, 4]] + timeLeft #[[2, 4]] & /@ newPoss];
   newPoss = 
    Select[newPoss, #[[1, 4]] + timeLeft*#[[2, 4]] + cumulative >= 
       best &]
   ];

timestep[{resources_, robots_}, blueprint_] :=
  
  Module[{possibleRobots, index},
   possibleRobots = possibleIndex[resources, blueprint];
   index = indexSplit[possibleRobots];
   Join[
    If[Total[possibleRobots] == 
      4, {Nothing}, {{resources + robots, robots}}],
    Table[
     {resources - blueprint[[i]] + robots, robots + index[[i]]},
     {i, Flatten[Position[possibleRobots, 1]]}]
    ]];
   
(*Part 1*)

qualityList = {};
t = AbsoluteTime[];
max = 24;
AbsoluteTiming[
 MemoryConstrained[
  Do[
   possibilities = {{{0, 0, 0, 0}, {1, 0, 0, 0}}};
   Do[
    globalWatch = {i, minute, AbsoluteTime[] - t, 
      Length[possibilities]};
    possibilities =
     filterPoss2[
        Flatten[timestep[#, blueprints[[i]]] & /@ #, 1], minute, max
        ] &@possibilities
    , {minute, max}];
   quality = {i, Max[possibilities[[;; , 1, 4]]]};
   AppendTo[qualityList, quality];
   , {i, 1, Length[blueprints]}],
  10*10^9];
 Total[Times @@ # & /@ qualityList]]

(*Part 2*)

qualityList = {};
t = AbsoluteTime[];
max = 32;
AbsoluteTiming[
 MemoryConstrained[
  Do[
   possibilities = {{{0, 0, 0, 0}, {1, 0, 0, 0}}};
   Do[
    globalWatch = {i, minute, AbsoluteTime[] - t, 
      Length[possibilities]};
    possibilities =
     filterPoss2[
        Flatten[timestep[#, blueprints[[i]]] & /@ #, 1], minute, max
        ] &@possibilities
    , {minute, max}];
   quality = {i, Max[possibilities[[;; , 1, 4]]]};
   AppendTo[qualityList, quality];
   , {i, 1, 3}],
  10*10^9];
 Times @@ qualityList[[;; , 2]]]
