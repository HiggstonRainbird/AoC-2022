(* ::Package:: *)

(* ::Text:: *)
(*Written December 11th, 2022.*)

(*Import*)

day = 11;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

input = Select[
   toExpression[
    Select[StringSplit[StringTrim[#], Alternatives[",", ":", " "]], 
       StringLength[#] > 0 &] & /@ 
     StringSplit[Import[inputPath], "\n"]],
   Length[#] > 0 &];

(*Setup*)

monkeys = {};
Do[
  Which[
    line[[1]] == "Monkey",
    AppendTo[monkeys, line[[2]]],
    
    line[[1]] == "Starting",
    monkeys[[-1]] = (monkeys[[-1]] -> line[[3 ;;]]),
    
    line[[1]] == "Operation",
    monkeys[[-1]] = {monkeys[[-1]], 
      Function[old, 
       Evaluate[ToExpression[StringJoin[ToString /@ line[[-3 ;;]]]]]]},
    
    line[[1]] == "Test",
    monkeys[[-1]] = 
     Join[monkeys[[-1]], {Function[new, 
        Evaluate[Divisible[new, line[[-1]]]]]}],
    
    line[[1]] == "If",
    monkeys[[-1]] = Join[monkeys[[-1]], {line[[-1]]}]
    ];
  , {line, input}];
monkeys = Join[#, {0}] & /@ monkeys;

(*Part 1*)

timeStep[monkeys_] :=
  Module[{newMonkeys = monkeys, items},
   Do[
    items = newMonkeys[[m + 1, 2]] /@ newMonkeys[[m + 1, 1, 2]];
    items = \[LeftFloor]items/3\[RightFloor];
    newMonkeys[[m + 1, 1, 2]] = {};
    newMonkeys[[m + 1, 6]] += Length[items];
    Do[
     If[newMonkeys[[m + 1, 3]][item],
      newMonkeys[[newMonkeys[[m + 1, 4]] + 1, 1, 2]] = 
       Join[newMonkeys[[newMonkeys[[m + 1, 4]] + 1, 1, 2]], {item}],
      newMonkeys[[newMonkeys[[m + 1, 5]] + 1, 1, 2]] = 
       Join[newMonkeys[[newMonkeys[[m + 1, 5]] + 1, 1, 2]], {item}]]
     , {item, items}]
    , {m, monkeys[[;; , 1, 1]]}];
   Return[newMonkeys, Module]
   ];
Times @@ Sort[Nest[timeStep, monkeys, 20][[;; , -1]]][[-2 ;;]]

(*Part 2*)

mod = LCM @@ Select[input, #[[1]] == "Test" &][[;; , -1]];
timeStep2[monkeys_] :=
  Module[{newMonkeys = monkeys, items},
   Do[
    items = 
     Mod[newMonkeys[[m + 1, 2]] /@ newMonkeys[[m + 1, 1, 2]], mod];
    newMonkeys[[m + 1, 1, 2]] = {};
    newMonkeys[[m + 1, 6]] += Length[items];
    Do[
     If[newMonkeys[[m + 1, 3]][item],
      newMonkeys[[newMonkeys[[m + 1, 4]] + 1, 1, 2]] = 
       Join[newMonkeys[[newMonkeys[[m + 1, 4]] + 1, 1, 2]], {item}],
      newMonkeys[[newMonkeys[[m + 1, 5]] + 1, 1, 2]] = 
       Join[newMonkeys[[newMonkeys[[m + 1, 5]] + 1, 1, 2]], {item}]]
     , {item, items}]
    , {m, monkeys[[;; , 1, 1]]}];
   Return[newMonkeys, Module]
   ];
Times @@ Sort[
    Nest[timeStep2, monkeys, 
      10000][[;; , -1]]][[-2 ;;]] // AbsoluteTiming
