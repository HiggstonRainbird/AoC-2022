(* ::Package:: *)

(* ::Text:: *)
(*Written December 15th, 2022.*)

(*Import*)

day = 15;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

input = toExpression[
   StringSplit[#, {"=", ":", ",", " "}] & /@ 
    StringSplit[Import[inputPath], {"\n"}]];

(*Setup*)

sensors = input[[;; , {4, 7}]];
beacons = input[[;; , {14, 17}]];

(*Part 1*)

RegionMeasure@Region[IntervalUnion @@
   Table[
    beaconDist = ManhattanDistance[sensors[[i]], beacons[[i]]];
    rowDist = 
     ManhattanDistance[{sensors[[i, 1]], 2000000}, sensors[[i]]];
    diff = beaconDist - rowDist;
    If[diff < 0, Nothing, 
     Interval[{sensors[[i, 1]] - diff, sensors[[i, 1]] + diff}]]
    , {i, Length[input]}]]
    
(*Part 2*)

(x*4000000 + y) /.
 FindInstance[Join[
    And @@ Table[
       ManhattanDistance[sensors[[i]], {x, y}] >
        ManhattanDistance[sensors[[i]], beacons[[i]]]
       , {i, Length[input]}] && 0 <= {x, y} <= 4000000],
   {x, y}, Integers][[1]]
