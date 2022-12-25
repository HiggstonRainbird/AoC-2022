(* ::Package:: *)

(* ::Text:: *)
(*Written December 24th, 2022.*)

(*Import*)

day = 24;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];

input = Characters /@ StringSplit[Import[inputPath], "\n"];
   
(*Part 1*)

start = {1, Position[input[[1]], "."][[1, 1]]} - {1, 1};
end = {Length[input], Position[input[[-1]], "."][[1, 1]]} - {1, 1};

ClearAll@neighbors;
neighbors[pos_] := Select[
   pos + # & /@ {{-1, 0}, {1, 0}, {0, 0}, {0, 1}, {0, -1}},
   (1 <= #[[1]] <= Length[input] - 2 \[And] 
       1 <= #[[2]] <= Length[input[[1]]] - 2) \[Or] # === 
      start \[Or] # === end &];

directions = {"^", "v", ">", "<"};
moves = {{-1, 0}, {1, 0}, {0, 1}, {0, -1}};
blizzards = Table[# - {1, 1} & /@ Position[input, d], {d, directions}];
boundaries = Dimensions[input] - {2, 2};

nextBlizzards[bl_] := 
 Table[Mod[# + moves[[d]], boundaries, 1] & /@ bl[[d]], {d, 1, 4}]

conf = {start};
Do[
  globalWatch = {round, Length[conf], 
    Min[ManhattanDistance[#, end] & /@ conf]};
  blizzards = nextBlizzards[blizzards];
  ClearAll@blizzardCache; 
  blizzardCache[b_] := False; (blizzardCache[#] = True) & /@ 
   Flatten[blizzards, 1];
  conf = Union[
    Flatten[
     Table[Select[neighbors[pos], ! blizzardCache[#] &], {pos, conf}],
      1]];
  If[MemberQ[conf, end], Print[round]; Break[]]
  , {round, 1, 1000}];
  
(* Part 2 *)

start = {1, Position[input[[1]], "."][[1, 1]]} - {1, 1};
end = {Length[input], Position[input[[-1]], "."][[1, 1]]} - {1, 1};

ClearAll@neighbors;
neighbors[pos_] := Select[
   pos + # & /@ {{-1, 0}, {1, 0}, {0, 0}, {0, 1}, {0, -1}},
   (1 <= #[[1]] <= Length[input] - 2 \[And] 
       1 <= #[[2]] <= Length[input[[1]]] - 2) \[Or] # === 
      start \[Or] # === end &];

directions = {"^", "v", ">", "<"};
moves = {{-1, 0}, {1, 0}, {0, 1}, {0, -1}};
blizzards = Table[# - {1, 1} & /@ Position[input, d], {d, directions}];
boundaries = Dimensions[input] - {2, 2};

nextBlizzards[bl_] := 
 Table[Mod[# + moves[[d]], boundaries, 1] & /@ bl[[d]], {d, 1, 4}]

AbsoluteTiming[
 conf = {start};
 roundCount = 0;
 Do[
  globalWatch = {round, Length[conf], 
    Min[ManhattanDistance[#, end] & /@ conf]};
  blizzards = nextBlizzards[blizzards];
  ClearAll@blizzardCache; 
  blizzardCache[b_] := False; (blizzardCache[#] = True) & /@ 
   Flatten[blizzards, 1];
  conf = Union[
    Flatten[Table[
      Select[neighbors[pos], ! blizzardCache[#] &], {pos, conf}], 
     1]];
  If[MemberQ[conf, end], roundCount += round; 
   Print[{round, roundCount}]; Break[]]
  , {round, 1, 10000}];
 
 conf = {end};
 Do[
  globalWatch = {round + roundCount, Length[conf], 
    Min[ManhattanDistance[#, start] & /@ conf]};
  blizzards = nextBlizzards[blizzards];
  ClearAll@blizzardCache; 
  blizzardCache[b_] := False; (blizzardCache[#] = True) & /@ 
   Flatten[blizzards, 1];
  conf = Union[
    Flatten[Table[
      Select[neighbors[pos], ! blizzardCache[#] &], {pos, conf}], 
     1]];
  If[MemberQ[conf, start], roundCount += round; 
   Print[{round, roundCount}]; Break[]]
  , {round, 1, 10000}];
 
 conf = {start};
 Do[
  globalWatch = {round + roundCount, Length[conf], 
    Min[ManhattanDistance[#, end] & /@ conf]};
  blizzards = nextBlizzards[blizzards];
  ClearAll@blizzardCache; 
  blizzardCache[b_] := False; (blizzardCache[#] = True) & /@ 
   Flatten[blizzards, 1];
  conf = Union[
    Flatten[Table[
      Select[neighbors[pos], ! blizzardCache[#] &], {pos, conf}], 
     1]];
  If[MemberQ[conf, end], roundCount += round; 
   Print[{round, roundCount}]; Break[]]
  , {round, 1, 10000}];
 roundCount]
