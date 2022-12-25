(* ::Package:: *)

(* ::Text:: *)
(*Written December 23rd, 2022.*)

(*Import*)

day = 23;
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
   StringSplit[#, Alternatives[",", " ", ":", "."]] & /@ 
    StringSplit[Import[inputPath], "\n"]];
input = Select[#, # =!= "" &] & /@ input;
   
(*Setup*)

diagonals = {{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1,
     0}, {1, 1}};
elves = Position[input, "#"];

ClearAll@currentElves;
currentElves[n_] := False;
Do[currentElves[e] = True, {e, elves}];

checks[e_, r_] :=
  And @@ # & /@ Map[
    ! currentElves[e + #] &,
    RotateLeft[{diagonals[[{1, 2, 3}]], diagonals[[{6, 7, 8}]], 
      diagonals[[{1, 4, 6}]], diagonals[[{3, 5, 8}]]}, r - 1],
    {2}];
noNeighbors[e_] := And @@ (! currentElves[e + #] & /@ diagonals);
proposedMove[e_, r_] :=
  Module[{c = checks[e, r], p},
   If[! noNeighbors[e] \[And] AnyTrue[c, # &],
    p = FirstPosition[c, True][[1]];
    e + RotateLeft[{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}, r - 1][[p]],
    
    e]
   ];
   
(* Parts 1 & 2 *)

part1 = 0;
part2 = 0;
Do[
  proposed = Table[e -> proposedMove[e, Mod[r, 4, 1]], {e, elves}];
  same = Count[proposed, _?(#[[1]] === #[[2]] &)];
  If[same == Length[elves], part2 = r; If[part1 != 0, Break[]]];
  globalWatch = {r, same, Length[elves]};
  
  tempElves = GatherBy[proposed, Last];
  failed = Flatten[Select[tempElves, Length[#] > 1 &], 1][[;; , 1]];
  success = Flatten[Select[tempElves, Length[#] == 1 &], 1][[;; , 2]];
  elves = Join[failed, success];
 
  ClearAll@currentElves;
  currentElves[n_] := False;
  Do[currentElves[e] = True, {e, elves}];
  If[
   r == 10,
   part1 = (Max[elves[[;; , 1]]] - Min[elves[[;; , 1]]] + 
        1) (Max[elves[[;; , 2]]] - Min[elves[[;; , 2]]] + 1) - 
     Length[elves];
   If[part2 != 0, Break[]]
   ];
  , {r, 1, 10000}];
{part1, part2}
