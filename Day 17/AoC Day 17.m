(* ::Package:: *)

(* ::Text:: *)
(*Written December 17th, 2022.*)

(*Import*)

day = 17;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

input = Characters@Import[inputPath];

(*Setup*)

WIDTH = 2^7 - 1;
pieces = {
   {15},
   {2, 7, 2},
   {1, 1, 7},
   {1, 1, 1, 1},
   {3, 3}
   };
moveLeft[{p_, x_, y_}, w_] :=
  If[
   Max[BitShiftLeft[pieces[[p]], x + 1]] > WIDTH ||
    Max[BitAnd[w[[y ;; y + Length[pieces[[p]]] - 1]], 
       BitShiftLeft[pieces[[p]], x + 1]]] != 0,
   {p, x, y},
   {p, x + 1, y}
   ];
moveRight[{p_, x_, y_}, w_] :=
  If[
   x == 0 ||
    Max[BitAnd[w[[y ;; y + Length[pieces[[p]]] - 1]], 
       BitShiftLeft[pieces[[p]], x - 1]]] != 0,
   {p, x, y},
   {p, x - 1, y}
   ];
moveDown[{p_, x_, y_}, w_] :=
  If[
   Length[w] < y + Length[pieces[[p]]] ||
    Max[BitAnd[w[[y + 1 ;; y + Length[pieces[[p]]]]], 
       BitShiftLeft[pieces[[p]], x]]] != 0,
   {p, x, y},
   {p, x, y + 1}
   ];

dropPiece[oldPiece_, n_, w_] :=
  
  Module[{piece = oldPiece, newN = n},
   While[
    True,
    If[input[[Mod[newN, Length[input]] + 1]] == "<",
     piece = moveLeft[piece, w],
     piece = moveRight[piece, w]];
    newN += 1;
    If[moveDown[piece, w] == piece,
     Break[],
     piece = moveDown[piece, w]];
    ];
   {piece, newN}
   ];
   
(*Part 1*)

well = Table[0, {i, 10000}];
n = 0;
Do[
  highestRock = 
   FirstPosition[well, Except[0], {Length[well] + 1, 1}, 1, 
     Heads -> False][[1]];
  x = 0;
  p = Mod[count - 1, 5] + 1;
  piece = {p, x, highestRock - Length[pieces[[p]]] - 3};
  While[moveLeft[piece, well] != piece, piece = moveLeft[piece, well]];
  piece = moveRight[piece, well]; piece = moveRight[piece, well];
  {piece, n} = dropPiece[piece, n, well];
  
  well[[piece[[3]] ;; piece[[3]] + Length[pieces[[p]]] - 1]] += 
   BitShiftLeft[pieces[[p]], piece[[2]]];
  , {count, 1, 2022}];
  
Count[well, Except[0]]

(*Part 2*)

surface[w_] := 
  FirstPosition[FoldList[BitOr, w], WIDTH, {Length[w] + 1, 1}][[1]] - 
   1;
padding[w_, p_] := 
  FirstPosition[Reverse[w], 0, {Length[w] + 1, 1}, {1}, 
     Heads -> False][[1]] + Length[pieces[[p]]] + 3 - 1;

ClearAll@storage;
storage[w_, p_, n_] := {0, 0};

well = {};
n = 0;
height = 0;
goal = 1000000000000;
Do[
  x = 0;
  p = Mod[count - 1, 5] + 1;
  piece = {p, x, 1};
  
  well = PadLeft[well, padding[well, p]];
  
  While[moveLeft[piece, well] != piece, piece = moveLeft[piece, well]];
  piece = moveRight[piece, well]; piece = moveRight[piece, well];
  oldN = n;
  
  {piece, n} = dropPiece[piece, n, well];
  
  well[[piece[[3]] ;; piece[[3]] + Length[pieces[[p]]] - 1]] += 
   BitShiftLeft[pieces[[p]], piece[[2]]];
  s = surface[well];
  If[s < Length[well],
   height += Length[well] - s;
   well = well[[;; s]]];
  
  If[
   storage[well, p, Mod[oldN, Length[input]] + 1][[1]] == 0,
   storage[well, p, Mod[oldN, Length[input]] + 1] = {count, height},
   {roundA, heightA} = 
    storage[well, p, Mod[oldN, Length[input]] + 1] + {1, 0};
   {roundB, heightB} = {count, height} + {1, 0};
   Break[];
   ];
  globalWatch = count;
  , {count, 1, goal}];

roundedRound = (roundB - roundA) Floor[(goal - roundA)/(roundB - roundA)] + roundA;
stored = SelectFirst[DownValues[storage], #[[2, 1]] == roundA + goal - roundedRound &];
extra = Count[#[[1, 1, 1]], Except[0]] &@stored;
newHeight = (roundedRound - roundA) ((heightB - heightA)/(roundB - roundA)) + #[[2, 2]] &@stored;
newHeight + extra
