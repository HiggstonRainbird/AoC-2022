(* ::Package:: *)

(* ::Text:: *)
(*Written December 22nd, 2022.*)

(*Import*)

day = 22;
inputPath = FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "Input.txt"}];

input = Characters /@ StringSplit[Import[inputPath], "\n"];
   
(*Part 1*)

map = input[[;; -2]];
directions = toExpression[StringJoin /@ SplitBy[input[[-1]], DigitQ]];

sparseMap = SparseArray[
   Join[
    # -> 1 & /@ Position[map, "."],
    # -> 2 & /@ Position[map, "#"]
    ]
   ];
ClearAll@sparseRow; ClearAll@sparseColumn;
sparseRow[row_] := 
  sparseRow[row] = 
   Flatten[Position[Normal[sparseMap[[row, ;;]]], Except[0], {1}, 
     Heads -> False]];
sparseColumn[column_] := 
  sparseColumn[column] = 
   Flatten[Position[Normal[sparseMap[[;; , column]]], Except[0], {1}, 
     Heads -> False]];
pos = Position[map, "."][[1]];
dir = {">", "^", "<", "v"};
step = {">" -> {0, 1}, "v" -> {1, 0}, "<" -> {0, -1}, "^" -> {-1, 0}};

next[pos_, dir_] :=
 Module[{s, nextPos = pos},
  s = (dir /. step);
  Which[
   s === {1, 0},
   If[pos[[1]] + s[[1]] > Max[sparseColumn[pos[[2]]]], 
    nextPos[[1]] = Min[sparseColumn[pos[[2]]]], 
    nextPos[[1]] = pos[[1]] + s[[1]]],
   
   s === {-1, 0},
   If[pos[[1]] + s[[1]] < Min[sparseColumn[pos[[2]]]], 
    nextPos[[1]] = Max[sparseColumn[pos[[2]]]], 
    nextPos[[1]] = pos[[1]] + s[[1]]],
   
   s === {0, 1},
   If[pos[[2]] + s[[2]] > Max[sparseRow[pos[[1]]]], 
    nextPos[[2]] = Min[sparseRow[pos[[1]]]], 
    nextPos[[2]] = pos[[2]] + s[[2]]],
   
   s === {0, -1},
   If[pos[[2]] + s[[2]] < Min[sparseRow[pos[[1]]]], 
    nextPos[[2]] = Max[sparseRow[pos[[1]]]], 
    nextPos[[2]] = pos[[2]] + s[[2]]]
   ];
  nextPos
  ]

Do[
  Which[
    d == "L", dir = RotateLeft[dir],
    d == "R", dir = RotateRight[dir],
    True,
    Do[
     nextPos = next[pos, dir[[1]]];
     If[sparseMap[[Sequence @@ nextPos]] != 2, pos = nextPos, Break[]];
     , {s, d}]];
  , {d, directions}];

Total[{1000, 4, 1}*# &@
  Join[pos, {dir[[1]] /. {">" -> 0, "v" -> 1, "<" -> 2, "^" -> 3}}]]

(*Part 2*)

neighbors[list_, p3_] :=
 Select[
  p3 + # & /@ {{-1, 0, 0}, {1, 0, 0}, {0, -1, 0}, {0, 1, 0}, {0, 
     0, -1}, {0, 0, 1}},
  1 <= #[[1]] <= Length[list] \[And] 
    1 <= #[[2]] <= Length[list[[p3[[1]]]]] \[And] 
    1 <= #[[3]] <= Length[list[[p3[[1]], p3[[2]]]]] \[And]
    list[[Sequence @@ #]] != 0 &]

map = input[[;; -2]];
directions = toExpression[StringJoin /@ SplitBy[input[[-1]], DigitQ]];

sparseMap2D = SparseArray[
   Join[
    # -> 1 & /@ Position[map, "."],
    # -> 2 & /@ Position[map, "#"]
    ]
   ];
squares = Select[
   Tuples[{{1, 51, 101, 151}, {1, 51, 101}}],
   sparseMap2D[[Sequence @@ #]] != 0 &];

flatToCube[{row_, col_}] :=
  Module[{quadrant, pos3D = {0, 0, 0}},
   quadrant = FirstPosition[
      squares,
      _?(Max[{row, col} - #] <= 49 \[And] 
          Min[{row, col} - #] >= 0 &),
      0,
      {1}, Heads -> False][[1]];
   
   pos3D = Which[
     quadrant == 1, {row, col - 50, 0} + {1, 1, 1},
     quadrant == 2, {row, 50, col - 100} + {1, 2, 1},
     quadrant == 3, {50, col - 50, row - 50} + {2, 1, 1},
     quadrant == 4, {150 - row, 0, col} + {2, 1, 1},
     quadrant == 5, {150 - row, col - 50, 50} + {2, 1, 2},
     quadrant == 6, {0, row - 150, col} + {1, 1, 1}
     ];
   
   pos3D
   ];
plane[pos3D_] := 
  If[Max[#] - Min[#] != 0, 0, 1] & /@ 
   Transpose[Join[{pos3D}, neighbors[sparseMap3D, pos3D]]];

ClearAll@cubeToFlat;
(cubeToFlat[flatToCube[#]] = #) & /@ 
  Position[Normal[sparseMap2D], Except[0], {2}, Heads -> False];
sparseMap3D = 
  SparseArray[
   flatToCube[#] -> sparseMap2D[[Sequence @@ #]] & /@ 
    Position[Normal[sparseMap2D], Except[0], {2}, Heads -> False]];
ClearAll@sparseRow; ClearAll@sparseColumn;
sparseRow[row_] := 
  sparseRow[row] = 
   Flatten[Position[Normal[sparseMap2D[[row, ;;]]], Except[0], {1}, 
     Heads -> False]];
sparseColumn[column_] := 
  sparseColumn[column] = 
   Flatten[Position[Normal[sparseMap2D[[;; , column]]], 
     Except[0], {1}, Heads -> False]];

step = {">" -> {0, 1}, "v" -> {1, 0}, "<" -> {0, -1}, "^" -> {-1, 0}};

next3[pos_, dir_] :=
  Module[{s, nextPos = pos,
    pos3old, pos3ghost, pos3new,
    dirnew,
    facing3new},
   s = (dir /. step);
   dirnew = s;
   If[
    ! (Min[sparseColumn[pos[[2]]]] <= pos[[1]] + s[[1]] <= 
        Max[sparseColumn[pos[[2]]]] \[And]
       Min[sparseRow[pos[[1]]]] <= pos[[2]] + s[[2]] <= 
        Max[sparseRow[pos[[1]]]]),
    
    pos3old = flatToCube[pos];
    pos3ghost = 2 pos3old - flatToCube[pos - s];
    pos3new = 
     Complement[neighbors[sparseMap3D, pos3ghost], {pos3old}][[1]];
    nextPos = cubeToFlat[pos3new];
    dirnew = cubeToFlat[2 pos3new - pos3ghost] - nextPos,
    
    nextPos += s;
    ];
   {nextPos, dirnew}
   ];

pos = Position[map, "."][[1]];
dir = {">", "^", "<", "v"};
step = {">" -> {0, 1}, "v" -> {1, 0}, "<" -> {0, -1}, 
   "^" -> {-1, 0}};
visited = {pos};
intermediates = {};
Do[
  Which[
    d == "L", dir = RotateLeft[dir],
    d == "R", dir = RotateRight[dir],
    True,
    Do[
     {nextPos, tmpDir} = next3[pos, dir[[1]]];
     If[sparseMap2D[[Sequence @@ nextPos]] != 2, pos = nextPos, 
      Break[]];
     While[tmpDir =!= (dir[[1]] /. step), dir = RotateRight[dir]];
     AppendTo[visited, pos];
     , {s, d}];
    AppendTo[intermediates, 
     Total[{1000, 4, 1}*# &@
       Join[pos, {dir[[1]] /. {">" -> 0, "v" -> 1, "<" -> 2, 
           "^" -> 3}}]]]];
  , {d, directions[[ ;; ]]}];
Total[{1000, 4, 1}*# &@
  Join[pos, {dir[[1]] /. {">" -> 0, "v" -> 1, "<" -> 2, "^" -> 3}}]]
