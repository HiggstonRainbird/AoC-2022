(* ::Package:: *)

(* ::Text:: *)
(*Written December 16th, 2022.*)

(*Import*)

day = 16;
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
  StringSplit[#, {"=", ";", ",", " "}] & /@ 
   StringSplit[Import[inputPath], {"\n"}]];
valves = Table[i[[2]] -> {"Flow" -> i[[6]], Select[i[[12 ;;]], # != "" &]}, {i, input}];
Export[
  FileNameJoin[{NotebookDirectory[], "Day" <> ToString[day] <> "TrimmedInput.txt"}],
  StringJoin[Riffle[ToString /@ Join[#[[{2, 6}]], Select[#[[12 ;;]], # != "" &]], " "]] & /@ input
  ];

(*Setup*)

allValves = Union[valves[[;; , 1]]];
allNonzeros = Union[Select[valves, #[[2, 1, 2]] != 0 &][[;; , 1]]];
ClearAll@masks; Do[
 masks[allValves[[i]]] = 2^(i - 1), {i, Length[allValves]}];

max = Total[masks /@ allValves];
maxNonzero = Total[masks /@ allNonzeros];

ClearAll@flows;
flows[n_] := Total[flows /@ NumberExpand[n, 2]];
flows[0] = 0;
Do[flows[masks[v]] = (v /. valves)[[1, 2]], {v, allValves}];

ClearAll@connections; 
Do[
  connections[2^(i - 1)] = (allValves[[i]] /. valves)[[2]],
 {i, Length[allValves]}];


stay[{hist_, curr_, opening_, flow_}] :=
  {{BitOr[hist, curr],
    curr,
    True,
    flow + flows[hist]}};
go[{hist_, curr_, opening_, flow_}] :=
  Table[
   {hist,
    v,
    False,
    flow + flows[hist]},
   {v, masks /@ connections[curr]}];
nothing[{hist_, curr_, opening_, flow_}] := {{hist, {}, {}, flow + flows[hist]}};
nextPath[path_] :=
 Which[
  path[[1]] === maxNonzero, nothing[path],
  path[[3]] === True, go[path],
  path[[3]] === False && 
     (BitAnd[path[[1]], path[[2]]] != 0 || 
     flows[path[[2]]] == 0),
  go[path],
  
  path[[3]] === False, Join[go[path], stay[path]],
  
  True, go[path]
  ]

maxGather[paths_List] := 
  Join[#[[1, 1 ;; 3]], {Max[#[[;; , -1]]]}] & /@ 
   GatherBy[paths, #[[1 ;; 3]] &];
histGather[paths_List] :=
 Module[{bestHist},
  bestHist[curr_, opening_] := {};
  Reap[
    Do[
     If[p[[1]] === maxNonzero, Sow[p]; bestHist[{{}, {}}] = p; 
      Continue[]];
     If[
      ! AnyTrue[
        bestHist[p[[2]], p[[3]]],
        BitAnd[#, p[[1]]] == p[[1]] && p[[1]] < # &],
      bestHist[p[[2]], p[[3]]] = 
       Join[bestHist[p[[2]], p[[3]]], {p[[1]]}];
      Sow[p]
      ];
     , {p, SortBy[paths, {-DigitCount[#[[1]], 2, 1], #[[4]]} &]}]
    ][[2, 1]]
  ]

(*Parts 1 & 2*)

paths = {{0, masks["AA"], False, 0}};
t = AbsoluteTime[];
Do[
  paths = Flatten[Table[nextPath[path], {path, paths}], 1];
  
  paths = DeleteDuplicates[paths];
  paths = maxGather[paths];
  paths = histGather[paths]; (* Comment out for sample input. *)
  
  globalWatch = {n, Length[paths]};
  
  If[n == 26,
   possibilities = 
    Sort[#[[1, 1]] -> Max[#[[;; , 4]]] & /@ GatherBy[paths, #[[1]] &]];
   part2 = 
    Max[Total[#[[;; , 2]]] & /@ 
      Select[Tuples[{possibilities, possibilities}], 
       BitAnd[#[[1, 1]], #[[2, 1]]] == 0 &]];
   ];
  Print[{n, Length[paths], Max[paths[[;; , -1]]], AbsoluteTime[] - t}];
  , {n, 30}];
part1 = Max[paths[[;; , -1]]];
{part1, part2, AbsoluteTime[] - t}
