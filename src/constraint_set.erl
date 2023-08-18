-module(constraint_set).

%% API
-export([merge_and_meet/2, merge_and_join/2]).

merge_and_meet([], _Set2) -> [];
merge_and_meet(_Set1, []) -> [];
merge_and_meet([[]], Set2) -> Set2;
merge_and_meet(Set1, [[]]) -> Set1;
merge_and_meet(La, Lb) ->
  R = lists:map(fun(E) -> unionlist(Lb, E) end, La),
  R2 = lists:foldl(fun(NewS, All) -> merge_and_join(NewS, All) end, [], R),
%%  sanity(R2),
  R2.

unionlist(L, A) -> lists:map(fun(E) -> nunion(A, E) end, L).


nunion([], L) -> L;
nunion(L, []) -> L;
nunion([{V1, T1, T2} | C1], [{V2, S1, S2} | C2]) when V1 == V2 ->
  Lower = ty_rec:union(T1, S1),
  Upper = ty_rec:intersect(T2, S2),

  [{V1, Lower, Upper}] ++ nunion(C1, C2);
nunion([Z = {V1, _, _} | C1], S = [{V2, _, _} | _C2]) when V1 < V2 ->
  [Z] ++ nunion(C1, S);
nunion(S = [{_, _, _} | _C1], [Z = {_, _, _} | C2]) ->
  [Z] ++ nunion(C2, S).




merge_and_join([[]], _Set2) -> [[]];
merge_and_join(_Set1, [[]]) -> [[]];
merge_and_join([], Set2) -> Set2;
merge_and_join(Set1, []) -> Set1;
merge_and_join(S1, S2) ->
  MayAdd = fun (S, Con) -> (not (has_smaller_constraint(Con, S))) end,
  S22 = lists:filter(fun(C) -> MayAdd(S1, C) end, S2),
  S11 = lists:filter(fun(C) -> MayAdd(S22, C) end, S1),
  lists:map(fun lists:usort/1, lists:usort(S11 ++ S22)).


has_smaller_constraint(_Con, []) -> false;
has_smaller_constraint(Con, [C | S]) ->
  case is_smaller(C, Con) of
    true -> true;
    _ -> has_smaller_constraint(Con, S)
  end.

% C1 and C2 are sorted by variable order
is_smaller([], _C2) -> true;
is_smaller(_C1, []) -> false;
is_smaller([{V1, T1, T2} | C1], [{V2, S1, S2} | C2]) when V1 == V2 ->
  case ty_rec:is_subtype(T1, S1) andalso ty_rec:is_subtype(S2, T2) of
    true -> is_smaller(C1, C2);
    _ -> false
  end;
is_smaller([{V1, _, _} | _C1], [{V2, _, _} | _C2]) when V1 < V2 ->
  % V1 is not in the other set
  % not smaller
  false;
is_smaller(C1, [{_V2, _, _} | C2]) ->
  is_smaller(C1, C2).
