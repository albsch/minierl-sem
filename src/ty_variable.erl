-module(ty_variable).
-vsn({2,0,0}).

% ETS table is used to strict monotonically increment a variable ID counter
-on_load(setup_ets/0).
-define(VAR_ETS, variable_counter_ets_table).

-behavior(eq).
-export([equal/2, compare/2]).

-behavior(var).
-export([new/1, smallest/3, normalize/6]).

-record(var, {id, name}).
-type var() :: #var{id :: integer(), name :: string()}.

-spec setup_ets() -> ok.
setup_ets() ->
  spawn(fun() ->
    % spawns a new process that is the owner of the variable id ETS table
    ets:new(?VAR_ETS, [public, named_table]),
    ets:insert(?VAR_ETS, {variable_id, 0}),
    receive _ -> ok end
        end),
  ok.

-spec equal(var(), var()) -> boolean().
equal(Var1, Var2) -> compare(Var1, Var2) =:= 0.

-spec compare(var(), var()) -> -1 | 0 | 1.
compare(#var{id = Id1}, #var{id = Id2}) when Id1 < Id2 -> -1;
compare(#var{id = Id1}, #var{id = Id2}) when Id1 > Id2 -> +1;
compare(_, _) -> 0.

leq(#var{id = Id1}, #var{id = Id2}) -> Id1 =< Id2.

-spec new(string()) -> var().
new(Name) ->
  NewId = ets:update_counter(?VAR_ETS, variable_id, {2,1}),
  #var{id = NewId, name = Name}.

% assumption: PVars U NVars is not empty
smallest(PositiveVariables, NegativeVariables, FixedVariables) ->
  % true = (length(PositiveVariables) + length(NegativeVariables)) > 0,

  % fixed variables are higher order than all non-fixed ones, will be picked last
  PositiveVariablesTagged = [{pos, V} || V <- PositiveVariables, not sets:is_element(V, FixedVariables)],
  NegativeVariablesTagged = [{neg, V} || V <- NegativeVariables, not sets:is_element(V, FixedVariables)],

  RestTaggedPos = [{deltapos, V} || V <- PositiveVariables, sets:is_element(V, FixedVariables)],
  RestTaggedNeg = [{deltaneg, V} || V <- NegativeVariables, sets:is_element(V, FixedVariables)],

  Sort = fun({_, V}, {_, V2}) -> leq(V, V2) end,
  [X | Z] = lists:sort(Sort, PositiveVariablesTagged++NegativeVariablesTagged)
    ++ lists:sort(Sort, RestTaggedPos)
    ++ lists:sort(Sort, RestTaggedNeg),

  {X, Z}.

% (NTLV rule)
normalize(Ty, PVar, NVar, Fixed, VarToTy, Mx) ->
  SmallestVar = ty_variable:smallest(PVar, NVar, Fixed),
  case SmallestVar of
    {{pos, Var}, _Others} ->
%%      io:format(user, "pvar var --~n~p~n~p~n~p~n", [PVar, [Var], (PVar -- [Var])]),
      true = (length(PVar) == length((PVar -- [Var])) + 1),
      AccP = lists:foldl(fun(Var, Acc) -> ty_rec:intersect(Acc, VarToTy(Var)) end, Ty, PVar -- [Var]),
      AccN = lists:foldl(fun(Var, Acc) -> ty_rec:union(Acc, VarToTy(Var)) end, ty_rec:empty(), NVar),
      S = ty_rec:diff(AccP, AccN),
%%      TyResult = lists:foldl(fun({_, V}, CTy) -> ty_rec:intersect(CTy, VarToTy(V)) end, Ty, Others),
%%      io:format(user, "Single out positive Variable:~n~p <= ~p~n", [Var, ty_rec:negate(S)]),
      [[{Var, ty_rec:empty(), ty_rec:negate(S)}]];
    {{neg, Var}, _Others} ->
      true = (length(NVar) == length((NVar -- [Var])) + 1),
      AccP = lists:foldl(fun(Var, Acc) -> ty_rec:intersect(Acc, VarToTy(Var)) end, Ty, PVar),
      AccN = lists:foldl(fun(Var, Acc) -> ty_rec:union(Acc, VarToTy(Var)) end, ty_rec:empty(), NVar -- [Var]),
      S = ty_rec:diff(AccP, AccN),
%%      io:format(user, "Single out negative Variable:~n~p <= ~p~n", [S, Var]),
%%      TyResult = lists:foldl(fun({_, V}, CTy) -> ty_rec:intersect(CTy, VarToTy(V)) end, Ty, Others),
      [[{Var, S, ty_rec:any()}]];
    {{delta, _}, _} ->
      error(todo),
      % io:format(user, "Normalize all fixed variables done! ~p~n", [Ty]),
      % part 1 paper Lemma C.3 and C.11 all fixed variables can be eliminated
      ty_rec:normalize(Ty, Fixed, Mx)
  end.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

usage_test() ->
  % create a fresh variable with a descriptor "A"
  _VarA = ty_variable:new("A"),
  ok.

strictly_increasing_id_test() ->
  #var{id = IdA} = ty_variable:new("A"),
  #var{id = IdB} = ty_variable:new("B"),
  #var{id = IdC} = ty_variable:new("C"),
  true = (IdA < IdB),
  true = (IdB < IdC),
  ok.

same_name_different_id_test() ->
  VarA = ty_variable:new("a"),
  VarB = ty_variable:new("a"),
  -1 = ty_variable:compare(VarA, VarB),
  ok.

-endif.
