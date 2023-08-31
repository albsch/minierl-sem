-module(dnf_ty_tuple).
-vsn({1,3,2}).

-define(P, {bdd_bool, ty_tuple}).

-behavior(eq).
-export([equal/2, compare/2]).

-behavior(type).
-export([empty/0, any/0, union/2, intersect/2, diff/2, negate/1]).
-export([eval/1, is_empty/1, is_any/1, normalize/5, substitute/2]).

-export([tuple/1, all_variables/1, collect_variable_positions/2]).

-type dnf_tuple() :: term().
-type ty_tuple() :: dnf_tuple(). % ty_tuple:type()
-type dnf_ty_tuple() :: term().

-spec tuple(ty_tuple()) -> dnf_ty_tuple().
tuple(TyTuple) -> gen_bdd:element(?P, TyTuple).

% ==
% type interface
% ==
empty() -> gen_bdd:empty(?P).
any() -> gen_bdd:any(?P).

union(B1, B2) -> gen_bdd:union(?P, B1, B2).
intersect(B1, B2) -> gen_bdd:intersect(?P, B1, B2).
diff(B1, B2) -> gen_bdd:diff(?P, B1, B2).
negate(B1) -> gen_bdd:negate(?P, B1).

eval(B) -> gen_bdd:eval(?P, B).
is_any(B) -> gen_bdd:is_any(?P, B).

% ==
% basic interface
% ==

equal(B1, B2) -> gen_bdd:equal(?P, B1, B2).
compare(B1, B2) -> gen_bdd:compare(?P, B1, B2).

is_empty(TyBDD) -> is_empty(
  TyBDD,
  ty_rec:any(),
  ty_rec:any(),
  _NegatedTuples = []
).

is_empty(0, _, _, _) -> true;
is_empty({terminal, 1}, S1, S2, N) ->
  phi(S1, S2, N);
is_empty({node, TyTuple, L_BDD, R_BDD}, BigS1, BigS2, Negated) ->
  S1 = ty_tuple:pi1(TyTuple),
  S2 = ty_tuple:pi2(TyTuple),

  is_empty(L_BDD, ty_rec:intersect(S1, BigS1), ty_rec:intersect(S2, BigS2), Negated)
  andalso
    is_empty(R_BDD, BigS1, BigS2, [TyTuple | Negated]).

phi(S1, S2, []) ->
  ty_rec:is_empty(S1)
    orelse
    ty_rec:is_empty(S2);
phi(S1, S2, [Ty | N]) ->
  ty_rec:is_empty(S1)
    orelse ty_rec:is_empty(S2)
    orelse (
      begin
        T1 = ty_tuple:pi1(Ty),
        T2 = ty_tuple:pi2(Ty),
        phi(ty_rec:diff(S1, T1), S2, N)
          andalso
          phi(S1, ty_rec:diff(S2, T2), N)
      end
  ).

normalize(TyTuple, [], [], Fixed, M) ->
  % optimized NProd rule
  normalize_no_vars(TyTuple, ty_rec:any(), ty_rec:any(), _NegatedTuples = [], Fixed, M);
normalize(DnfTyTuple, PVar, NVar, Fixed, M) ->
  Ty = ty_rec:tuple(dnf_var_ty_tuple:tuple(DnfTyTuple)),
  % ntlv rule
  ty_variable:normalize(Ty, PVar, NVar, Fixed, fun(Var) -> ty_rec:tuple(dnf_var_ty_tuple:var(Var)) end, M).


normalize_no_vars(0, _, _, _, _Fixed, _) -> [[]]; % empty
normalize_no_vars({terminal, 1}, S1, S2, N, Fixed, M) ->
  phi_norm(S1, S2, N, Fixed, M);
normalize_no_vars({node, TyTuple, L_BDD, R_BDD}, BigS1, BigS2, Negated, Fixed, M) ->
  S1 = ty_tuple:pi1(TyTuple),
  S2 = ty_tuple:pi2(TyTuple),

  % TODO lazy
  constraint_set:merge_and_meet(
    normalize_no_vars(L_BDD, ty_rec:intersect(S1, BigS1), ty_rec:intersect(S2, BigS2), Negated, Fixed, M),
    normalize_no_vars(R_BDD, BigS1, BigS2, [TyTuple | Negated], Fixed, M)
  ).

phi_norm(S1, S2, [], Fixed, M) ->
  % TODO lazy
  T1 = ty_rec:normalize(S1, Fixed, M),
  T2 = ty_rec:normalize(S2, Fixed, M),
  constraint_set:merge_and_join(T1, T2);
phi_norm(S1, S2, [Ty | N], Fixed, M) ->
  T1 = ty_rec:normalize(S1, Fixed, M),
  T2 = ty_rec:normalize(S2, Fixed, M),

  T3 =
    begin
      TT1 = ty_tuple:pi1(Ty),
      TT2 = ty_tuple:pi2(Ty),
      X1 = phi_norm(ty_rec:diff(S1, TT1), S2, N, Fixed, M),
      X2 = phi_norm(S1, ty_rec:diff(S2, TT2), N, Fixed, M),
      constraint_set:merge_and_meet(X1, X2)
    end,

  % TODO lazy
  constraint_set:merge_and_join(T1, constraint_set:merge_and_join(T2, T3)).


substitute(0, _) -> 0;
substitute({terminal, 1}, _) ->
  {terminal, 1};
substitute({node, TyTuple, L_BDD, R_BDD}, Map) ->
  S1 = ty_tuple:pi1(TyTuple),
  S2 = ty_tuple:pi2(TyTuple),

  NewS1 = ty_rec:substitute(S1, Map),
  NewS2 = ty_rec:substitute(S2, Map),

  NewTyTuple = ty_tuple:tuple(NewS1, NewS2),

  union(
    intersect(tuple(NewTyTuple), L_BDD),
    intersect(negate(tuple(NewTyTuple)), R_BDD)
    ).

all_variables(0) -> [];
all_variables({terminal, _}) -> [];
all_variables({node, Tuple, PositiveEdge, NegativeEdge}) ->
  ty_rec:all_variables(ty_tuple:pi1(Tuple))
  ++ ty_rec:all_variables(ty_tuple:pi2(Tuple))
    ++ all_variables(PositiveEdge)
    ++ all_variables(NegativeEdge).

collect_variable_positions(0,_Current) -> #{};
collect_variable_positions({terminal, _}, _Current) -> #{};
collect_variable_positions({node, Tuple, PositiveEdge, NegativeEdge}, Current) ->
  T1 = ty_tuple:pi1(Tuple),
  T2 = ty_tuple:pi2(Tuple),

  VT1 = ty_rec:collect_variable_positions(T1, Current),
  VT2 = ty_rec:collect_variable_positions(T2, Current),

  Left = collect_variable_positions(PositiveEdge, Current),
  Right = collect_variable_positions(NegativeEdge, Current),

  ty_rec:merge_maps([Left, Right, VT1, VT2]).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

usage_test() ->
  %   (int, int) ^ (1, 2)
  TIa = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
  TIb = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
  TIc = ty_rec:interval(dnf_var_int:int(ty_interval:interval(1, 1))),
  TId = ty_rec:interval(dnf_var_int:int(ty_interval:interval(2, 2))),

  Ty_TupleA = ty_tuple:tuple(TIa, TIb),
  Ty_TupleB = ty_tuple:tuple(TIc, TId),

  B1 = dnf_ty_tuple:tuple(Ty_TupleA),
  B2 = dnf_ty_tuple:tuple(Ty_TupleB),

  Bdd = dnf_ty_tuple:intersect(B1, B2),

%%  io:format(user, "~p~n", [Bdd]),
  false = dnf_ty_tuple:is_empty(Bdd),

  ok.
-endif.
