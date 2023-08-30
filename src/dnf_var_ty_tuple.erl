-module(dnf_var_ty_tuple).
-vsn({1,3,0}).

-define(P, {dnf_ty_tuple, ty_variable}).

-behavior(eq).
-export([equal/2, compare/2]).

-behavior(type).
-export([empty/0, any/0, union/2, intersect/2, diff/2, negate/1]).
-export([eval/1, is_empty/1, is_any/1, normalize/3, substitute/2]).

-export([var/1, tuple/1, clean_type/3, all_variables/1]).

-type dnf_tuple() :: term().
-type ty_tuple() :: dnf_tuple(). % ty_tuple:type()
-type variable() :: term(). % variable:type()
-type dnf_var_tuple() :: term().

-spec tuple(ty_tuple()) -> dnf_var_tuple().
tuple(Tuple) -> gen_bdd:terminal(?P, Tuple).

-spec var(variable()) -> dnf_var_tuple().
var(Var) -> gen_bdd:element(?P, Var).

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


is_empty(0) -> true;
is_empty({terminal, Tuple}) ->
  dnf_ty_tuple:is_empty(Tuple);
is_empty({node, _Variable, PositiveEdge, NegativeEdge}) ->
  is_empty(PositiveEdge)
    and is_empty(NegativeEdge).

normalize(Ty, Fixed, M) -> normalize(Ty, [], [], Fixed, M).

normalize(0, _, _, _, _) -> [[]]; % satisfiable
normalize({terminal, Tuple}, PVar, NVar, Fixed, M) ->
  case ty_ref:is_normalized_memoized(Tuple, Fixed, M) of
    true -> error(todo); % [[]];
    miss ->
      % memoize only non-variable component t0
      dnf_ty_tuple:normalize(Tuple, PVar, NVar, Fixed, sets:union(M, sets:from_list([Tuple])))
  end;
normalize({node, Variable, PositiveEdge, NegativeEdge}, PVar, NVar, Fixed, M) ->
  constraint_set:merge_and_meet(
    normalize(PositiveEdge, [Variable | PVar], NVar, Fixed, M),
    normalize(NegativeEdge, PVar, [Variable | NVar], Fixed, M)
  ).


substitute(T, M) -> substitute(T, M, [], []).

substitute(0, _, _, _) -> 0;
substitute({terminal, Tuple}, Map, Pos, Neg) ->
  AllPos = lists:map(
    fun(Var) ->
      Substitution = maps:get(Var, Map, ty_rec:variable(Var)),
      ty_rec:pi(tuple, Substitution)
    end, Pos),
  AllNeg = lists:map(
    fun(Var) ->
      Substitution = maps:get(Var, Map, ty_rec:variable(Var)),
      NewNeg = ty_rec:negate(Substitution),
      ty_rec:pi(tuple, NewNeg)
    end, Neg),

  lists:foldl(fun(Current, All) -> intersect(Current, All) end, tuple(dnf_ty_tuple:substitute(Tuple, Map)), AllPos ++ AllNeg);

substitute({node, Variable, PositiveEdge, NegativeEdge}, Map, P, N) ->

  LBdd = substitute(PositiveEdge, Map, [Variable | P], N),
  RBdd = substitute(NegativeEdge, Map, P, [Variable | N]),

  union(LBdd, RBdd).


clean_type(0, _, _) -> 0;
clean_type({terminal, Tuple}, Fixed, Position) ->
  % done
  {terminal, dnf_ty_tuple:clean_type(Tuple, Fixed, Position)};
clean_type({node, Variable, PositiveEdge, NegativeEdge}, FixedVariables, Position) ->

  Left = clean_type(PositiveEdge, FixedVariables, Position),
  Right = clean_type(NegativeEdge, FixedVariables, Position),

  % if variable not in fixed => clean
  case sets:is_element(Variable, FixedVariables) of
    true ->
      VarBdd = dnf_var_ty_tuple:var(Variable),
      union(intersect(VarBdd, Left), intersect(negate(VarBdd), Right));
    _ -> % if not fixed -> must be tally (otherwise would be normalized and substituted)
      % TODO remove sanity check
%%      {_, _, "tally_fresh"} = Variable,
      Ty = for_position(Position),
      union(intersect(Ty, Left), intersect(negate(Ty), Right))
  end.

for_position(covariant) -> empty();
for_position(contravariant) -> any().

all_variables(0) -> [];
all_variables({terminal, _}) -> [];
all_variables({node, Variable, PositiveEdge, NegativeEdge}) ->
  [Variable] ++ all_variables(PositiveEdge) ++ all_variables(NegativeEdge).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

usage_test() ->
  %   a1 ^ (int, int)
  TIa = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
  TIb = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
  Ty_Tuple = ty_tuple:tuple(TIa, TIb),

  VarA = ty_variable:new("a1"),

  Dnf_Ty_Tuple = dnf_ty_tuple:tuple(Ty_Tuple),

  BVar1 = dnf_var_ty_tuple:var(VarA),
  BTupleA = dnf_var_ty_tuple:tuple(Dnf_Ty_Tuple),

  Bdd = dnf_var_ty_tuple:intersect(BVar1, BTupleA),

  false = dnf_var_int:is_empty(Bdd),
%%  io:format(user, "~p~n", [Bdd]),

  ok.

-endif.
