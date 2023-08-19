-module(dnf_var_ty_function).
-vsn({1,3,0}).

-define(P, {dnf_ty_function, ty_variable}).

-behavior(eq).
-export([equal/2, compare/2]).

-behavior(type).
-export([empty/0, any/0, union/2, intersect/2, diff/2, negate/1]).
-export([eval/1, is_empty/1, is_any/1, normalize/3]).

-export([var/1, function/1]).

-type dnf_function() :: term().
-type ty_function() :: dnf_function(). % ty_function:type()
-type variable() :: term(). % variable:type()
-type dnf_var_function() :: term().

-spec function(ty_function()) -> dnf_var_function().
function(Tuple) -> gen_bdd:terminal(?P, Tuple).

-spec var(variable()) -> dnf_var_function().
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
is_empty({terminal, Function}) ->
  dnf_ty_function:is_empty(Function);
is_empty({node, _Variable, PositiveEdge, NegativeEdge}) ->
  is_empty(PositiveEdge)
    and is_empty(NegativeEdge).

normalize(Ty, Fixed, M) -> normalize(Ty, [], [], Fixed, M).

normalize(0, _, _, _, _) -> [[]]; % satisfiable
normalize({terminal, Function}, PVar, NVar, Fixed, M) ->
  case ty_ref:is_normalized_memoized(Function, Fixed, M) of
    true ->
      io:format(user, "Memoized:~n~p", [Function]),
      error(todo); %[[]];
    miss ->
      % memoize only non-variable component t0
      dnf_ty_function:normalize(Function, PVar, NVar, Fixed, sets:union(M, sets:from_list([Function])))
  end;
normalize({node, Variable, PositiveEdge, NegativeEdge}, PVar, NVar, Fixed, M) ->
  constraint_set:merge_and_meet(
    normalize(PositiveEdge, [Variable | PVar], NVar, Fixed, M),
    normalize(NegativeEdge, PVar, [Variable | NVar], Fixed, M)
  ).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%usage_test() ->
%%  %   a1 ^ (int -> int)
%%  TIa = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
%%  TIb = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
%%  Ty_Function = ty_function:function(TIa, TIb),
%%
%%  VarA = ty_variable:new("a1"),
%%
%%  Dnf_Ty_Function = dnf_ty_function:function(Ty_Function),
%%
%%  BVar1 = dnf_var_ty_function:var(VarA),
%%  BFunctionA = dnf_var_ty_function:function(Dnf_Ty_Function),
%%
%%  Bdd = dnf_var_ty_function:intersect(BVar1, BFunctionA),
%%
%%  false = dnf_var_ty_function:is_empty(Bdd),
%%%%  io:format(user, "~p~n", [Bdd]),
%%
%%  ok.

normalize_test() ->
  %   a -> atom ^ ~(b -> b)
  Alpha = ty_variable:new("Alpha"),
  Beta = ty_variable:new("Beta"),
  TyAtom = ty_rec:atom(dnf_var_ty_atom:any()),
  TyAlpha = ty_rec:variable(Alpha),
  TyBeta = ty_rec:variable(Beta),

  T1 = dnf_var_ty_function:function(dnf_ty_function:function(ty_function:function(TyAlpha, TyAtom))),
  T2 = dnf_var_ty_function:function(dnf_ty_function:function(ty_function:function(TyBeta, TyBeta))),
  ToNormalize = dnf_var_ty_function:diff(T1, T2),


  io:format(user, "to normalize ~p~n", [ToNormalize]),

%%  Res = dnf_var_ty_function:normalize(ToNormalize, sets:new(), sets:new()),

%%  io:format(user, "~n~nDONE ~n~p~n", [Res]),

  ok.

-endif.
