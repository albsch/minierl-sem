-module(dnf_ty_function).
-vsn({1,3,1}).

-ifdef(TEST).
-export([normalize_no_vars/6]).
-export([explore_function_norm/5]).
-export([explore_function_norm_v0/7]).
-endif.

-define(P, {bdd_bool, ty_function}).

-behavior(eq).
-export([equal/2, compare/2]).

-behavior(type).
-export([empty/0, any/0, union/2, intersect/2, diff/2, negate/1]).
-export([eval/1, is_empty/1, is_any/1, normalize/5]).

-export([function/1]).

-type ty_ref() :: {ty_ref, integer()}.
-type dnf_function() :: term().
-type ty_function() :: dnf_function(). % ty_function:type()
-type dnf_ty_function() :: term().

-spec function(ty_function()) -> dnf_ty_function().
function(TyFunction) -> gen_bdd:element(?P, TyFunction).

% ==
% type interface
% ==
empty() -> gen_bdd:empty(?P).
any() ->
  gen_bdd:any(?P).

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

is_empty(TyDnf) ->
  is_empty(
    TyDnf,
    ty_rec:empty(), [], []
  ).

is_empty(0, _, _, _) -> true;
is_empty({terminal, 1}, _, _, []) -> false;
is_empty({terminal, 1}, S, P, [Function | N]) ->
  T1 = ty_function:domain(Function),
  T2 = ty_function:codomain(Function),
  (
  %% ∃ T1-->T2 ∈ N s.t.
  %%    T1 is in the domain of the function
  %%    S is the union of all domains of the positive function intersections
  ty_rec:is_subtype(T1, S)
    andalso
    explore_function(T1, ty_rec:negate(T2), P)
  )
  %% Continue searching for another arrow ∈ N
    orelse
    is_empty({terminal, 1}, S, P, N)
  ;
is_empty({node, Function, L_BDD, R_BDD}, S, P, N) ->
  T1 = ty_function:domain(Function),
  is_empty(L_BDD, ty_rec:union(S, T1), [Function | P], N)
  andalso
    is_empty(R_BDD, S, P, [Function | N])
.

% optimized phi' (4.10) from paper covariance and contravariance
% justification for this version of phi can be found in `prop_phi_function.erl`
-spec explore_function(ty_ref(), ty_ref(), [term()]) -> boolean().
explore_function(T1, T2, []) ->
  ty_rec:is_empty(T2) orelse ty_rec:is_empty(T1);
explore_function(T1, T2, [Function | P]) ->
  ty_rec:is_empty(T1) orelse ty_rec:is_empty(T2)
  orelse
    begin
      S1 = ty_function:domain(Function),
      S2 = ty_function:codomain(Function),
      explore_function(T1, ty_rec:intersect(T2, S2), P)
        andalso
        explore_function(ty_rec:diff(T1, S1), T2, P)
    end.

normalize(TyFunction, [], [], Fixed, M) ->
  % optimized NArrow rule
  normalize_no_vars(TyFunction, ty_rec:empty(), [], [], Fixed, M);
normalize(DnfTyFunction, PVar, NVar, Fixed, M) ->
  Ty = ty_rec:function(DnfTyFunction),
  % ntlv rule
  ty_variable:normalize(Ty, PVar, NVar, Fixed, fun(Var) -> ty_rec:function(dnf_var_ty_function:var(Var)) end, M).


normalize_no_vars(0, _, _, _, _Fixed, _) -> [[]]; % empty
normalize_no_vars({terminal, 1}, _, _, [], _Fixed, _) -> []; % non-empty
normalize_no_vars({terminal, 1}, S, P, [Function | N], Fixed, M) ->
  R1 = begin
    T1 = ty_function:domain(Function),
    T2 = ty_function:codomain(Function),
    %% ∃ T1-->T2 ∈ N s.t.
    %%   T1 is in the domain of the function
    %%   S is the union of all domains of the positive function intersections
    X1 = ty_rec:normalize(ty_rec:intersect(T1, ty_rec:negate(S)), Fixed, M),
    X2 = case persistent_term:get(phi_norm_fun, v) of
           v -> explore_function_norm(T1, ty_rec:negate(T2), P, Fixed, M);
           v0 -> explore_function_norm_v0(T1, T2, P, ty_rec:empty(), ty_rec:any(), Fixed, M)
    end,
    constraint_set:merge_and_meet(X1, X2)
  end,
  %% Continue searching for another arrow ∈ N
  R2 = normalize_no_vars({terminal, 1}, S, P, N, Fixed, M),
  constraint_set:merge_and_join(R1, R2)
;
normalize_no_vars({node, Function, L_BDD, R_BDD}, S, P, Negated, Fixed, M) ->
  T1 = ty_function:domain(Function),

  % TODO lazy
  constraint_set:merge_and_meet(
    normalize_no_vars(L_BDD, ty_rec:union(S, T1), [Function | P], Negated, Fixed, M),
    normalize_no_vars(R_BDD, S, P, [Function | Negated], Fixed, M)
  ).


explore_function_norm_v0(T1, T2, [], D , C, Fix, M) ->
  % (T1 <: D) or (C <: T2)
  constraint_set:join(
    ty_rec:normalize(ty_rec:diff(T1, D), Fix, M),
    ty_rec:normalize(ty_rec:diff(C, T2), Fix, M)
  );
% phi(T1, T2, P U {S1 --> S2}, D, C)
explore_function_norm_v0(T1, T2, [Function | P], D, C, Fix, M) ->
  S1 = ty_function:domain(Function),
  S2 = ty_function:codomain(Function),
  % phi(T1, T2, P, D, C&S2)
  constraint_set:meet(
  explore_function_norm_v0(T1, T2, P, D, ty_rec:intersect(C, S2), Fix, M)
    , % and
  % phi(T1, T2, P, D|S1, C)
  explore_function_norm_v0(T1, T2, P, ty_rec:union(D, S1), C, Fix, M)
  ).

explore_function_norm(T1, T2, [], Fixed, M) ->
  NT1 = ty_rec:normalize(T1, Fixed, M),
  NT2 = ty_rec:normalize(T2, Fixed, M),

  % TODO lazy
  constraint_set:merge_and_join(
    NT1, NT2
  );
explore_function_norm(T1, T2, [Function | P], Fixed, M) ->
  % TODO lazy
  NT1 = ty_rec:normalize(T1, Fixed, M),
  NT2 = ty_rec:normalize(T2, Fixed, M),

  NT3 =
    begin
      S1 = ty_function:domain(Function),
      S2 = ty_function:codomain(Function),

      NS1 = explore_function_norm(T1, ty_rec:intersect(T2, S2), P, Fixed, M),
      NS2 = explore_function_norm(ty_rec:diff(T1, S1), T2, P, Fixed, M),

      constraint_set:merge_and_meet(NS1, NS2)
    end,

  constraint_set:merge_and_join(NT1, constraint_set:merge_and_join(NT2, NT3)).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%usage_test() ->
%%  %   (int -> int) ^ (1 -> 2)
%%  TIa = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
%%  TIb = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
%%  TIc = ty_rec:interval(dnf_var_int:int(ty_interval:interval(1, 1))),
%%  TId = ty_rec:interval(dnf_var_int:int(ty_interval:interval(2, 2))),
%%
%%  Ty_FunctionA = ty_function:function(TIa, TIb),
%%  Ty_FunctionB = ty_function:function(TIc, TId),
%%
%%  B1 = dnf_ty_function:function(Ty_FunctionA),
%%  B2 = dnf_ty_function:function(Ty_FunctionB),
%%
%%  Bdd = dnf_ty_function:intersect(B1, B2),
%%
%%  false = dnf_ty_function:is_empty(Bdd),
%%%%  io:format(user, "~p~n", [Bdd]),
%%
%%  ok.
%%
normalize_test_() ->
  {timeout, 3000,
    fun() ->
      %   a, a -> atom, b -> b
      Alpha = ty_variable:new("Alpha"), TyAlpha = ty_rec:variable(Alpha),
      Beta = ty_variable:new("Beta"), TyBeta = ty_rec:variable(Beta),
      TyAtom = ty_rec:atom(dnf_var_ty_atom:any()),

      Normalized = dnf_ty_function:normalize_no_vars({terminal, 1}, TyAlpha,
        [ty_function:function(TyAlpha, TyAtom)],
        [ty_function:function(TyBeta, TyBeta)],
        sets:new(), sets:new()
      ),

      io:format(user, "~n~nDONE ~n~p~n", [Normalized]),

      ok
    end
  }.

%%normalize2_test_() ->
%%  {timeout, 3000,
%%    fun() ->
%%      %   norm(b, ~b ^ N, []) == { {(b <= 0)} {(N <= b)} }
%%      Alpha = ty_variable:new("Alpha"),
%%      Beta = ty_variable:new("Beta"), TyBeta = ty_rec:variable(Beta),
%%      N = ty_rec:atom(),
%%
%%      T1 = TyBeta,
%%      T2 = ty_rec:intersect(ty_rec:negate(TyBeta), N),
%%      Res = explore_function_norm(T1, T2, [], sets:new()),
%%
%%      io:format(user, "Done ~p~n", [Res]),
%%
%%      % TODO check via equivalence instead of syntactically
%%      Res = [[{Beta, ty_rec:empty(), ty_rec:empty()}], [{Beta, N, ty_rec:any()}]],
%%
%%      ok
%%    end
%%  }.
-endif.
