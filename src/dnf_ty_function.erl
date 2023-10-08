-module(dnf_ty_function).
-vsn({2,0,0}).

-ifdef(TEST).
-export([normalize_no_vars/6]).
-export([explore_function_norm/5]).
-endif.

-define(P, {bdd_bool, ty_function}).
-define(F(Z), fun() -> Z end).

-behavior(eq).
-export([equal/2, compare/2]).

-behavior(type).
-export([empty/0, any/0, union/2, intersect/2, diff/2, negate/1]).
-export([is_empty/1, is_any/1, normalize/5, substitute/3]).

-export([function/1, all_variables/1, has_ref/2]).

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
any() -> gen_bdd:any(?P).

union(B1, B2) -> simplify(gen_bdd:union(?P, B1, B2)).
intersect(B1, B2) -> simplify(gen_bdd:intersect(?P, B1, B2)).
diff(B1, B2) -> simplify(gen_bdd:diff(?P, B1, B2)).
negate(B1) -> simplify(gen_bdd:negate(?P, B1)).

is_any(B) -> gen_bdd:is_any(?P, B).
is_empty(B) -> gen_bdd:is_empty(?P, B).

simplify(Bdd) ->
  IsEmpty = is_empty_full(Bdd),
  case IsEmpty of
    true -> empty();
    _ -> Bdd
  end.

equal(B1, B2) -> gen_bdd:equal(?P, B1, B2).
compare(B1, B2) -> gen_bdd:compare(?P, B1, B2).

is_empty_full(TyDnf) ->
  is_empty_full(
    TyDnf,
    ty_rec:empty(), [], []
  ).

is_empty_full({leaf, 0}, _, _, _) -> true;
% TODO should only be {terminal, 1}, not just 1!
%%is_empty(1, _, _, []) -> false;
is_empty_full({leaf, 1}, _, _, []) -> false;
is_empty_full({leaf, 1}, S, P, [Function | N]) ->
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
    is_empty_full({leaf, 1}, S, P, N)
  ;
is_empty_full({node, Function, L_BDD, R_BDD}, S, P, N) ->
  T1 = ty_function:domain(Function),
  is_empty_full(L_BDD, ty_rec:union(S, T1), [Function | P], N)
  andalso
    is_empty_full(R_BDD, S, P, [Function | N])
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
  Ty = ty_rec:function(dnf_var_ty_function:function(DnfTyFunction)),
  % ntlv rule
  ty_variable:normalize(Ty, PVar, NVar, Fixed, fun(Var) -> ty_rec:function(dnf_var_ty_function:var(Var)) end, M).


normalize_no_vars({leaf, 0}, _, _, _, _Fixed, _) -> [[]]; % empty
normalize_no_vars({leaf, 1}, _, _, [], _Fixed, _) -> []; % non-empty
normalize_no_vars({leaf, 1}, S, P, [Function | N], Fixed, M) ->
  T1 = ty_function:domain(Function),
  T2 = ty_function:codomain(Function),
  %% ∃ T1-->T2 ∈ N s.t.
  %%   T1 is in the domain of the function
  %%   S is the union of all domains of the positive function intersections
  X1 = ?F(ty_rec:normalize(ty_rec:intersect(T1, ty_rec:negate(S)), Fixed, M)),
  X2 = ?F(explore_function_norm(T1, ty_rec:negate(T2), P, Fixed, M)),
  R1 = ?F(constraint_set:meet(X1, X2)),
  %% Continue searching for another arrow ∈ N
  R2 = ?F(normalize_no_vars({leaf, 1}, S, P, N, Fixed, M)),
  constraint_set:join(R1, R2);
normalize_no_vars({node, Function, L_BDD, R_BDD}, S, P, Negated, Fixed, M) ->
  T1 = ty_function:domain(Function),

  constraint_set:meet(
    ?F(normalize_no_vars(L_BDD, ty_rec:union(S, T1), [Function | P], Negated, Fixed, M)),
    ?F(normalize_no_vars(R_BDD, S, P, [Function | Negated], Fixed, M))
  ).

explore_function_norm(T1, T2, [], Fixed, M) ->
  NT1 = ?F(ty_rec:normalize(T1, Fixed, M)),
  NT2 = ?F(ty_rec:normalize(T2, Fixed, M)),
  constraint_set:join( NT1, NT2 );
explore_function_norm(T1, T2, [Function | P], Fixed, M) ->
  NT1 = ?F(ty_rec:normalize(T1, Fixed, M)),
  NT2 = ?F(ty_rec:normalize(T2, Fixed, M)),

  S1 = ty_function:domain(Function),
  S2 = ty_function:codomain(Function),

  NS1 = ?F(explore_function_norm(T1, ty_rec:intersect(T2, S2), P, Fixed, M)),
  NS2 = ?F(explore_function_norm(ty_rec:diff(T1, S1), T2, P, Fixed, M)),

  constraint_set:join(NT1,
    ?F(constraint_set:join(NT2,
      ?F(constraint_set:meet(NS1, NS2))))).

substitute({leaf, 0}, _, _) -> {leaf, 0};
substitute({leaf, 1}, _, _) ->
  {leaf, 1};
substitute({node, TyFunction, L_BDD, R_BDD}, Map, Memo) ->
  S1 = ty_function:domain(TyFunction),
  S2 = ty_function:codomain(TyFunction),

  NewS1 = ty_rec:substitute(S1, Map, Memo),
  NewS2 = ty_rec:substitute(S2, Map, Memo),

  NewTyFunction = ty_function:function(NewS1, NewS2),

  union(
    intersect(function(NewTyFunction), L_BDD),
    intersect(negate(function(NewTyFunction)), R_BDD)
  ).

has_ref({leaf, _}, _) -> false;
has_ref({node, Function, PositiveEdge, NegativeEdge}, Ref) ->
  ty_function:has_ref(Function, Ref)
  orelse
  has_ref(PositiveEdge, Ref)
    orelse
    has_ref(NegativeEdge, Ref).

all_variables({leaf, _}) -> [];
all_variables({node, Function, PositiveEdge, NegativeEdge}) ->
  ty_rec:all_variables(ty_function:domain(Function))
    ++ ty_rec:all_variables(ty_function:codomain(Function))
    ++ all_variables(PositiveEdge)
    ++ all_variables(NegativeEdge).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_leaf_test() ->
  % (0, 0)
  Empty = empty(),
  true = is_empty(Empty),
  false = is_any(Empty),
  % (1, 1)
  Any = any(),
  false = is_empty(Any),
  true = is_any(Any),
  ok.

equiv_any_test() ->
  % (0 -> 1) == (leaf 1)
  AnyFunction = ty_function:function(ty_rec:empty(), ty_rec:any()),

  Any = any(),
  false = is_empty(Any),
  true = is_any(Any),

  AnyOther = function(AnyFunction),
  false = is_empty(AnyOther),
  true = is_any(AnyOther),
  Any = AnyOther, % same representation

  ok.

not_empty_test() ->
  % (N, N)
  TyInt = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
  Function = ty_function:function(TyInt, TyInt),
  false = ty_function:is_empty(Function),
  false = ty_function:is_any(Function),
  DnfFunction = function(Function),
  false = is_empty(DnfFunction),
  false = is_any(DnfFunction),

  % AnyT
  FunctionAny = any(),

  % union ==> any
  Other = union(DnfFunction, FunctionAny),
  false = is_empty(Other),
  true = is_any(Other),
  Other = FunctionAny,

  ok.

intersection_representation_test() ->
  TyInt = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
  TyI1 = ty_rec:interval(dnf_var_int:int(ty_interval:interval(1, 1))),
  TyI2 = ty_rec:interval(dnf_var_int:int(ty_interval:interval(2, 2))),

  T1 = function(ty_function:function(TyInt, ty_rec:any())),
  T2 = function(ty_function:function(ty_rec:any(), TyInt)),

  % intersect, not empty
  Bdd = intersect(T1, T2),
  false = is_empty(Bdd),
  false = is_any(Bdd),

  T3 = function(ty_function:function(ty_rec:any(), TyI1)),
  Bdd2 = intersect(Bdd, T3),
  false = is_empty(Bdd2),
  false = is_any(Bdd2),

  T4 = function(ty_function:function(ty_rec:any(), TyI2)),
  Bdd3 = intersect(Bdd2, T4),
  false = is_empty(Bdd3),
  false = is_any(Bdd3),

  ok.


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
