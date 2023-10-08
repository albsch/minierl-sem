-module(dnf_ty_tuple).

-define(P, {bdd_bool, ty_tuple}).
-define(F(Z), fun() -> Z end).

-export([equal/2, compare/2]).
-export([empty/0, any/0, union/2, intersect/2, diff/2, negate/1]).
-export([is_empty/1, is_empty_full/1, is_any/1, normalize/5, substitute/3]).
-export([tuple/1, all_variables/1, has_ref/2]).

tuple(TyTuple) -> gen_bdd:element(?P, TyTuple).
empty() -> gen_bdd:empty(?P).
any() -> gen_bdd:any(?P).

union(B1, B2) -> simplify(gen_bdd:union(?P, B1, B2)).
intersect(B1, B2) -> simplify(gen_bdd:intersect(?P, B1, B2)).
diff(B1, B2) -> simplify(gen_bdd:diff(?P, B1, B2)).
negate(B1) -> simplify(gen_bdd:negate(?P, B1)).

% ==
% DNF Tuple simplification
% * tuple are covariant in both type parameters
%   => there should be only one positive tuple in a BDD for each coclause
%   => intersect all positive tuples into one tuple
%   => check expensive invariant of emptiness and anyness, in case simplification is not enough
% ==
% R1: (t1, t2) & (t3, t4) = (t1 & t3, t2 & t4)
% R2: is_empty(Bdd) => empty()
% R3: is_any(Bdd) => any()
simplify(Bdd) ->
  % R1
  Bdd1 = gen_bdd:dnf(?P, Bdd, {fun simplify_coclause/3, fun combine_results/2}),

  % R2 & R3
  % check any  Any <= Bdd <-> Any & !Bdd empty
  IsAny = is_any_full(Bdd1),
  case IsAny of
    true -> any();
    _ ->
      IsEmpty = is_empty_full(Bdd1),
      case IsEmpty of
        true -> empty();
        _ -> Bdd1
      end
  end.

is_empty_coclause(_Pos, _Neg, 0) -> true;
is_empty_coclause([], Neg, 1) -> is_empty_coclause([ty_tuple:any()], Neg, 1);
is_empty_coclause(Pos, Neg, 1) ->
  % invariant: Pos is single tuple (simplification step required)
  [Tuple] = Pos,
  S1 = ty_tuple:pi1(Tuple),
  S2 = ty_tuple:pi2(Tuple),
  ty_rec:is_empty(S1) orelse
    ty_rec:is_empty(S2) orelse
    phi(S1, S2, Neg).

is_empty_union(F1, F2) -> F1() andalso F2().

simplify_coclause(Positive, Negative, 1) ->
  BigTuple = ty_tuple:big_intersect(Positive),
  case ty_tuple:is_empty(BigTuple) of
    true -> empty();
    false ->
      lists:foldl(fun(Neg, AccBdd) ->
        NBdd = gen_bdd:negate(?P, dnf_ty_tuple:tuple(Neg)),
        gen_bdd:intersect(?P, NBdd, AccBdd)
                  end, dnf_ty_tuple:tuple(BigTuple), Negative)
  end;
simplify_coclause(_Positive, _Negative, 0) ->
  gen_bdd:empty(?P).

combine_results(Fun1, Fun2) -> gen_bdd:union(?P, Fun1(), Fun2()).


% ==
% basic interface
% ==

equal(B1, B2) -> gen_bdd:equal(?P, B1, B2).
compare(B1, B2) -> gen_bdd:compare(?P, B1, B2).

% constant is_any check
is_any(B) -> gen_bdd:is_any(?P, B).
is_any_full(B) -> is_empty_full(gen_bdd:negate(?P, B)).
% constant is_empty check & full tuple exploration
is_empty(TyBDD) -> gen_bdd:is_empty(?P, TyBDD).
is_empty_full(TyBDD) -> gen_bdd:dnf(?P, TyBDD, {fun is_empty_coclause/3, fun is_empty_union/2}).

phi(_S1, _S2, []) -> false;
phi(S1, S2, [Ty | N]) ->
  begin
    T1 = ty_tuple:pi1(Ty),
    S1T1 = ty_rec:diff(S1, T1),
    (ty_rec:is_empty(S1T1) orelse phi(S1T1, S2, N))
  end
    andalso
  begin
    T2 = ty_tuple:pi2(Ty),
    S2T2 = ty_rec:diff(S2, T2),
    (ty_rec:is_empty(S2T2) orelse phi(S1, S2T2, N))
  end.


normalize(TyTuple, [], [], Fixed, M) ->
  % optimized NProd rule
  normalize_no_vars(TyTuple, ty_rec:any(), ty_rec:any(), _NegatedTuples = [], Fixed, M);
normalize(DnfTyTuple, PVar, NVar, Fixed, M) ->
  Ty = ty_rec:tuple(dnf_var_ty_tuple:tuple(DnfTyTuple)),
  % ntlv rule
  ty_variable:normalize(Ty, PVar, NVar, Fixed, fun(Var) -> ty_rec:tuple(dnf_var_ty_tuple:var(Var)) end, M).

normalize_no_vars({leaf, 0}, _, _, _, _Fixed, _) -> [[]]; % empty
normalize_no_vars({leaf, 1}, S1, S2, N, Fixed, M) ->
  phi_norm(S1, S2, N, Fixed, M);
normalize_no_vars({node, TyTuple, L_BDD, R_BDD}, BigS1, BigS2, Negated, Fixed, M) ->
  S1 = ty_tuple:pi1(TyTuple),
  S2 = ty_tuple:pi2(TyTuple),

  X1 = ?F(normalize_no_vars(L_BDD, ty_rec:intersect(S1, BigS1), ty_rec:intersect(S2, BigS2), Negated, Fixed, M)),
  X2 = ?F(normalize_no_vars(R_BDD, BigS1, BigS2, [TyTuple | Negated], Fixed, M)),
  constraint_set:meet(X1, X2).

phi_norm(S1, S2, [], Fixed, M) ->
  T1 = ?F(ty_rec:normalize(S1, Fixed, M)),
  T2 = ?F(ty_rec:normalize(S2, Fixed, M)),
  constraint_set:join(T1, T2);
phi_norm(S1, S2, [Ty | N], Fixed, M) ->
  T1 = ?F(ty_rec:normalize(S1, Fixed, M)),
  T2 = ?F(ty_rec:normalize(S2, Fixed, M)),

  T3 =
    ?F(begin
      TT1 = ty_tuple:pi1(Ty),
      TT2 = ty_tuple:pi2(Ty),
      X1 = ?F(phi_norm(ty_rec:diff(S1, TT1), S2, N, Fixed, M)),
      X2 = ?F(phi_norm(S1, ty_rec:diff(S2, TT2), N, Fixed, M)),
      constraint_set:meet(X1, X2)
    end),

  constraint_set:join(T1, ?F(constraint_set:join(T2, T3))).


substitute({leaf, 0}, _, _) -> {leaf, 0};
substitute({leaf, 1}, _, _) ->
  {leaf, 1};
substitute({node, TyTuple, L_BDD, R_BDD}, Map, Memo) ->
  S1 = ty_tuple:pi1(TyTuple),
  S2 = ty_tuple:pi2(TyTuple),

  NewS1 = ty_rec:substitute(S1, Map, Memo),
  NewS2 = ty_rec:substitute(S2, Map, Memo),

  NewTyTuple = ty_tuple:tuple(NewS1, NewS2),

  union(
    intersect(tuple(NewTyTuple), L_BDD),
    intersect(negate(tuple(NewTyTuple)), R_BDD)
    ).

has_ref({leaf, _}, _) -> false;
has_ref({node, Tuple, PositiveEdge, NegativeEdge}, Ref) ->
  ty_tuple:has_ref(Tuple, Ref)
    orelse
    has_ref(PositiveEdge, Ref)
    orelse
    has_ref(NegativeEdge, Ref).

all_variables({leaf, _}) -> [];
all_variables({node, Tuple, PositiveEdge, NegativeEdge}) ->
  ty_rec:all_variables(ty_tuple:pi1(Tuple))
  ++ ty_rec:all_variables(ty_tuple:pi2(Tuple))
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
  % (1, 1) == (leaf 1)
  AnyTuple = ty_tuple:tuple(ty_rec:any(), ty_rec:any()),

  Any = any(),
  false = is_empty(Any),
  true = is_any(Any),

  AnyOther = tuple(AnyTuple),
  false = is_empty(AnyOther),
  true = is_any(AnyOther),
  Any = AnyOther, % same representation

  ok.

not_empty_test() ->
  % (N, N)
  TyInt = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
  Tuple = ty_tuple:tuple(TyInt, TyInt),
  false = ty_tuple:is_empty(Tuple),
  false = ty_tuple:is_any(Tuple),
  DnfTuple = tuple(Tuple),
  false = is_empty(DnfTuple),
  false = is_any(DnfTuple),

  % AnyT
  TupleAny = any(),

  % union ==> any
  Other = union(DnfTuple, TupleAny),
  false = is_empty(Other),
  true = is_any(Other),
  Other = TupleAny,

  ok.

intersection_representation_test() ->
  TyInt = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
  TyI1 = ty_rec:interval(dnf_var_int:int(ty_interval:interval(1, 1))),
  TyI2 = ty_rec:interval(dnf_var_int:int(ty_interval:interval(2, 2))),

  T1 = tuple(ty_tuple:tuple(TyInt, ty_rec:any())),
  T2 = tuple(ty_tuple:tuple(ty_rec:any(), TyInt)),

  % intersect until empty
  Bdd = intersect(T1, T2),
  false = is_empty(Bdd),
  false = is_any(Bdd),

  T3 = tuple(ty_tuple:tuple(ty_rec:any(), TyI1)),
  Bdd2 = intersect(Bdd, T3),
  false = is_empty(Bdd2),
  false = is_any(Bdd2),

  T4 = tuple(ty_tuple:tuple(ty_rec:any(), TyI2)),
  Bdd3 = intersect(Bdd2, T4),
  true = is_empty(Bdd3),
  false = is_any(Bdd3),
  Bdd3 = empty(),

  ok.

union_any_test() ->
  TyInt = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),

  T1 = tuple(ty_tuple:tuple(TyInt, ty_rec:any())),
  T2 = tuple(ty_tuple:tuple(ty_rec:negate(TyInt), ty_rec:any())),

  % union ==> any
  Bdd = union(T1, T2),
  false = is_empty(Bdd),
  true = is_any(Bdd),
  Bdd = any(),

  ok.

usage_test() ->
  %   (int, int) ^ (1, 2)
  TIa = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
  TIb = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
  TIc = ty_rec:interval(dnf_var_int:int(ty_interval:interval(1, 1))),
  TId = ty_rec:interval(dnf_var_int:int(ty_interval:interval(2, 2))),

  Ty_TupleA = ty_tuple:tuple(TIa, TIb),
  Ty_TupleB = ty_tuple:tuple(TIc, TId),

  B1 = tuple(Ty_TupleA),
  B2 = tuple(Ty_TupleB),

  Bdd = intersect(B1, B2),

  false = is_empty(Bdd),
  false = is_any(Bdd),

  ok.

-endif.
