-module(tally).


-export([
  tally/1,
  tally/2
]).

% C = {(α → Bool, β → β) , (Int∨Bool → Int , α → β)}

tally(Constraints, FixedVars) ->
  % TODO heuristic here and benchmark
  Normalized = lists:foldl(fun({S, T}, A) ->
    constraint_set:meet(
      fun() ->
        SnT = ty_rec:diff(S, T),
        ty_rec:normalize(SnT, FixedVars, sets:new())
      end,
      fun() -> A end)
              end, [[]], Constraints),

  % TODO can be moved inside the normalize phase, benchmark difference
  Saturated = lists:foldl(fun(ConstraintSet, A) ->
    constraint_set:join(
      fun() -> constraint_set:merge(ConstraintSet, FixedVars, sets:new()) end,
      fun() -> A end)
                           end, [], Normalized),

  Saturated,

  case Saturated of
    [] -> {error, []};
    _ -> solve(Saturated, FixedVars)
  end.


%%  Min = minimize_solutions(S),
%%  X = case Min of
%%    {fail, _X} ->
%%      {error, []};
%%    _ ->
%%      % transform to subst:t()
%%      [maps:from_list([{VarName, Ty} || {{var, VarName}, Ty} <- Subst]) || Subst <- S]
%%  end,
%%
%%  X.

%%-spec tally(constraint:simple_constraints()) -> [substitution:t()] | {error, [{error, string()}]}.
tally(Constraints) ->
  tally(Constraints, sets:new()).

solve(SaturatedSetOfConstraintSets, FixedVariables) ->
  S = ([ solve_single(C, [], FixedVariables) || C <- SaturatedSetOfConstraintSets]),

  RawSubstitutions = [unify(E) || E <- S],

  % replace covariant -> Empty and contravariant -> Any

  % unify produces very ugly types
  % clean up a bit
  CleanSubstitution = fun(Substitution, CFix) ->
    lists:map(fun({Var, Ty}) -> {Var, ty_rec:clean_type(Ty, CFix)} end, Substitution)
                      end,
  lists:map(fun(E) -> CleanSubstitution(E, FixedVariables) end, RawSubstitutions).

solve_single([], Equations, _) -> Equations;
solve_single([{SmallestVar, Left, Right} | Cons], Equations, Fix) ->
  % constraints are already sorted by variable ordering
  % smallest variable first
  FreshTyVar = ty_rec:variable(ty_variable:new("tally_fresh")),
  NewEq = Equations ++ [{eq, SmallestVar, ty_rec:intersect(ty_rec:union(Left, FreshTyVar), Right)}],

  solve_single(Cons, NewEq, Fix).

unify([]) -> [];
unify(EquationList) ->
  % sort to smallest variable
  % select in E the equation α = tα for smallest α
  [Eq = {eq, Var, TA} | _Tail] = lists:usort(fun({_, Var, _}, {_, Var2, _}) -> ty_variable:compare(Var, Var2) =< 0 end, EquationList),

  % create new recursive type μX
  MuX = ty_ref:new_ty_ref(),

  % define type
  % μX.(tα{X/α}) (X fresh)
  Mapping = #{Var => MuX},
  Inner = ty_rec:substitute(TA, Mapping),
  ty_ref:define_ty_ref(MuX, ty_ref:load(Inner)),

  E_ = [
    {eq, XA, ty_rec:substitute(TAA, Mapping)} ||
    (X = {eq, XA, TAA}) <- EquationList, X /= Eq
  ],

  % TODO remove assert
  true = length(EquationList) - 1 == length(E_),

  Sigma = unify(E_),
  NewTASigma = apply_substitution(MuX, Sigma),

  [{Var, NewTASigma}] ++ Sigma.

apply_substitution(Ty, Substitutions) ->
%%  io:format(user, "Applying: ~p with ~p~n", [Ty, Substitutions]),
  SubstFun = fun({Var, To}, Tyy) ->
    Mapping = #{Var => To},
    ty_rec:substitute(Tyy, Mapping)
    end,
  lists:foldl(SubstFun, Ty, Substitutions).

%%minimize_solutions(X = {fail, _}, _) -> X;
%%minimize_solutions(M, Sym) ->
%%  R = lists:filter(fun(Sigma) -> not can_be_removed(Sigma, M, Sym) end, M),
%%
%%  case R of
%%    M -> M;
%%    _ ->
%%      ?LOG_DEBUG("Successfully reduced tally solution size! ~p -> ~p", length(M), length(R)),
%%      R
%%  end.
%%
%%can_be_removed(Sigma, AllSubs, Sym) ->
%%  % does Sigma' exist such that
%%  lists:any(fun(SigmaPrime) ->
%%    % dom(Sigma') <: dom(Sigma)
%%    domain(SigmaPrime, Sigma)
%%    andalso
%%    %for all alpha \in dom(sigma'): sigma'(alpha) ~ sigma(alpha)
%%    sub_domain_equivalent(SigmaPrime, Sigma, Sym)
%%            end, lists:delete(Sigma, AllSubs)).
%%
%%
%%domain(Sigma1, Sigma2) ->
%%  S1 = [Var || {Var, _} <- Sigma1],
%%  S2 = [Var || {Var, _} <- Sigma2],
%%  gb_sets:is_subset(gb_sets:from_list(S1), gb_sets:from_list(S2)).
%%
%%sub_domain_equivalent(S1, S2, Sym) ->
%%  SAll = [Var || {Var, _} <- S1],
%%  lists:all(fun(Var) ->
%%    [Ty] = [T || {V, T} <- S1, V == Var],
%%    [Ty2] = [T || {V, T} <- S2, V == Var],
%%    subty:is_equivalent(Sym, Ty, Ty2)
%%            end, SAll).
