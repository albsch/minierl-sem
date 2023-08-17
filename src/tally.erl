-module(tally).


-export([
  tally/1,
  tally/2
]).

-type ty_varname() :: term().

-spec tally(constraint:simple_constraints(), sets:set(ty_varname())) -> [substitution:t()] | {error, [{error, string()}]}.
tally(Constraints, FixedVars) ->

  logger:notice("Norming!"),
  N = tally_norm:norm_api(Constraints, FixedVars),

  erlang:error("TODO").
%%  M = case N of [] -> {fail, norm}; _ -> N end,
%%
%%  S = tally_solve:solve(M, FixedVars),
%%
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

-spec tally(constraint:simple_constraints()) -> [substitution:t()] | {error, [{error, string()}]}.
tally(Constraints) ->
  tally(Constraints, sets:new()).


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
