-module(normalize_tests).
-include_lib("eunit/include/eunit.hrl").

-import(test_ast, [norm/1, mu/2, n/1, b/0, b/1, f/2, t/2, i/2, i/1, u/2, u/1, r/1, r/0, none/0, any/0, v/1, subty/2, normalize/3, normalize/2, var_of/1]).

%%simple_normalize_atom_test() ->
%%  % satisfiable constraint: some_atom <: ANY_atom
%%  [[]] = normalize(b(some_atom), b(), sets:new()),
%%
%%  % unsatisfiable constraint: ANY_atom <: some_atom
%%  [] = normalize(b(), b(some_atom), sets:new()),
%%
%%  Alpha = v(alpha),
%%  Beta = v(beta),
%%
%%  % simple variable constraints
%%  [] = normalize(Alpha, b(some_atom), sets:from_list([Alpha])),
%%  [] = normalize(Alpha, Beta, sets:from_list([Alpha, Beta])),
%%  [[]] = normalize(i(Alpha, b()), u(Beta, b()), sets:from_list([Alpha, Beta])),
%%
%%  ok.
%%
%%simple_atom_normalize_test() ->
%%  Alpha = v(alpha),
%%
%%  [[{V, L, R}]] = normalize(i(Alpha, b()), sets:new()),
%%  true = ty_rec:is_empty(L),
%%  true = ty_rec:is_equivalent(R, norm(n(b()))),
%%  V = var_of(Alpha),
%%
%%  ok.
%%
%%var_ordering_atom_normalize_test() ->
%%  Alpha = v(alpha),
%%  Beta = v(beta),
%%
%%  [[{V, L, R}]] = normalize(i([Beta, Alpha, b()]), sets:new()),
%%  true = ty_rec:is_empty(L),
%%  true = ty_rec:is_equivalent(R, norm(n(i(b(), Beta)))),
%%  V = var_of(Alpha),
%%
%%  ok.
%%
%%neg_var_atom_normalize_test() ->
%%  Alpha = v(alpha),
%%  Beta = v(beta),
%%
%%  [[{V, L, R}]] = normalize(i([Beta, n(Alpha), b()]), sets:new()),
%%  true = ty_rec:is_equivalent(ty_rec:any(), R),
%%  true = ty_rec:is_equivalent(L, norm(i(b(), Beta))),
%%  V = var_of(Alpha),
%%
%%  ok.
%%
%%simple_normalize_interval_test() ->
%%  [[]] = normalize(r(1), r(), sets:new()),
%%  [] = normalize(r(), r(1), sets:new()),
%%
%%  Alpha = v(alpha),
%%  Beta = v(beta),
%%
%%  % simple variable constraints
%%  [] = normalize(Alpha, r(1), sets:from_list([Alpha])),
%%  [] = normalize(Alpha, Beta, sets:from_list([Alpha, Beta])),
%%  [[]] = normalize(i(Alpha, r()), u(Beta, r()), sets:from_list([Alpha, Beta])),
%%
%%  ok.
%%
%%simple_normalize_tuple_test() ->
%%  [] = normalize(i(t(r(), r()), n(t(b(), b()))), sets:new()),
%%  [[]] = normalize(i(t(r(), r()), n(t(r(), r()))), sets:new()),
%%
%%  ok.

example_1_normalize_test() ->
  % α→Bool ≤ β→β
  Res = normalize(f(v(alpha), b(bool)), f(v(beta), v(beta)), sets:new()),

  io:format(user, "Res: ~p~n", [Res]),

  ok.
