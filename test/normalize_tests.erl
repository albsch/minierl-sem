-module(normalize_tests).
-include_lib("eunit/include/eunit.hrl").

-import(test_ast, [mu/2, n/1, b/0, b/1, f/2, t/2, i/2, i/1, u/2, u/1, r/1, r/0, none/0, any/0, v/1, subty/2, norm/2]).

simple_normalize_test() ->
%%  % satisfiable constraint: some_atom <: ANY_atom
%%  [[]] = norm(b(some_atom), b()),
%%
%%  % unsatisfiable constraint: ANY_atom <: some_atom
%%  [] = norm(b(), b(some_atom)),

  % simple variable constraint: alpha <: some_atom
  R = norm(v(alpha), b(some_atom)),
  io:format(user, "~p~n", [R]),

  ok.
