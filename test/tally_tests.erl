-module(tally_tests).
-include_lib("eunit/include/eunit.hrl").

-import(test_ast, [norm_css_basic/1, norm_substs/1, norm/1, mu/2, n/1, b/0, b/1, f/2, t/2, i/2, i/1, u/2, u/1, r/1, r/0, none/0, any/0, v/1, subty/2, normalize/3, normalize/2, var_of/1, norm_css/1]).

%%example_merge_test() ->
%%  C1 = {norm(f(v(alpha), b(bool))), norm(f(v(beta), v(beta)))},
%%  C2 = {norm(f(u(r(), b(bool)), r())), norm(f(v(alpha), v(beta)))},
%%  X = tally:tally([C1, C2]),
%%  [Z1, Z2] = X,
%%
%%  io:format(user, "Result 1:~n~p~n~n", [Z1]),
%%  [{V1, T1}, {V2, T2}] = Z1,
%%  io:format(user, "~p == ~p~n", [V1, ty_ref:load(T1)]),
%%  io:format(user, "~p == ~p~n", [V2, ty_ref:load(T2)]),
%%%%
%%  [{V3, T3}, {V4, T4}] = Z2,
%%  io:format(user, "~p == ~p~n", [V3, ty_ref:load(T3)]),
%%  io:format(user, "~p == ~p~n", [V4, ty_ref:load(T4)]),
%%
%%  ok.

%%example_merge2_test() ->
%%  C1 = {norm(f(v(alpha), b(bool))), norm(f(v(beta), v(beta)))},
%%  C2 = {norm(t(r(), r())), norm(f(v(alpha), v(beta)))},
%%  Res = tally:tally([C1, C2]),
%%
%%  io:format(user, "Result:~n~p~n,", [Res]),
%%
%%  ok.


buggy_old_tally_test() ->
  %%C1: ($1 -> $2) <= $0
  %%C2: $4 <= $2
  %%C3: 42 <= $4
  %%C4: $3 <= any
  %%C5: $3 /\ int <= $4
  %%C6: $3 /\ int <= $5
  %%C7: (any -> bool) <= ($5 -> $6)
  %%C8: $6 <= bool
  %%C9: $1 <= $3
  C1 = {norm(f(v('1'), v('2'))), norm(v('0'))},
  C2 = {norm(v('4'))           , norm(v('2'))},
  C3 = {norm(r(42))            , norm(v('4'))},
  C4 = {norm(v('3'))           , norm(any())},
  C5 = {norm(i(v('3'), r()))   , norm(v('4'))},
  C6 = {norm(i(v('3'), r()))   , norm(v('5'))},
  C7 = {norm(f(any(), b(bool))), norm(f(v('5'), v('6')))},
  C8 = {norm(v('6'))           , norm(b(bool))},
  C9 = {norm(v('1'))           , norm(v('3'))},

  Res = tally:tally([C1, C2, C3, C4, C5, C6, C7, C8, C9]),



  %% Sol 1
  %% $0 => fun((none()) -> 42),
  %% $1 => none(),
  %% $2 => 42,
  %% $3 => none(),
  %% $4 => 42,
  %% $5 => none(),
  %% $6 => none()}

  %% Sol 2
  %% $0 => fun((none()) -> 42),
  %% $1 => none(),
  %% $2 => 42,
  %% $3 => none(),
  %% $4 => 42,
  %% $5 => none(),
  %% $6 => false | true}

%%  io:format(user, "Result:~n~p~n", [Res]),
  [Sub1, Sub2] = Res,
  io:format(user, "Sub1:~n~p~n", [lists:map(fun({Var, Ty}) -> {Var, ty_ref:load(Ty)} end, Sub1)]),

  % ($1, 1) => X_8 ^ !X_12
  % ($2, 2) => X_9 ^ ATOM U !X_9 ^ X_11 ^ ATOM U X_9 ^ INT U !X_9 ^ X_11 ^ INT U !X_9 ^ !X_11 ^ X_12 ^ INT U !X_9 ^ !X_11 ^ !X_12 ^ 42
  % $0 => X_10 ^ Atom
  %     U X_10 ^ Int
  %     U X_10 ^ Tuple
  %     U $1 -> $2


  io:format(user, "Sub2:~n~p~n", [Sub2]),

  ok.