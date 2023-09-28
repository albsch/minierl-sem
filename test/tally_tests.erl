-module(tally_tests).
-include_lib("eunit/include/eunit.hrl").

-import(test_ast, [norm_css_basic/1, norm_substs/1, norm/1, mu/2, n/1, b/0, b/1, f/2, t/2, i/2, i/1, u/2, u/1, r/1, r/0, none/0, any/0, v/1, subty/2, normalize/3, normalize/2, var_of/1, norm_css/1]).

% TODO think about how to test tally equivalent solutions

%%example_merge_test() ->
%%  C1 = {norm(f(v(alpha), b(bool))), norm(f(v(beta), v(beta)))},
%%  C2 = {norm(f(u(r(), b(bool)), r())), norm(f(v(alpha), v(beta)))},
%%  X = tally:tally([C1, C2]),
%%  [Z1, Z2] = X,
%%
%%  [{_V1, _T1}, {_V2, _T2}] = Z1,
%%  [{_V3, _T3}, {_V4, _T4}] = Z2,
%%
%%  ok.
%%
%%example_merge2_test() ->
%%  C1 = {norm(f(v(alpha), b(bool))), norm(f(v(beta), v(beta)))},
%%  C2 = {norm(t(r(), r())), norm(f(v(alpha), v(beta)))},
%%  _Res = tally:tally([C1, C2]),
%%  ok.

%%issue_8_tally_test() ->
%%  % fix order for variables
%%  lists:foreach(fun(Atom) -> norm(v(Atom)) end, ['0','1','2','3','4','5','6']),
%%  C1 = {norm(f(v('1'), v('2'))), norm(v('0'))},
%%  C2 = {norm(v('4'))           , norm(v('2'))},
%%  C3 = {norm(r(42))            , norm(v('4'))},
%%  C4 = {norm(v('3'))           , norm(any())},
%%  C5 = {norm(i(v('3'), r()))   , norm(v('4'))},
%%  C6 = {norm(i(v('3'), r()))   , norm(v('5'))},
%%  C7 = {norm(f(any(), b(bool))), norm(f(v('5'), v('6')))},
%%  C8 = {norm(v('6'))           , norm(b(bool))},
%%  C9 = {norm(v('1'))           , norm(v('3'))},
%%
%%  Res = tally:tally([C1, C2, C3, C4, C5, C6, C7, C8, C9]),
%%
%%  [_Sub1, _Sub2] = Res,
%%
%%  ok.
%%
%%issue_13_tally_test() ->
%%  A = b(a),
%%  B = b(b),
%%  D = b(dummy),
%%  TAny = t(any(), any()),
%%  LargeInter = i([
%%    v(v0),
%%    n(i([t(A, D), TAny])),
%%    t(B, D),
%%    TAny
%%  ]),
%%
%%  C1 = { i([v(v0), t(A, D), t(any(), any())]),  t(v(v3), D) },
%%  C2 = { u([ i(t(A, D), TAny), i(t(B, D), TAny) ]), t(v(v8), D) },
%%  C3 = { t(v(v2), D), v(v0)},
%%  C4 = { LargeInter, t(v(v8), D)},
%%  C5 = { LargeInter, t(v(v7), D)},
%%  C6 = { LargeInter, t(v(v6), D)},
%%  C7 = { A, v(v2)},
%%  C8 = { i([v(v0), t(A, D), TAny]),t(v(v5), D)},
%%  C9 = { i([v(v0), t(A, D), TAny]),t(v(v4), D)},
%%
%%  % fix order for variables
%%%%  lists:foreach(fun(Atom) -> norm(v(Atom)) end, ['0','1','2','3','4','5','6']),
%%
%%  _Res = tally:tally(norm_all([C1, C2, C3, C4, C5, C6, C7, C8, C9])),
%%
%%%%  io:format(user, "Res: ~n~p~n", [Res]),
%%
%%  ok.


% debug tallying ([]
% [
%   ('a1,Int)
%   ('a22,'a3)
%   (1,'a17)
%   ('a11,Any)
%   ('a7,'a5)
%   ('a19,'a14)
%   ('a20,'a12)
%   ('a12,'a7)
%   ('a3,'a1)
%   ('a10,'a11)
%   ('a4,'a8)
%   ('a2,Any)
%   (3,'a21)
%   (0,'a12)
%   ('a18,'a15)


%   (('a4 -> 'a5),'a2)
%   (('a8, `tag),'a6)
%   ('a2, ('a21 -> 'a22))
%   ((`tag, `tag),'a0)
%   (('a11&(Any \ 0)),'a16)
%   (('a11&(Any \ 0)),'a13)
%   (('a4 -> 'a5),('a15 -> 'a19))
%   ('a6,((Any, `tag)&(Any, `tag)))
%   ('a0,((`tag, `tag)&(`tag, `tag)))

%   (('a6&((Any, `tag)&(Any, `tag))),('a10, `tag))
%   (('a6&((Any, `tag)&(Any, `tag))),('a9, `tag))
%   (('a0&((`tag, `tag)&(`tag, `tag))),(`tag, `tag))

%   ((((Int,Int) -> Int)&(((Int,`float) -> `float)&(((`float,Int) -> `float)&((`float,`float) -> `float)))),(('a16,'a17) -> 'a18))
%   ((((Int,Int) -> Int)&(((Int,`float) -> `float)&(((`float,Int) -> `float)&((`float,`float) -> `float)))),(('a13,'a14) -> 'a20))
% ]);;
% [DEBUG:tallying]
%Result:[{'a0:=(`tag,`tag); 'a11:=Int; 'a13:=*---1 | 1--* | Int & 'a13a13 & 'a16a16 | 0 & 'a13a13; 'a18:=Int; 'a15:=Int;
%'a2:=(3 | Int & 'a21a21 -> Int) & (Int -> Int) | (3 | Int & 'a21a21 -> Int) & 'a2a2; 'a22:=Int; 'a21:=3 | Int & 'a21a21;
%        'a6:=(Int,`tag); 'a10:=Int; 'a12:=Int; 'a4:=Int; 'a8:=Int; 'a20:=Int; 'a14:=Int; 'a5:=Int; 'a19:=Int; 'a3:=Int; 'a1:=Int; 'a7:=
%        Int; 'a16:=*---1 | 1--* | Int & 'a16a16; 'a17:=1 | Int & 'a17a17; 'a9:=Int | 'a9a9 \ Int}]
slow_tally_test() ->
  T0 = t(b(tag), b(tag)),
  AnyTag = i(t(any(), b(tag)), t(any(), b(tag))),
  Res = tally:tally(norm_all([
    { f(v(a4), v(a5)), v(a2) },
    { f(v(a4), v(a5)), f(v(a15), v(a19)) },
    {
      PlusFun,
      f(t(v(a16), v(a17)), v(a18))
    },

    { v(a7), v(a5) },
    { v(a12), v(a7) },
    { v(a20), v(a12) },
    { v(a4), v(a8) },
    { v(a18), v(a15) }
  ])),

  8 = length(Res),
  io:format(user, "Res: ~n~p~n", [Res]),

  ok.

%%fun_tally_test() ->
%%  lists:foreach(fun(Atom) -> norm(v(Atom)) end, ['a1','a2','a3','a4']),
%%  Res = tally:tally(norm_all([
%%    { f(v(a1), v(a2)), f(v(a3), v(a4)) }
%%  ])),
%%  2 = length(Res),
%%  ok.

%%slow_tally_test() ->
%%  T0 = t(b(tag), b(tag)),
%%  AnyTag = i(t(any(), b(tag)), t(any(), b(tag))),
%%  PlusFun = i([
%%    f(t(r(), r()), r()),
%%    f(t(r(), b(float)), b(float)),
%%    f(t(b(float), r()), b(float)),
%%    f(t(b(float), b(float)), b(float))
%%  ]),
%%
%%
%%  Res = tally:tally(norm_all([
%%    { f(v(a4), v(a5)), v(a2) },
%%    { t(v(a8), b(tag)), v(a0) },
%%    { v(a2), f(v(a21), v(a22)) },
%%    { T0, v(a0) },
%%    { i(v(a11), n(r(0))), v(a16) },
%%    { i(v(a11), n(r(0))), v(a13) },
%%    { f(v(a4), v(a5)), f(v(a15), v(a19)) },
%%    { v(a6), AnyTag},
%%    { v(a0), i(T0, T0) },
%%    { i(v(a6), AnyTag), t(v(a10), b(tag))},
%%    { i(v(a0), i(T0, T0)), T0},
%%    {
%%      PlusFun,
%%      f(t(v(a16), v(a17)), v(a18))
%%    },
%%    {
%%      PlusFun,
%%      f(t(v(a13), v(a14)), v(a20))
%%    },
%%
%%
%%
%%
%%    { v(a1), r() },
%%    { v(a22), v(a3) },
%%    { r(1), v(a17) },
%%    { v(a11), any() },
%%    { v(a7), v(a5) },
%%    { v(a19), v(a14) },
%%    { v(a20), v(a12) },
%%    { v(a12), v(a7) },
%%    { v(a3), v(a1) },
%%    { v(a10), v(a11) },
%%    { v(a4), v(a8) },
%%    { v(a2), any() },
%%    { r(3), v(a21) },
%%    { r(0), v(a12) },
%%    { v(a18), v(a15) }
%%  ])),
%%
%%  io:format(user, "Res: ~n~p~n", [Res]),
%%
%%  ok.

norm_all(List) ->
  lists:map(fun({S, T}) -> {norm(S), norm(T)} end, List).
