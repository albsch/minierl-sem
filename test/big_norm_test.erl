-module(big_norm_test).
-include_lib("eunit/include/eunit.hrl").

-import(test_ast, [norm_substs/1, norm/1, mu/2, d/2, n/1, b/0, b/1, f/2, t/2, i/2, i/1, u/2, u/1, r/1, r/0, none/0, any/0, v/1, subty/2, normalize/3, normalize/2, var_of/1, norm_css/1]).

nm(Ty) ->
  normalize(Ty, sets:new()).



% Note: changing the variable order does not work, the input list is always sorted before initializing the variables in CDuce
% debug tallying (['a00 'a01 'a02 'a03 'a04 'a05 'a06 'a07 'a08 'a09 'a10 'a11 'a12 'a13 'a14 'a15 'a16 'a17 'a18 'a19 'a20 'a21 'a22] [] [ ('a01,Int) ('a22,'a03) (1,'a17) ('a11,Any) ('a07,'a05) ('a19,'a14) ('a20,'a12) ('a12,'a07) ('a03,'a01) ('a10,'a11) ('a04,'a08) ('a02,Any) (3,'a21) (0,'a12) ('a18,'a15) (('a04 -> 'a05),'a02) (('a08, `tag),'a06) ('a02, ('a21 -> 'a22)) ((`tag, `tag),'a00) (('a11&(Any \ 0)),'a16) (('a11&(Any \ 0)),'a13) (('a04 -> 'a05),('a15 -> 'a19)) ('a06,((Any, `tag)&(Any, `tag))) ('a00,((`tag, `tag)&(`tag, `tag))) (('a06&((Any, `tag)&(Any, `tag))),('a10, `tag)) (('a06&((Any, `tag)&(Any, `tag))),('a09, `tag)) (('a00&((`tag, `tag)&(`tag, `tag))),(`tag, `tag)) ((((Int,Int) -> Int)&(((Int,`float) -> `float)&(((`float,Int) -> `float)&((`float,`float) -> `float)))),(('a16,'a17) -> 'a18)) ((((Int,Int) -> Int)&(((Int,`float) -> `float)&(((`float,Int) -> `float)&((`float,`float) -> `float)))),(('a13,'a14) -> 'a20)) ]);;



big_test() ->
 % Accumulator
 % Type to norm
 % Result of norm
 % Result of meet with accumulator

  T0 = t(b(tag), b(tag)),
  PlusFun = i([
    f(t(r(), r()), r()),
    f(t(r(), b(float)), b(float)),
    f(t(b(float), r()), b(float)),
    f(t(b(float), b(float)), b(float))
  ]),
  Suite = [
    {
      [[]],
      d(v(a1), r()),
      [[{none(), v(a1), r()}]],
      [[{none(), v(a1), r()}]]
    },

    {
      [[]],
      d(PlusFun, f(t(v(a16), v(a17)), v(a18))),
      [],
      []
    },


    {
      [[]],
      d(i(v(a00), i(T0, T0)), T0),
      [],
      []
    }
  ],


%%    {Empty <= 'a16 <= Empty}; OK
%%    {Empty <= 'a16 <= Int, Empty <= 'a17 <= Int, Int <= 'a18 <= Any}; OK
%%    {Empty <= 'a16 <= `float, Empty <= 'a17 <= `float | Int, `float <= 'a18 <= Any}; OK
%%    {Empty <= 'a16 <= `float | Int, Empty <= 'a17 <= `float, `float <= 'a18 <= Any}; OK
%%    {Empty <= 'a16 <= `float | Int, Empty <= 'a17 <= `float | Int, `float | Int <= 'a18 <= Any}; OK
%%    {Empty <= 'a17 <= Empty} OK
  all_steps(Suite),
  ok.


all_steps([]) -> ok;
all_steps([{Accumulator, Type, ExpectedNorm, ExpectedMeet} | Rest]) ->
  ActualNorm = nm(Type),
  io:format(user, "Actual Norm~n~p~n", [ActualNorm]),
  io:format(user, "Expected Norm~n~p~n", [ExpectedNorm]),

  io:format(user, "Next step~n", []),




  all_steps(Rest).
