-module(tally_tests).

%%-include_lib("eunit/include/eunit.hrl").
%%-include_lib("log.hrl").
%%
%%-import(stdtypes, [tvar/1, ttuple_any/0, tnegate/1, tatom/0, tatom/1, tfun_full/2, tfun1/2, trange/2,
%%                   tunion/1, tunion/2, tintersect/1, trange_any/0, ttuple/1, tany/0, tnone/0,
%%                   tint/0, tint/1, ttuple1/1, tinter/1, tinter/2, tlist/1, tempty_list/0,
%%                   tfloat/0, tfun2/3, tnot/1, tbool/0
%%                  ]).
%%
%%-spec test_tally_unique(list({ast:ty(), ast:ty()}), #{ atom() => ast:ty()}) -> ok.
%%test_tally_unique(ConstrList, ExpectedSubst) ->
%%    test_tally_unique(ConstrList, ExpectedSubst, []).
%%
%%-spec test_tally_unique(list({ast:ty(), ast:ty()}), #{ atom() => ast:ty()}, [ast:ty_varname()]) -> ok.
%%test_tally_unique(ConstrList, ExpectedSubst, FixedVars) ->
%%    Constrs = sets:from_list(
%%                lists:map(
%%                  fun ({T, U}) -> {csubty, sets:from_list([ast:loc_auto()]), T, U} end,
%%                  ConstrList
%%                 )),
%%    SortFun = fun({K1,_}, {K2,_}) -> K1 < K2 end,
%%    case tally:tally(symtab:empty(), Constrs, sets:from_list(FixedVars)) of
%%        [RealSubst] ->
%%            ?LOG_WARN("~nSubst: ~s~n~nExpected: ~s",
%%                      pretty:render_subst(RealSubst),
%%                      pretty:render_subst(ExpectedSubst)),
%%            ?assertEqual(lists:sort(SortFun, maps:to_list(ExpectedSubst)),
%%                         lists:sort(SortFun, maps:to_list(RealSubst)));
%%        {error, Err} ->
%%            ?LOG_WARN("tally returned an error: ~200p", Err),
%%            error("test failed because tally returned an error");
%%        Substs ->
%%            ?LOG_WARN("tally returned several substitutions: ~s", pretty:render_substs(Substs)),
%%            error("test failed because tally returned more than one substitution")
%%    end.
%%
%%tally_01_test() ->
%%    test_tally_unique([{tvar(alpha), tint()}], #{ alpha => tnone() }).
%%
%%tally_02_test() ->
%%    test_tally_unique([{tint(), tvar(alpha)}], #{ alpha => tint() }).
%%
%%tally_03_test() ->
%%    Alpha = tvar(alpha),
%%    Beta = tvar(beta),
%%    test_tally_unique([{Alpha, ttuple1(tany())},
%%                       {ttuple1(Beta), Alpha}, {tint(), Beta}],
%%                      #{ beta => tint(), alpha => ttuple1(tint())}).
%%
%%% See #30
%%tally_04_test() ->
%%    Alpha = tvar(alpha),
%%    Beta = tvar(beta),
%%    test_tally_unique([{tlist(Beta), Alpha}],
%%                      #{ alpha => tempty_list() }).
%%
%%% See #31
%%% debug tallying (['b] [ ('b, 'a2) ]);;
%%% result:[{'a2:='b}]
%%tally_05_test() ->
%%    Alpha = tvar(alpha),
%%    Beta = tvar(beta),
%%    test_tally_unique([{Beta, Alpha}],
%%                      #{ alpha => Beta },
%%                      [beta]).
%%
%%% debug tallying (['b] [ (['b*], 'a2) ]);;
%%% result:[{'a2:=[ 'b* ]}]
%%tally_06_test() ->
%%  Alpha = tvar(alpha),
%%  Beta = tvar(beta),
%%  test_tally_unique([{tlist(Beta), Alpha}],
%%    #{ alpha => tunion([tempty_list(), stdtypes:tlist_improper(Beta, tempty_list())]) },
%%    [beta]).
%%
%%% see #36
%%% # debug tallying ([] [(1|2, 'alpha) ( (Int -> Int) & ((1|2) -> (1|2)), 'alpha -> 'beta) ('beta, 1|2)]);;
%%% [DEBUG:tallying]
%%% Result:[{'beta:=1--2 | 1--2 & 'a6a6; 'alpha:=1--2 | 1--2 & 'a6a6}]
%%% Cleaned result:[{'beta:=1--2 | 1--2 & 'a6a6; 'alpha:=1--2 | 1--2 & 'a6a6}]
%%tally_07_test() ->
%%  Alpha = tvar(alpha),
%%  Beta = tvar(beta),
%%  OneOrTwo = tunion(tint(1), tint(2)),
%%  OneOrTwoRange = trange(1, 2),
%%  test_tally_unique(
%%    [{OneOrTwo, Alpha},
%%      {Beta, OneOrTwo},
%%      {tinter(tfun1(tint(), tint()), tfun1(OneOrTwo, OneOrTwo)), tfun1(Alpha, Beta)}],
%%    #{ alpha => OneOrTwoRange, beta => OneOrTwoRange }).
%%
%%% Also see #36
%%% #debug tallying ([] [( ((Int, Int) -> Int) & ((Int, Float) -> Float) & ((Float, Int) -> Float) & ((Float, Float) -> Float), ('alpha, 'beta) -> 'gamma ) (Int \ (1--2), 'alpha) (1, 'beta) (1, 'delta) (2, 'delta) ('delta, Int) ('gamma, 'delta)]);;
%%% [DEBUG:tallying]
%%% Result:[{'gamma:=Int; 'delta:=Int; 'beta:=1 | Int & 'betabeta;
%%%         'alpha:=*--0 | 3--* | Int & 'alphaalpha}]
%%% Cleaned result:[{'gamma:=Int; 'delta:=Int; 'beta:=1; 'alpha:=*--0 | 3--*}]
%%tally_08_test() ->
%%    Alpha = tvar(alpha),
%%    Beta = tvar(beta),
%%    Gamma = tvar(gamma),
%%    Delta = tvar(delta),
%%    One = tint(1),
%%    Two = tint(2),
%%    OneOrTwo = tunion(One, Two),
%%    I = tint(),
%%    F = tfloat(),
%%    test_tally_unique(
%%      [{tinter([tfun2(I, I, I), tfun2(I, F, F), tfun2(F, I, F), tfun2(F, F, F)]), tfun2(Alpha, Beta, Gamma)},
%%       {tinter(I, tnot(OneOrTwo)), Alpha},
%%       {One, Beta},
%%       {One, Delta},
%%       {Two, Delta},
%%       {Delta, I},
%%       {Gamma, Delta}],
%%      #{ alpha => tunion(tunion([trange(3, '*')]), tunion([trange('*', 0)])),
%%         beta => tunion([trange(1, 1)]),
%%         gamma => I,
%%         delta => I }).
