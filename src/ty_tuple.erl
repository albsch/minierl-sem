-module(ty_tuple).
-vsn({2,0,0}).

%% 2-tuple representation

-behavior(eq).
-export([compare/2, equal/2]).

-behavior(b_tuple).
-export([tuple/2, pi1/1, pi2/1, has_ref/2, big_intersect/1]).

compare(A, B) when A < B -> -1;
compare(A, B) when A > B -> 1;
compare(_, _) -> 0.

equal(P1, P2) -> compare(P1, P2) =:= 0.

tuple(Ref1, Ref2) -> {ty_tuple, Ref1, Ref2}.

pi1({ty_tuple, Ref, _}) -> Ref.
pi2({ty_tuple, _, Ref}) -> Ref.

has_ref({ty_tuple, Ref, _}, Ref) -> true;
has_ref({ty_tuple, _, Ref}, Ref) -> true;
has_ref({ty_tuple, _, _}, _Ref) -> false.

big_intersect(AllTuples) ->
    {NS, NT} = lists:foldl(
        fun(Ty, {S, T}) ->
            {ty_rec:intersect(pi1(Ty), S), ty_rec:intersect(pi2(Ty), T)}
        end,
        {ty_rec:any(), ty_rec:any()}, AllTuples),
    {ty_tuple, NS, NT}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

usage_test() ->
    % (int, int)
    TIa = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
    TIb = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),

    _Ty_Tuple = ty_tuple:tuple(TIa, TIb),

    ok.

-endif.
