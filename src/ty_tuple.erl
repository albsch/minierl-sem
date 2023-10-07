-module(ty_tuple).
-vsn({2,0,0}).

%% 2-tuple representation

-behavior(eq).
-export([compare/2, equal/2]).

-behavior(b_tuple).
-export([tuple/2, pi1/1, pi2/1, has_ref/2]).

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


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

usage_test() ->
    % (int, int)
    Int = ty_interval:interval('*', '*'),
    false = ty_interval:is_empty(Int),
    true = ty_interval:is_any(Int),
    DnfVarInt = dnf_var_int:int(Int),
    io:format(user, "~p~n", [DnfVarInt]),
    false = dnf_var_int:is_empty(DnfVarInt),
    true = dnf_var_int:is_any(DnfVarInt),
    TIa = ty_rec:interval(DnfVarInt),
    TIb = ty_rec:interval(DnfVarInt),

    _Ty_Tuple = ty_tuple:tuple(TIa, TIb),

    ok.

-endif.
