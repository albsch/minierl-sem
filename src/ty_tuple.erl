-module(ty_tuple).
-vsn({2,0,0}).

%% 2-tuple representation

-export([compare/2, equal/2, is_empty/1, is_any/1]).
-export([tuple/2, pi1/1, pi2/1, has_ref/2, big_intersect/1, any/0]).

compare(A, B) when A < B -> -1;
compare(A, B) when A > B -> 1;
compare(_, _) -> 0.

equal(P1, P2) -> compare(P1, P2) =:= 0.

any() ->
    {ty_tuple, ty_rec:any(), ty_rec:any()}.

tuple(Ref1, Ref2) ->
    % ensure that constructed tuple is keeping the Empty & Any representation invariant
    case ty_rec:is_empty(Ref1) orelse ty_rec:is_empty(Ref2) of
        true -> error(todo_smart_cons);
        _ ->
            case ty_rec:is_any(Ref1) andalso ty_rec:is_any(Ref2) of
                true -> {ty_tuple, ty_rec:any(), ty_rec:any()};
                _ ->
                    {ty_tuple, Ref1, Ref2}
            end
    end.

pi1({ty_tuple, Ref, _}) -> Ref.
pi2({ty_tuple, _, Ref}) -> Ref.

has_ref({ty_tuple, Ref, _}, Ref) -> true;
has_ref({ty_tuple, _, Ref}, Ref) -> true;
has_ref({ty_tuple, _, _}, _Ref) -> false.

% constant empty check
% by invariant, semantically equivalent empty tuples are always represented as (0,0)
% by invariant, semantically equivalent any tuples are always represented as (1,1)
is_empty({ty_tuple, A, B}) ->
    case ty_rec:empty() of
        A -> true;
        B -> true;
        _ -> false
    end.

is_any({ty_tuple, A, B}) ->
    case {ty_rec:any(), ty_rec:any()} of
        {A, B} -> true;
        _ -> false
    end.

big_intersect(All) ->
    lists:foldl(fun({ty_tuple, S, T}, {ty_tuple, A, B}) ->
        {ty_tuple, ty_rec:intersect(S, A), ty_rec:intersect(T, B)}
                end, any(), All).

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
