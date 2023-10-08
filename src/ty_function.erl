-module(ty_function).

%% domain -> co-domain function representation

-export([compare/2, equal/2, is_empty/1, is_any/1]).
-export([function/2, domain/1, codomain/1, codomains_intersect/1, has_ref/2]).

compare(A, B) when A < B -> -1;
compare(A, B) when A > B -> 1;
compare(_, _) -> 0.

equal(P1, P2) -> compare(P1, P2) =:= 0.

any() ->
    {ty_function, ty_rec:empty(), ty_rec:any()}.

function(Ref1, Ref2) ->
    % ensure that constructed function is keeping the Any representation invariant
    case ty_rec:is_empty(Ref1) of
        true -> any();
        _ ->
            {ty_function, Ref1, Ref2}
    end.

domain({ty_function, Ref, _}) -> Ref.
codomain({ty_function, _, Ref}) -> Ref.

codomains_intersect([]) -> ty_rec:any();
codomains_intersect([Fun]) -> ty_function:codomain(Fun);
codomains_intersect([Fun | Funs]) -> ty_rec:intersect(ty_function:codomain(Fun), codomains_intersect(Funs)).

has_ref({ty_function, Ref, _}, Ref) -> true;
has_ref({ty_function, _, Ref}, Ref) -> true;
has_ref({ty_function, _, _}, _Ref) -> false.

% constant empty check
% by invariant, functions are never empty
% by invariant, semantically equivalent any functions are always represented as (0 -> 1)
is_empty({ty_function, _, _}) -> false.
is_any({ty_function, A, _B}) ->
    case ty_rec:empty() of
        A -> true;
        _ -> false
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

usage_test() ->
    TIa = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),
    TIb = ty_rec:interval(dnf_var_int:int(ty_interval:interval('*', '*'))),

    % int -> int
    TyFunction = ty_function:function(TIa, TIb),
    false = is_empty(TyFunction),
    false = is_any(TyFunction),

    ok.

-endif.
