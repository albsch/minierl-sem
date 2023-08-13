-module(ty_atom).
-vsn({1,2,0}).

%% Efficient atom representation

-behavior(eq).
-export([compare/2, equal/2]).

-behavior(type).
-export([empty/0, any/0]).
-export([union/2, intersect/2, diff/2, negate/1, is_any/1]).
-export([is_empty/1, eval/1]).

-behavior(b_atom).
-export([finite/1, cofinite/1]).

empty() -> {{0, nil}, finite}.
any() -> {{0, nil}, cofinite}.

finite([]) ->
  any();
finite([X | Xs]) ->
  intersect({gb_sets:from_list([X]), finite}, finite(Xs)).
cofinite(ListOfBasic) -> {gb_sets:from_list(ListOfBasic), cofinite}.

negate({S, finite}) -> {S, cofinite};
negate({S, cofinite}) -> {S, finite}.

intersect(_, Z = {{0, nil}, finite}) -> Z;
intersect(Z = {{0, nil}, finite}, _) -> Z;
intersect({{0, nil}, cofinite}, S) -> S;
intersect(S, {{0, nil}, cofinite}) -> S;
intersect(S = {_, cofinite}, T = {_, finite}) -> intersect(T, S);
intersect({S, finite}, {T, finite}) ->
  {gb_sets:intersection(S, T), finite};
intersect({S, finite}, {T, cofinite}) ->
  {gb_sets:difference(S, T), finite};
intersect({S, cofinite}, {T, cofinite}) ->
  {gb_sets:union(S,T), cofinite}.

union(S,T) -> negate(intersect(negate(S), negate(T))).

diff(S,T) -> intersect(S, negate(T)).

equal({_, finite},{_, cofinite}) -> false;
equal({_, cofinite},{_, finite}) -> false;
equal({S, _}, {T, _}) -> gb_sets:is_subset(S,T) andalso gb_sets:is_subset(T,S).

eval(_Rep) -> erlang:error("TODO").

is_empty(Rep) ->
  case Rep of
    {{0, nil}, finite} -> true;
    _ -> false
  end.

is_any(Rep) ->
  case Rep of
    {{0, nil}, cofinite} -> true;
    _ -> false
  end.

% using erlang total ordering for now
compare(R1, R2) -> case R1 < R2 of true -> -1; _ -> case R1 > R2 of true -> 1; _ -> 0 end end.