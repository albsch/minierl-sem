-module(set_of_constraint_sets).

%% API
-export([is_equivalent/2]).

%
is_equivalent(S1, S2) ->
  is_smaller(S1, S2) andalso is_smaller(S2, S1).


is_smaller(S1, S2) -> is_smaller(S1, S1, S2).

is_smaller([], _) -> true;
is_smaller([Cs| S1], S2) ->
  constraint_set:has_smaller_constraint_w(Cs, S2),
  true;

