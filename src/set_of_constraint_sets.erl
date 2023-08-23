-module(set_of_constraint_sets).

%% API
-export([is_smaller/2]).

%
is_smaller([], _S2) -> true;
is_smaller([C | Cs], S2) ->
  constraint_set:has_smaller_constraint(C, S2)
  andalso is_smaller(Cs, S2).
