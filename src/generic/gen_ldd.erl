-module(gen_ldd).

% A generic linear decision diagram parameterized over both the 'nodes and 'leafs: LDD<Element, Terminal>
%
% The LDD is different form a BDD in the following ways:
% * There is only a list of linear shared list of negated elements,
%   and at most one positive element branching out
% * A coclause of the DNF consists therefore only of the form
%   Terminal & [Element]
%
% Useful for tuples to keep up the invariant that the representation always
% consists only of one positive tuple because of covariance.
% This is not possible with BDDs since BDDs introduce redundant (non-trivial) formulas upon union.
% e.g.: !(N, N) U (1, N)
%     is represented by a BDD by
%       {node, (N, N),  (node, (1, N), T, F), T)
%     where the evaluation will yield, notice the redundant non-trivial (N,N):
%      !(N, N) U ( (N, N) & (1, N) )
% whereas with a LDD, the representation is
%     {node, none, (1, N), (node, (N,N), (Any, Any), nil))
%     where the evaluation will yield:
%     (1,N)   U   (Any. Any) & !(N,N)
%  keeping up the invariant that all coclauses contain exactly one positive atom

-export([leaf/2, element/2, empty/1, any/1, union/3, intersect/3, negate/2, diff/3, is_empty/2, is_any/2]).

%%-behavior(eq). % implements eq behavior indirectly parameterized over a type
-export([equal/3, compare/3]).

% traverse DNF
-export([dnf/3]).

% basic interface (parameterized)
equal(Gen = {_, Element}, {node, E1, A1, B1}, {node, E2, A2, B2}) ->
    Element:equal(E1, E2) andalso equal(Gen, A1, A2) andalso equal(Gen, B1, B2);
equal({Terminal, _}, {leaf, T1}, {leaf, T2}) -> Terminal:equal(T1, T2);
equal(_, _, _) -> false.

% this is used for ordering BDD nodes
compare({Terminal, _}, {leaf, T1}, {leaf, T2}) -> Terminal:compare(T1, T2);
compare(_, {leaf, _}, {node, _, _, _}) -> +1;
compare(_, {node, _, _, _}, {leaf, _}) -> -1;
compare(Gen = {_, Element}, {node, E1, A1, B1}, {node, E2, A2, B2}) ->
  case Element:compare(E1, E2) of
    0 ->
      case compare(Gen, A1, A2) of
        0 -> compare(Gen, B1, B2);
        Res -> Res
      end;
    Res -> Res
  end.

% ==
% type interface
empty({Terminal, _}) -> {leaf, Terminal:empty()}.
any({Terminal, _}) -> {leaf, Terminal:any()}.

element(I = {Terminal, Element}, Atom) ->
  % smart constructor: if Atom is already ANY or EMPTY, then reduce
  case Element:is_empty(Atom) of
    true -> empty(I);
    _ ->
      case Element:is_any(Atom) of
        true -> any(I);
        _ ->
          s(I, {node, Atom, any(I), empty(I)})
      end
  end.

leaf(I, Ty) -> leaf_of(I, Ty).

union(I = {Terminal, Element}, A, B) ->
  case is_empty(I, A) of
    true -> B;
    _ ->
      case is_empty(I, B) of
        true -> A;
        _ ->
          case is_any(I, A) orelse is_any(I, B) of
            true -> any(I);
            _ ->
              case {A, B} of
                {{leaf, X}, {leaf, Y}} -> leaf_of(I, Terminal:union(X, Y));
                {BDD1 = {node, E, A1, B1}, BDD2 = {node, E2, A2, B2}} ->
                  case Element:compare(E, E2) of
                    -1 ->
                      s(I,{node, E, s(I,union(I, A1, BDD2)), s(I,union(I, B1, BDD2))});
                    +0 ->
                      s(I,{node, E, s(I,union(I, A1, A2)), s(I,union(I, B1, B2))});
                    +1 ->
                      s(I,{node, E2, s(I,union(I, A2, BDD1)), s(I,union(I, B2, BDD1))})
                  end;
                {BDD1 = {leaf, _X}, _BDD2 = {node, E2, A2, B2}} ->
                  s(I, {node, E2, s(I, union(I, A2, BDD1)), s(I, union(I, B2, BDD1))});
                {_BDD1 = {node, E1, A1, B1}, BDD2 = {leaf, _X}} ->
                  s(I, {node, E1, s(I, union(I, A1, BDD2)), s(I, union(I, B1, BDD2))})
              end
          end
      end
  end.

negate(I = {Terminal, _}, TN = {leaf, A}) ->
  case is_any(I, TN) of
    true -> {leaf, Terminal:empty()};
    _ ->
      case is_empty(I, TN) of
        true -> {leaf, Terminal:any()};
        _ ->
          s(I, {leaf, Terminal:negate(A)})
      end
  end;
negate(I, {node, E, B1, B2}) -> s(I, {node, E, s(I, negate(I, B1)), s(I, negate(I, B2))}).

diff(I, A, B) -> intersect(I, A, negate(I, B)).
intersect(I, A, B) -> negate(I, union(I, negate(I, A), negate(I, B))).


% A DNF that is more than a leaf node is by construction and invariant neither EMPTY nor ANY
is_any({Terminal, _}, {leaf, Ty}) -> Terminal:is_any(Ty);
is_any(P = {T, E}, X = {node, Element, Left, Right}) ->
  % invariant
  false = is_empty(P, Left) andalso is_empty(P, Right),
  false = is_any(P, Left) andalso is_any(P, Right),
  false % TODO correct?
.

is_empty({Terminal, _}, {leaf, Ty}) ->
  Terminal:is_empty(Ty);
is_empty(P, {node, _Element, Left, Right}) ->
  % invariant TODO remove expensive
  false = is_empty(P, Left) andalso is_empty(P, Right),
  false = is_any(P, Left) andalso is_any(P, Right),
  false % TODO correct?
.

leaf_of({Terminal, _}, Ty) ->
  case Terminal:equal(Ty, Terminal:empty()) of
    true -> {leaf, Terminal:empty()};
    _ ->
      case Terminal:equal(Ty, Terminal:any()) of
        true -> {leaf, Terminal:any()};
        _ ->
          {leaf, Ty}
      end
  end.


s(_G, {node, _, B, B}) -> B;
s(_G, X) -> X.


dnf(I, Bdd, {ProcessCoclause, CombineResults}) ->
  do_dnf(I, Bdd, {ProcessCoclause, CombineResults}, _Pos = [], _Neg = []).

do_dnf(I = {T, _}, X = {node, Element, Left, Right}, F = {_Process, Combine}, Pos, Neg) ->
  io:format(user, "Doing dnf: ~p~nWith PosNeg: ~p~n", [X, {Pos, Neg}]),
  F1 = fun() -> do_dnf(I, Left, F, [Element | Pos], Neg) end,
  F2 = fun() -> do_dnf(I, Right, F, Pos, [Element | Neg]) end,
  Combine(F1, F2);
do_dnf(I = {T, _}, {leaf, Terminal}, {Proc, _Comb}, Pos, Neg) ->
  DnfTerminal = T:dnf(Terminal),
  Proc(Pos, Neg, DnfTerminal).
