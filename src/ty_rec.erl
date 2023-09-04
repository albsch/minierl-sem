-module(ty_rec).
-vsn({1,3,0}).

-behavior(type).
-export([empty/0, any/0]).
-export([union/2, negate/1, intersect/2, diff/2, is_any/1]).
-export([is_empty/1, eval/1]).

% additional type constructors
-export([function/1, variable/1, atom/1, interval/1, tuple/1]).
% type constructors with type refs
-export([function/2, tuple/2]).
% top type constructors
-export([function/0, atom/0, interval/0, tuple/0]).

-export([is_equivalent/2, is_subtype/2, normalize/3]).

-export([substitute/2, substitute/3, pi/2, clean_type/2, clean_type/4, all_variables/1, merge_maps/1, collect_variable_positions/2]).

-record(ty, {atom, interval, tuple, function}).

-type ty_ref() :: {ty_ref, integer()}.
-type interval() :: term().
-type ty_tuple() :: term().
-type ty_function() :: term().
-type ty_variable() :: term().
-type ty_atom() :: term().


% ======
% top-level API
% ======

is_subtype(TyRef1, TyRef2) ->
  NewTy = intersect(TyRef1, ty_rec:negate(TyRef2)),

  is_empty(NewTy).

is_equivalent(TyRef1, TyRef2) ->
  is_subtype(TyRef1, TyRef2) andalso is_subtype(TyRef2, TyRef1).

% ======
% Type constructors
% ======

-spec empty() -> ty_ref().
empty() ->
  ty_ref:store(#ty{
    atom = dnf_var_ty_atom:empty(),
    interval = dnf_var_int:empty(),
    tuple = dnf_var_ty_tuple:empty(),
    function = dnf_var_ty_function:empty()
  }).

-spec any() -> ty_ref().
any() ->
  ty_ref:any().

-spec variable(ty_variable()) -> ty_ref().
variable(Var) ->
  Any = ty_ref:load(any()),

  ty_ref:store(Any#ty{
    atom = dnf_var_ty_atom:intersect(Any#ty.atom, dnf_var_ty_atom:ty_var(Var)),
    interval = dnf_var_int:intersect(Any#ty.interval, dnf_var_int:var(Var)),
    tuple = dnf_var_ty_tuple:intersect(Any#ty.tuple, dnf_var_ty_tuple:var(Var)),
    function = dnf_var_ty_function:intersect(Any#ty.function, dnf_var_ty_function:var(Var))
  }).

-spec atom(ty_atom()) -> ty_ref().
atom(Atom) ->
  Empty = ty_ref:load(empty()),
  ty_ref:store(Empty#ty{ atom = Atom }).

-spec atom() -> ty_ref().
atom() -> atom(dnf_var_ty_atom:any()).

-spec interval(interval()) -> ty_ref().
interval(Interval) ->
  Empty = ty_ref:load(empty()),
  ty_ref:store(Empty#ty{ interval = Interval }).

-spec interval() -> ty_ref().
interval() -> interval(dnf_var_int:any()).

-spec tuple(ty_ref(), ty_ref()) -> ty_ref().
tuple(A, B) ->
  Empty = ty_ref:load(empty()),
  Tuple = dnf_var_ty_tuple:tuple(dnf_ty_tuple:tuple(ty_tuple:tuple(A, B))),
  ty_ref:store(Empty#ty{ tuple = Tuple }).

-spec tuple(ty_tuple()) -> ty_ref().
tuple(Tuple) ->
  Empty = ty_ref:load(empty()),
  ty_ref:store(Empty#ty{ tuple = Tuple }).

-spec tuple() -> ty_ref().
tuple() -> tuple(dnf_var_ty_tuple:any()).

-spec function(ty_ref(), ty_ref()) -> ty_ref().
function(A, B) ->
  Empty = ty_ref:load(empty()),
  Fun = dnf_var_ty_function:function(dnf_ty_function:function(ty_function:function(A, B))),
  ty_ref:store(Empty#ty{ function = Fun }).

-spec function(ty_function()) -> ty_ref().
function(Fun) ->
  Empty = ty_ref:load(empty()),
  ty_ref:store(Empty#ty{ function = Fun }).

-spec function() -> ty_ref().
function() ->
  function(dnf_var_ty_function:any()).

% ======
% Boolean operations
% ======

-spec intersect(ty_ref(), ty_ref()) -> ty_ref().
intersect(TyRef1, TyRef2) ->
  #ty{atom = A1, interval = I1, tuple = P1, function = F1} = ty_ref:load(TyRef1),
  #ty{atom = A2, interval = I2, tuple = P2, function = F2} = ty_ref:load(TyRef2),
  ty_ref:store(#ty{
    atom = dnf_var_ty_atom:intersect(A1, A2),
    interval = dnf_var_int:intersect(I1, I2),
    tuple = dnf_var_ty_tuple:intersect(P1, P2),
    function = dnf_var_ty_function:intersect(F1, F2)
  }).

-spec negate(ty_ref()) -> ty_ref().
negate(TyRef1) ->
  #ty{atom = A1, interval = I1, tuple = P1, function = F1} = ty_ref:load(TyRef1),
  ty_ref:store(#ty{
    atom = dnf_var_ty_atom:negate(A1),
    interval = dnf_var_int:negate(I1),
    tuple = dnf_var_ty_tuple:negate(P1),
    function = dnf_var_ty_function:negate(F1)
  }).

-spec diff(ty_ref(), ty_ref()) -> ty_ref().
diff(A, B) -> intersect(A, negate(B)).

-spec union(ty_ref(), ty_ref()) -> ty_ref().
union(A, B) -> negate(intersect(negate(A), negate(B))).


is_empty(TyRef) ->
  % first try op-cache
  case ty_ref:is_empty_cached(TyRef) of
    R when R == true; R == false -> R;
    miss ->
      ty_ref:store_is_empty_cached(TyRef, is_empty_miss(TyRef))
  end.

is_empty_miss(TyRef) ->
  Ty = ty_ref:load(TyRef),
  dnf_var_ty_atom:is_empty(Ty#ty.atom)
    andalso dnf_var_int:is_empty(Ty#ty.interval)
    andalso (
      begin
        case ty_ref:is_empty_memoized(TyRef) of
          true -> true;
          miss ->
            % memoize
            ok = ty_ref:memoize(TyRef),
            dnf_var_ty_tuple:is_empty(Ty#ty.tuple)
              andalso dnf_var_ty_function:is_empty(Ty#ty.function)
        end
      end
  ).

% TODO implement witness
eval(_) ->
  erlang:error(eval_witness_not_implemented).


is_any(_Arg0) ->
  erlang:error(any_not_implemented). % TODO needed?


normalize(TyRef, Fixed, M) ->
  Ty = ty_ref:load(TyRef),
  AtomNormalize = dnf_var_ty_atom:normalize(Ty#ty.atom, Fixed, M),
  case AtomNormalize of
    [] -> [];
    _ ->
      IntervalNormalize = dnf_var_int:normalize(Ty#ty.interval, Fixed, M),
      Res1 = constraint_set:merge_and_meet(AtomNormalize, IntervalNormalize),
      case Res1 of
        [] -> [];
        _ ->
          begin
                Res2 = dnf_var_ty_tuple:normalize(Ty#ty.tuple, Fixed, M),
                Res3 = constraint_set:merge_and_meet(Res1, Res2),
                case Res3 of
                  [] -> [];
                  _ ->
                    Res4 = dnf_var_ty_function:normalize(Ty#ty.function, Fixed, M),
                    constraint_set:merge_and_meet(Res3, Res4)
                end
          end
      end
  end.

substitute(TyRef, SubstituteMap) ->
  substitute(TyRef, SubstituteMap, sets:new()).

substitute(TyRef, SubstituteMap, OldMemo) ->
  case sets:is_element(TyRef, OldMemo) of
    true ->
      TyRef; %done
    false ->
      Memo = sets:union(OldMemo, sets:from_list([TyRef])),
      Ty = #ty{
        atom = Atoms,
        interval = Ints,
        tuple = Tuples,
        function = Functions
      } = ty_ref:load(TyRef),
      io:format(user, "Substitute:~p~n", [Ty]),
      io:format(user, "With:~p~n", [SubstituteMap]),
      NewTy = #ty{
        atom = dnf_var_ty_atom:substitute(Atoms, SubstituteMap),
        interval = dnf_var_int:substitute(Ints, SubstituteMap),
        tuple = dnf_var_ty_tuple:substitute(Tuples, SubstituteMap, Memo),
        function = dnf_var_ty_function:substitute(Functions, SubstituteMap, Memo)
      },

      io:format(user, "Got (~p) :~p~n", [TyRef, NewTy]),
      case has_ref(NewTy, TyRef) of
        true ->
          RecursiveNewRef = ty_ref:new_ty_ref(),
          %ty_rec:replace_ref(NewTy, TyRef,),
          error(todo_rec);

        false -> ty_ref:store(NewTy)
      end
  end.

has_ref(#ty{tuple = Tuple, function = Function}, TyRef) ->
  dnf_var_ty_tuple:has_ref(Tuple, TyRef)
 orelse
    dnf_var_ty_function:has_ref(Function, TyRef).


pi(atom, TyRef) ->
  Ty = ty_ref:load(TyRef),
  Ty#ty.atom;
pi(interval, TyRef) ->
  Ty = ty_ref:load(TyRef),
  Ty#ty.interval;
pi(tuple, TyRef) ->
  Ty = ty_ref:load(TyRef),
  Ty#ty.tuple;
pi(function, TyRef) ->
  Ty = ty_ref:load(TyRef),
  Ty#ty.function.

clean_type(TyRef, FixedVars) ->
  % replace variable that occur only in covariant position by empty
  % replace variable that occur only in contravariant position by any
  % leave other variables in place
  VariablePositions = collect_variable_positions(TyRef, _CurrentPosition = 1), % covariant == *1, contravariant *-1
  clean_type(TyRef, FixedVars, VariablePositions, covariant).

clean_type(TyRef, FixedVars, VariablePositions, CurrentPosition) ->
  ToClean = maps:filter(fun(K, V) -> not sets:is_element(K, FixedVars) andalso (V == [1] orelse V == [-1]) end, VariablePositions),
  SubstituteMap = maps:map(fun(_K, [1]) -> ty_rec:empty(); (_K, [-1]) -> ty_rec:any() end, ToClean),
%%  io:format(user, "Maps~n~p~n", [SubstituteMap]),
%%  io:format(user, "Ty~n~p~n", [ty_ref:load(TyRef)]),
%%  io:format(user, "All~n~p~n", [ty_rec:all_variables(TyRef)]),
  ty_rec:substitute(TyRef, SubstituteMap).
%%  TyRef
%%.

all_variables(TyRef) ->
  #ty{
    atom = Atoms,
    interval = Ints,
    tuple = Tuples,
    function = Functions
  } = ty_ref:load(TyRef),

  lists:usort(dnf_var_ty_atom:all_variables(Atoms)
  ++ dnf_var_int:all_variables(Ints)
  ++ dnf_var_ty_tuple:all_variables(Tuples)
  ++ dnf_var_ty_function:all_variables(Functions)).


collect_variable_positions(TyRef, CurrentPosition) ->
  #ty{
    atom = Atoms,
    interval = Ints,
    tuple = Tuples,
    function = Functions
  } = ty_ref:load(TyRef),

  % TODO CONTINUE HERE VERIFY AND EXPORT ALL CONTINUE DNF TUPLE & FUNCTION
  M1 = dnf_var_ty_atom:collect_variable_positions(Atoms, CurrentPosition),
  M2 = dnf_var_int:collect_variable_positions(Ints, CurrentPosition),
  M3 = dnf_var_ty_tuple:collect_variable_positions(Tuples, CurrentPosition),
  M4 = dnf_var_ty_function:collect_variable_positions(Functions, CurrentPosition),

  merge_maps([M1, M2, M3, M4]).

merge_maps(Maps) ->
  lists:foldl(fun(Item, Map) -> maps:merge_with(fun (_, V1, V2) -> lists:usort(V1 ++ V2) end, Item, Map) end, #{}, Maps).




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

usage_test() ->
  Lists = ty_ref:new_ty_ref(),

  % nil
  Nil = ty_rec:atom(dnf_var_ty_atom:ty_atom(ty_atom:finite([nil]))),

  % (alpha, Lists)
  Alpha = ty_variable:new("alpha"),
  AlphaTy = ty_rec:variable(Alpha),
  Tuple = ty_rec:tuple(dnf_var_ty_tuple:tuple(dnf_ty_tuple:tuple(ty_tuple:tuple(AlphaTy, Lists)))),
  Recursive = ty_rec:union(Nil, Tuple),


  ty_ref:define_ty_ref(Lists, ty_ref:load(Recursive)),

  SomeBasic = ty_rec:atom(dnf_var_ty_atom:ty_atom(ty_atom:finite([somebasic]))),
  SubstMap = #{Alpha => SomeBasic},
  Res = ty_rec:substitute(Lists, SubstMap),
  io:format(user, "Res:~n~p~n", [Res]),


  ok.

-endif.
