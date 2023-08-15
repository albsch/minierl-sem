-module(ty_ref).
-vsn({1,3,3}).

-export([any/0, store/1, load/1, new_ty_ref/0, define_ty_ref/2, is_empty_cached/1, store_is_empty_cached/2, store_recursive_variable/2, check_recursive_variable/1]).
-export([memoize/1, is_empty_memoized/1, reset/0]).

-on_load(setup_ets/0).
-define(TY_UTIL, ty_counter).        % counter store
-define(TY_MEMORY, ty_mem).          % id -> ty
-define(TY_UNIQUE_TABLE, ty_unique). % ty -> id

-define(EMPTY_MEMO, memoize_ty_ets).        % ty_ref -> true
-define(EMPTY_CACHE, is_empty_memoize_ets). % ty_rec -> true/false

% helper table to construct recursive definitions properly
% once a bound variable is encountered in ty_rec:variable,
% it is treated as a recursive bound variable instead of a free one
-define(RECURSIVE_TABLE, remember_recursive_variables_ets).

all_tables() ->
  [?TY_UNIQUE_TABLE, ?TY_MEMORY, ?TY_UTIL, ?EMPTY_MEMO, ?EMPTY_CACHE, ?RECURSIVE_TABLE].

reset() ->
  ets:delete(?EMPTY_MEMO),
  ets:delete(?EMPTY_CACHE),

  ets:new(?EMPTY_CACHE, [public, named_table]),
  ets:new(?EMPTY_MEMO, [public, named_table]),

  % memoize ANY as not empty
  {ty_ref, AnyId} = ty_rec:any(),
  ets:insert(?EMPTY_CACHE, {AnyId, false}),

  % memoize EMPTY as empty
  {ty_ref, EmptyId} = ty_rec:empty(),
  ets:insert(?EMPTY_CACHE, {EmptyId, true})
.

-spec setup_ets() -> ok.
setup_ets() ->
  spawn(fun() ->
    % spawns a new process that is the owner of the variable id ETS table
    lists:foreach(fun(Tab) -> ets:new(Tab, [public, named_table]) end, all_tables()),
    ets:insert(?TY_UTIL, {ty_number, 0}),

    % define ANY node once
    ok = define_any(),

    % memoize ANY as not empty
    {ty_ref, AnyId} = ty_rec:any(),
    ets:insert(?EMPTY_CACHE, {AnyId, false}),

    % memoize EMPTY as empty
    {ty_ref, EmptyId} = ty_rec:empty(),
    ets:insert(?EMPTY_CACHE, {EmptyId, true}),

    receive _ -> ok end
        end),
  ok.

any() -> {ty_ref, 0}.

define_any() ->
  Any = {ty_ref, 0},

  % create tuple top
  DnfVarTupleAny = dnf_var_ty_tuple:any(),

  % create function top
  DnfVarFunctionAny = dnf_var_ty_function:any(),

  % create interval top
  DnfVarIntervalAny = dnf_var_int:any(),

  % create atom top
  DnfVarAtomAny = dnf_var_ty_atom:any(),

  % union
  Ty1 = ty_rec:interval(DnfVarIntervalAny),
  Ty2 = ty_rec:tuple(DnfVarTupleAny),
  Ty3 = ty_rec:atom(DnfVarAtomAny),
  Ty4 = ty_rec:function(DnfVarFunctionAny),

  U1 = ty_rec:union(Ty1, Ty2),
  U2 = ty_rec:union(U1, Ty3),
  U = ty_rec:union(U2, Ty4),

  % define
  ty_ref:define_ty_ref(Any, ty_ref:load(U)),

  ok.

next_ty_id() ->
	ets:update_counter(?TY_UTIL, ty_number, {2, 1}).

new_ty_ref() ->
  {ty_ref, next_ty_id()}.

define_ty_ref({ty_ref, Id}, Ty) ->
  io:format(user, "Store: ~p :=~n~p~n", [Id, Ty]),
  ets:insert(?TY_UNIQUE_TABLE, {Ty, Id}),
  ets:insert(?TY_MEMORY, {Id, Ty}),
  {ty_ref, Id}.

load({ty_ref, Id}) ->
  %%  io:format(user, "LOOKUP ~p -> ~p ~n", [Id, Object]),
  [{Id, Ty}] = ets:lookup(?TY_MEMORY, Id),
  Ty.

store(Ty) ->
  Object = ets:lookup(?TY_UNIQUE_TABLE, Ty),
  case Object of
    [] ->
      Id = ets:update_counter(?TY_UTIL, ty_number, {2, 1}),
%%      io:format(user, "Store: ~p :=~n~p~n", [Id, Ty]),
      ets:insert(?TY_UNIQUE_TABLE, {Ty, Id}),
      ets:insert(?TY_MEMORY, {Id, Ty}),
      {ty_ref, Id};
    [{_, Id}] ->
%%      io:format(user, "o", []),
      {ty_ref, Id}
  end.

memoize({ty_ref, Id}) ->
  ets:insert(?EMPTY_MEMO, {Id, true}),
  ok.

is_empty_memoized({ty_ref, Id}) ->
  Object = ets:lookup(?EMPTY_MEMO, Id),
  case Object of
    [] -> miss;
    [{_, true}] -> true
  end.

is_empty_cached({ty_ref, Id}) ->
  Object = ets:lookup(?EMPTY_CACHE, Id),
  case Object of
    [] -> miss;
    [{_, Result}] ->
%%      io:format(user, "x", []),
      Result
  end.

store_is_empty_cached({ty_ref, Id}, Result) ->
  ets:insert(?EMPTY_CACHE, {Id, Result}),
  Result.

store_recursive_variable(Variable, Ty) ->
  ets:insert(?RECURSIVE_TABLE, {Variable, Ty}),
  ok.

check_recursive_variable(Variable) ->
  Object = ets:lookup(?RECURSIVE_TABLE, Variable),
  case Object of
    [] -> miss;
    [{_, Result}] -> Result
  end.
