-module(t).
-include_lib("typo/include/t.hrl").
-export([match/2, match/3, as/2, normalize/2, union/1]).
-export_type([t/0, options/0, context/0, bindings/0, var_name/0, exception/0, exceptions/0, stacktrace/0, match_stack/0]).

-type t() ::
    any
  | none % 'never' in ts
  % | unknown ??
  | {list, t()}
  | {cons, t(), t()}
  |  nil
  |  tuple
  | {tuple, [t()]}
  | {union, [t()]}
  % | {map, #{t() => {t(), Requireness :: boolean()}}
  | {'fun', [t()], t()}
  |  atom
  | {atom, atom()}
  |  integer
  % | {integer, Cond::#{ from => integer(), to => integer() }} % TODO
  | {integer, integer()}
  |  float
  | {float, float()}
  % | string
  % | pid
  % | reference
  % | binary
  | {var, var_name()}
.
-type ts() :: list(t()).

-type options() :: #options{}.
-type context() :: #context{}.
-type bindings() :: #{ var_name() => t() }.
-type var_name() :: atom().
-type exception() :: {exit | error | throw, term(), stacktrace()}.
-type exceptions() :: list(exception()).
-type stacktrace() :: list(term()). % TODO refine
-type match_stack() :: list({t(), t()}).
% -type message() :: {erl_anno:anno(), reason()}.

-type match_result() :: {ok | {error, match_stack()}, context()}.

-spec match(t(), t()) ->
  ok | {error, match_stack()}.
match(A, B) ->
  {R, _} = match(#context{}, A, B),
  R.

-spec match(context(), t(), t()) ->
  match_result().
match(Ctx0 = #context{bindings = Bindings, match_stack = MatchStack}, A, B) ->
  {Result, Ctx1} = match_(Ctx0#context{match_stack = [{A, B}|MatchStack]}, A, B),
  Ctx2 =
    case Result of
       ok        -> Ctx1;
      {error, _} -> Ctx1#context{bindings = Bindings}
    end,
  {Result, Ctx2#context{match_stack = MatchStack}}.

match_(Ctx, VarType = {var, _}, Type) ->
  match_var(Ctx, VarType, Type);
match_(Ctx, Type, VarType = {var, _}) ->
  match_var(Ctx, Type, VarType);

match_(Ctx, Type, Type) ->
  {ok, Ctx};

match_(Ctx, any, _) ->
  {ok, Ctx};
match_(Ctx, Type, AnyOrNone) when AnyOrNone =:= any; AnyOrNone =:= none -> % Really???
  case Type of
    {list, Type_}      -> match(Ctx, Type_, AnyOrNone);
    {cons, Head, Tail} -> match_all(Ctx, [{Head, AnyOrNone}, {Tail, AnyOrNone}]);
    {tuple, Types}     -> match_all(Ctx, [{Type_, AnyOrNone} || Type_ <- Types]);
    {union, Types}     -> match_all(Ctx, [{Type_, AnyOrNone} || Type_ <- Types]);
    {'fun', Args, Ret} -> match_all(Ctx, [{{tuple, Args}, AnyOrNone}, {Ret, AnyOrNone}]);
    _                  -> {ok, Ctx} % primitive types
  end;

match_(Ctx, {list, TypeA}, {list, TypeB}) ->
  match(Ctx, TypeA, TypeB);
match_(Ctx, {list, Type}, {cons, Head, Tail}) ->
  union_match_all(Ctx, [{Type, Head}, {{list, Type}, Tail}], []);
match_(Ctx, {cons, HeadA, TailA}, {cons, HeadB, TailB}) ->
  match_all(Ctx, [{HeadA, HeadB}, {TailA, TailB}]);
match_(Ctx, {list, _}, nil) ->
  {ok, Ctx};

match_(Ctx, tuple, {tuple, Types}) ->
  match(Ctx, {tuple, Types}, any);
match_(Ctx, {tuple, TypesA}, {tuple, TypesB}) when length(TypesA) =:= length(TypesB) ->
  match_all(Ctx, lists:zip(TypesA, TypesB));

match_(Ctx, Type, {union, UnionTypes}) ->
  union_match_all(Ctx, [{Type, UnionType} || UnionType <- UnionTypes], []);
match_(Ctx, {union, UnionTypes = [_|_]}, Type) ->
  union_match_any(Ctx, [{UnionType, Type} || UnionType <- UnionTypes]);

%%        foo(1 | 2    ) -> 1 | 2.
%% contr: foo(1        ) -> 1 | 2 | 3.
%% co:    foo(1 | 2 | 3) -> 1.
match_(Ctx0, {'fun', ArgsA, RetA}, {'fun', ArgsB, RetB}) ->
  {Status, Ctx1} = match_all(reverse(Ctx0), lists:zip(ArgsB, ArgsA)),
  Ctx2 = reverse(Ctx1),
  % TODO error details with revert is hard to understand
  case Status of
    ok                 -> match(Ctx2, RetA, RetB);
    Error = {error, _} -> {Error, Ctx2}
  end;

% mb use word 'MetaType' instead of 'F'
match_(Ctx, F, Type) when is_function(F, 0) ->
  match(Ctx, F(), Type);
match_(Ctx, Type, F) when is_function(F, 0) ->
  match(Ctx, Type, F());

match_(Ctx, atom, {atom, _}) ->
  {ok, Ctx};
match_(Ctx, integer, {integer, _}) ->
  {ok, Ctx};
match_(Ctx, float, {float, _}) ->
  {ok, Ctx};

match_(Ctx, _, _) ->
  {{error, lists:reverse(Ctx#context.match_stack)}, Ctx}.


-spec match_var(context(), t(), t()) ->
  match_result().
match_var(Ctx, TypeA, TypeB) ->
  #options{reverse_flag = Flag} = Ctx#context.options,
  case {Flag, TypeA, TypeB} of
    {false, {var, Var}, Type} -> match_var_bind_or_match(Ctx, Type, Var);
    {true , Type, {var, Var}} -> match_var_bind_or_match(Ctx, Type, Var);
    {false, Type, {var, Var}} -> match_var_resolve      (Ctx, Type, Var);
    {true , {var, Var}, Type} -> match_var_resolve      (Ctx, Type, Var)
  end.

-spec match_var_bind_or_match(context(), t(), var_name()) ->
  match_result().
match_var_bind_or_match(Ctx = #context{bindings = Bindings}, Type, Var) ->
  case maps:find(Var, Bindings) of
    {ok, VarValue} -> match(Ctx, VarValue, Type);
    error          -> {ok, Ctx#context{bindings = bind_var(Bindings, Var, Type)}}
  end.

-spec match_var_resolve(context(), t(), var_name()) ->
  match_result().
match_var_resolve(Ctx, Type, Var) ->
  case maps:find(Var, Ctx#context.bindings) of
    {ok, VarValue} -> match(Ctx, Type, VarValue);
     error         -> {{error, lists:reverse(Ctx#context.match_stack)}, Ctx}
  end.

-spec bind_var(bindings(), var_name(), t()) ->
  bindings().
bind_var(Bindings, '_', _) ->
  Bindings;
bind_var(Bindings, Var, Type) ->
  false = maps:is_key(Var, Bindings), % assert
  maps:put(Var, Type, Bindings).

-spec match_all(context(), list({t(), t()})) ->
  match_result().
match_all(Ctx, []) ->
  {ok, Ctx};
match_all(Ctx, [{TypeA, TypeB}|Types]) ->
  case match(Ctx, TypeA, TypeB) of
    {ok, NewCtx} -> match_all(NewCtx, Types);
    Error        -> Error
  end.

%% TODO prettify
-spec union_match_any(context(), nonempty_list({t(), t()})) ->
  match_result().
union_match_any(Ctx, UnionTypes) ->
  union_match_any(Ctx, lists:reverse(UnionTypes), []).

-spec union_match_any(context(), list({t(), t()}), list(t:context())) ->
  match_result().
union_match_any(Ctx, [{TypeA, TypeB}|Types], CtxsAcc) ->
  {Status, SubCtx} = match(Ctx, TypeA, TypeB),
  NewCtxsAcc =
    case Status of
      ok -> [SubCtx|CtxsAcc];
      _  -> CtxsAcc
    end,
  case {Types, NewCtxsAcc} of
    {[], []} -> {Status, Ctx};
    {[], _ } -> {ok, union_contexts_all(NewCtxsAcc)};
    {_ , _ } -> union_match_any(Ctx, Types, NewCtxsAcc)
  end.

-spec union_match_all(context(), list({t(), t()}), context()) ->
  match_result().
union_match_all(_, [], CtxsAcc) ->
  {ok, union_contexts_all(lists:reverse(CtxsAcc))};
union_match_all(Ctx, [{TypeA, TypeB}|Types], CtxsAcc) ->
  case match(Ctx, TypeA, TypeB) of
    {ok, SubCtx} -> union_match_all(Ctx, Types, [SubCtx|CtxsAcc]);
    Error        -> Error
  end.

-spec union_contexts_all(list(bindings())) ->
  bindings().
union_contexts_all([H|T]) ->
  F =
    fun(Bindings, Acc) ->
      union_contexts(Acc, Bindings)
    end,
  lists:foldl(F, H, T).

-spec union_contexts(bindings(), bindings()) ->
  bindings().
union_contexts(
  #context{bindings = BindingsA, exceptions = ExceptionsA},
  #context{bindings = BindingsB, exceptions = ExceptionsB}
) ->
  #context{
    bindings   = union_bindings(BindingsA, BindingsB),
    exceptions = ExceptionsA ++ ExceptionsB % TODO unique
  }.

-spec union_bindings(bindings(), bindings()) ->
  bindings().
union_bindings(BindingsA, BindingsB) ->
  maps:merge_with(
    fun(_, TypeA, TypeB) ->
      union([TypeA, TypeB])
    end,
    BindingsA,
    BindingsB
  ).

%%
%% construct a union
%%
-spec union(ts()) ->
  none | any | {union, ts()}.
union(Ts) ->
  union(Ts, []).
-spec union(ts(), ts()) ->
  none | any | {union, ts()}.
union([                   ], []    ) -> none;
union([                   ], [Type]) -> Type;
union([                   ], Acc   ) -> {union, lists:reverse(Acc)};
union([ any          |_   ], _     ) -> any;
union([ none         |Tail], Acc   ) -> union(Tail, Acc);
union([{union, Types}|Tail], Acc   ) -> union(Types ++ Tail, Acc);
union([ Head         |Tail], Acc   ) ->
  NewAcc =
    case lists:member(Head, Acc) of
      true  -> Acc;
      false -> [Head|Acc]
    end,
  union(Tail, NewAcc).

%%
%% converts to type
%%
as(_From, _To) ->
  erlang:error(pt_stub).

-spec normalize_all(context(), ts()) ->
  ts().
normalize_all(Ctx, Types) ->
  lists:map(fun(Type) -> normalize(Ctx, Type) end, Types).

-spec normalize(context(), t()) ->
  t().
normalize(Ctx, {list, Type}) ->
  {list, normalize(Ctx, Type)};
normalize(Ctx, {cons, Head, Tail}) ->
  {cons, normalize(Ctx, Head), normalize(Ctx, Tail)};
normalize(Ctx, {tuple, Types}) ->
  {tuple, normalize_all(Ctx, Types)};
normalize(Ctx, {union, Types}) ->
  union(normalize_all(Ctx, Types), []);
normalize(Ctx, {'fun', Args, Ret}) ->
  {'fun', normalize_all(Ctx, Args), normalize(Ctx, Ret)};
normalize(Ctx, Type = {var, Var}) ->
  case maps:find(Var, Ctx#context.bindings) of
    {ok, V} -> normalize(Ctx, V);
     error  -> Type
  end;
normalize(_, Type) ->
  Type.

reverse(Ctx = #context{options = Opts = #options{reverse_flag = Reverse}}) ->
  Ctx#context{options = Opts#options{reverse_flag = not Reverse}}.
