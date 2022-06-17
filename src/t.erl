-module(t).
-include_lib("typo/include/t.hrl").
-export([match/4, as/2, 'case'/4, normalize/3]).

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


-type options() :: #options{}.
-type context() :: #context{}.
-type bindings() :: #{ var_name() => t() }.
-type var_name() :: atom().
-type error() :: reason().
% -type error() :: {erl_anno:anno(), reason()}.
-type reason() :: any(). % TODO specify


-type match_result() :: {ok | error, context()}.
-spec match(options(), context(), t(), t()) ->
  match_result().
match(Opts, Ctx, VarType = {var, _}, Type) ->
  match_var(Opts, Ctx, VarType, Type);
match(Opts, Ctx, Type, VarType = {var, _}) ->
  match_var(Opts, Ctx, Type, VarType);

match(_, Ctx, Type, Type) ->
  {ok, Ctx};

match(_, Ctx, any, _) ->
  {ok, Ctx};
match(Opts, Ctx, Type, AnyOrNone) when AnyOrNone =:= any; AnyOrNone =:= none -> % Really???
  case Type of
    {list, Type_}      -> match(Opts, Ctx, Type_, AnyOrNone);
    {cons, Head, Tail} -> match_all(Opts, Ctx, [{Head, AnyOrNone}, {Tail, AnyOrNone}]);
    {tuple, Types}     -> match_all(Opts, Ctx, [{Type_, AnyOrNone} || Type_ <- Types]);
    {union, Types}     -> match_all(Opts, Ctx, [{Type_, AnyOrNone} || Type_ <- Types]);
    {'fun', Args, Ret} -> match_all(Opts, Ctx, [{{tuple, Args}, AnyOrNone}, {Ret, AnyOrNone}]);
    _                  -> {ok, Ctx} % primitive types
  end;

match(Opts, Ctx, {list, TypeA}, {list, TypeB}) ->
  match(Opts, Ctx, TypeA, TypeB);
match(Opts, Ctx, {list, Type}, {cons, Head, Tail}) ->
  union_match_all(Opts, Ctx, [{Type, Head}, {{list, Type}, Tail}], []);
match(Opts, Ctx, {cons, HeadA, TailA}, {cons, HeadB, TailB}) ->
  match_all(Opts, Ctx, [{HeadA, HeadB}, {TailA, TailB}]);
match(_, Ctx, {list, _}, nil) ->
  {ok, Ctx};

match(Opts, Ctx, tuple, {tuple, Types}) ->
  match(Opts, Ctx, {tuple, Types}, any);
match(Opts, Ctx, {tuple, TypesA}, {tuple, TypesB}) when length(TypesA) =:= length(TypesB) ->
  match_all(Opts, Ctx, lists:zip(TypesA, TypesB));

match(Opts, Ctx, Type, {union, UnionTypes}) ->
  union_match_all(Opts, Ctx, [{Type, UnionType} || UnionType <- UnionTypes], []);
match(Opts, Ctx, {union, UnionTypes}, Type) ->
  case union_match_any(Opts, Ctx, [{UnionType, Type} || UnionType <- UnionTypes], []) of
    OK = {ok, _}    -> OK;
    {error, NewCtx} -> error_result({badmatch, {union, UnionTypes}, Type}, NewCtx)
  end;

%%        foo(1 | 2    ) -> 1 | 2.
%% contr: foo(1        ) -> 1 | 2 | 3.
%% co:    foo(1 | 2 | 3) -> 1.
match(Opts, Ctx, {'fun', ArgsA, RetA}, {'fun', ArgsB, RetB}) ->
  case match(reverse(Opts), Ctx, {tuple, ArgsB}, {tuple, ArgsA}) of
    {ok, NewCtx}     -> match(Opts, NewCtx, RetA, RetB);
    Error={error, _} -> Error
  end;

% mb use word 'MetaType' instead of 'F'
match(Opts, Ctx, F, Type) when is_function(F, 0) ->
  match(Opts, Ctx, F(), Type);
match(Opts, Ctx, Type, F) when is_function(F, 0) ->
  match(Opts, Ctx, Type, F());

match(_, Ctx, atom, {atom, _}) ->
  {ok, Ctx};
match(_, Ctx, integer, {integer, _}) ->
  {ok, Ctx};
match(_, Ctx, float, {float, _}) ->
  {ok, Ctx};

match(_, Ctx, TypeA, TypeB) ->
  error_result({badmatch, TypeA, TypeB}, Ctx).


-spec error_result(error(), context()) ->
  {error, context()}.
error_result(Error, Ctx) ->
  NewCtx =
    Ctx#context{
      errors = [Error | Ctx#context.errors]
    },
  {error, NewCtx}.

-spec match_var(options(), context(), t(), t()) ->
  match_result().
match_var(Opts = #options{reverse_flag = Flag}, Ctx, TypeA, TypeB) ->
  case {Flag, TypeA, TypeB} of
    {false, {var, Var}, Type} -> match_var_bind_or_match(Opts, Ctx, Type, Var);
    {true , Type, {var, Var}} -> match_var_bind_or_match(Opts, Ctx, Type, Var);
    {false, Type, {var, Var}} -> match_var_resolve      (Opts, Ctx, Type, Var);
    {true , {var, Var}, Type} -> match_var_resolve      (Opts, Ctx, Type, Var)
  end.

-spec match_var_bind_or_match(options(), context(), t(), var_name()) ->
  match_result().
match_var_bind_or_match(Opts, Ctx = #context{bindings = Bindings}, Type, Var) ->
  case maps:find(Var, Bindings) of
    {ok, VarValue} -> match(Opts, Ctx, VarValue, Type);
    error          -> {ok, Ctx#context{bindings = bind_var(Bindings, Var, Type)}}
  end.

match_var_resolve(Opts, Ctx, Type, Var) ->
  case maps:find(Var, Ctx#context.bindings) of
    {ok, VarValue} -> match(Opts, Ctx, Type, VarValue);
    error          -> error_result({'unknown var', Var}, Ctx)
  end.

-spec bind_var(bindings(), var_name(), t()) ->
  bindings().
bind_var(Bindings, '_', _) ->
  Bindings;
bind_var(Bindings, Var, Type) ->
  false = maps:is_key(Var, Bindings), % assert
  maps:put(Var, Type, Bindings).

-spec match_all(options(), context(), list({t(), t()})) ->
  match_result().
match_all(_, Ctx, []) ->
  {ok, Ctx};
match_all(Opts, Ctx, [{TypeA, TypeB}|Types]) ->
  case match(Opts, Ctx, TypeA, TypeB) of
    Error={error, _}  -> Error;
    {ok, NewCtx} -> match_all(Opts, NewCtx, Types)
  end.

-spec union_match_any(options(), context(), list({t(), t()}), context()) ->
  match_result().
union_match_any(_, Ctx, [], []) ->
  {error, Ctx};
union_match_any(_, _, [], CtxsAcc) ->
  {ok, union_contexts_all(lists:reverse(CtxsAcc))};
union_match_any(Opts, Ctx, [{TypeA, TypeB}|Types], CtxsAcc) ->
  NewCtxsAcc =
    case match(Opts, Ctx, TypeA, TypeB) of
      {ok, SubCtx} -> [SubCtx|CtxsAcc];
      {error, _  } -> CtxsAcc
    end,
  union_match_any(Opts, Ctx, Types, NewCtxsAcc).

-spec union_match_all(options(), context(), list({t(), t()}), context()) ->
  match_result().
union_match_all(_, _, [], CtxsAcc) ->
  {ok, union_contexts_all(lists:reverse(CtxsAcc))};
union_match_all(Opts, Ctx, [{TypeA, TypeB}|Types], CtxsAcc) ->
  case match(Opts, Ctx, TypeA, TypeB) of
    {ok, SubCtx}     -> union_match_all(Opts, Ctx, Types, [SubCtx|CtxsAcc]);
    Error={error, _} -> Error
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
  #context{bindings = BindingsA, errors = ErrorsA},
  #context{bindings = BindingsB, errors = ErrorsB}
) ->
  #context{
    bindings = union_bindings(BindingsA, BindingsB),
    errors   = ErrorsA ++ ErrorsB
  }.

-spec union_bindings(bindings(), bindings()) ->
  bindings().
union_bindings(BindingsA, BindingsB) ->
  maps:merge_with(
    fun(_, TypeA, TypeB) ->
      union(TypeA, TypeB)
    end,
    BindingsA,
    BindingsB
  ).


%%
%% Sum of two types
%%
-spec union(t(), t()) ->
  t().
union(TypeA, TypeB) ->
  UnionTypes =
    case {TypeA, TypeB} of
      {{union, TypesA}, {union, TypesB}} -> TypesA ++ TypesB;
      {{union, Types }, Type           } -> Types  ++ [Type];
      {Type           , {union, Types }} -> [Type  |  Types];
      {_              , _              } -> [TypeA ,  TypeB]
    end,
  fold_union(UnionTypes, []).

-spec fold_union(list(t()), list(t())) ->
  none | any | {union, list(t())}.
fold_union([                   ], []    ) -> none;
fold_union([                   ], [Type]) -> Type;
fold_union([                   ], Acc   ) -> {union, lists:reverse(Acc)};
fold_union([ any          |_   ], _     ) -> any;
fold_union([ none         |Tail], Acc   ) -> fold_union(Tail, Acc);
fold_union([{union, Types}|Tail], Acc   ) -> fold_union(Types ++ Tail, Acc);
fold_union([ Head         |Tail], Acc   ) ->
  NewAcc =
    case lists:member(Head, Acc) of
      true  -> Acc;
      false -> [Head|Acc]
    end,
  fold_union(Tail, NewAcc).

%%
%% converts to  type
%%
as(_From, _To) ->
  erlang:error(pt_stub).

-spec 'case'(options(), context(), list({t(), fun((context()) -> {t(), context()})}), bindings()) ->
  {t(), context()}.
'case'(Opts, Ctx, ValueType, Cases) ->
  Bindings = Ctx#context.bindings,
  {UnionTypes, NewCtx} =
    lists:foldl(
      fun({ClauseType, Fun}, {UnionTypesAcc, CtxsAcc}) ->
        case match(Opts, CtxsAcc#context{bindings = Bindings}, ClauseType, ValueType) of
          {ok, NewCtx} ->
            {UnionType, NewCtxAcc} = Fun(NewCtx#context{errors = CtxsAcc#context.errors}),
            {[UnionType|UnionTypesAcc], NewCtxAcc};
          {error, NewCtxAcc} ->
            {UnionTypesAcc, NewCtxAcc#context{errors = CtxsAcc#context.errors}}
        end
      end,
      {[], Ctx},
      Cases
    ),
  {
    fold_union(UnionTypes, []),
    NewCtx#context{bindings = Bindings}
  }.

-spec normalize_all(options(), context(), list(t())) ->
  list(t()).
normalize_all(Opts, Ctx, Types) ->
  lists:map(fun(Type) -> normalize(Opts, Ctx, Type) end, Types).

-spec normalize(options(), context(), t()) ->
  t().
normalize(Opts, Ctx, {list, Type}) ->
  {list, normalize(Opts, Ctx, Type)};
normalize(Opts, Ctx, {cons, Head, Tail}) ->
  {cons, normalize(Opts, Ctx, Head), normalize(Opts, Ctx, Tail)};
normalize(Opts, Ctx, {tuple, Types}) ->
  {tuple, normalize_all(Opts, Ctx, Types)};
normalize(Opts, Ctx, {union, Types}) ->
  fold_union(normalize_all(Opts, Ctx, Types), []);
normalize(Opts, Ctx, {'fun', Args, Ret}) ->
  {'fun', normalize_all(Opts, Ctx, Args), normalize(Opts, Ctx, Ret)};
normalize(Opts, Ctx, Type = {var, Var}) ->
  case maps:find(Var, Ctx#context.bindings) of
    {ok, V} -> normalize(Opts, Ctx, V);
     error  -> Type
  end;
normalize(_, _, Type) ->
  Type.

reverse(Opts = #options{reverse_flag = Reverse}) ->
  Opts#options{reverse_flag = not Reverse}.
