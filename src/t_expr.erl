-module(t_expr).
-include_lib("typo/include/t.hrl").
-export([block/2, function/4, function_call/4, 'case'/3, match/3, try_catch/3]).

-type expr() :: fun().
-type exprs() :: list(expr()).

-type clause() :: {
  Pattern :: t:t(),
  Expr    :: exprs()
}.
-type clauses() :: list(clause()).

-type result() ::
  {t:t(), t:context()}.

-spec block(t:context(), exprs()) ->
  result().
block(Ctx, Exprs) ->
  {Result, NewCtx} = block_(Ctx, Exprs),
  % block doesn't expose bindings
  {
    t:normalize(NewCtx, Result),
    NewCtx#context{ bindings = Ctx#context.bindings }
  }.
block_(CtxAcc, [Expr]) ->
  Expr(CtxAcc); % only the last expr in block returns
block_(CtxAcc, [Expr|Exprs]) ->
  {Result, NewCtxAcc} = Expr(CtxAcc),
  case Result of
    none -> {Result, NewCtxAcc}; % TODO dead code warning
    _    -> block(NewCtxAcc, Exprs)
  end.

-spec function(atom(), atom(), list(t:t()), clauses()) ->
  result().
function(_Module, _Function, Args = {tuple, _ArgsTypes}, Clauses) ->
  Ctx =
    #context{
      bindings   = #{}
      % stacktrace = [{Module, Function, ArgsTypes}|Ctx#context.stacktrace]
    },
  {Result, _ResultCtx} =
    'case'(Ctx, Clauses, Args, function_clause),
  Result.
  % , Ctx#context{
  %   exceptions = ResultCtx#context.exceptions,
  %   messages   = ResultCtx#context.messages
  % }}.

-spec function_call(t:context(), atom(), atom(), list(t:t())) ->
  result().
function_call(Ctx, Module, Function, Args) ->
  case {Module, Function, Args} of
    {erlang, Class, [Reason]} when Class =:= exit; Class =:= error; Class =:= throw ->
      add_exception({Class, Reason, stacktrace(Ctx)}, Ctx);
    {erlang, error, [Reason, StacktraceArgs]} ->
      add_exception({error, Reason, stacktrace(Ctx, StacktraceArgs)}, Ctx);
    {erlang, raise, [Class, Reason, Stacktrace]} ->
      add_exception({Class, Reason, Stacktrace}, Ctx);
    {_, _, _} ->
      % TODO call
      ok
  end.

-spec 'case'(t:context(), clauses(), t:t()) ->
  result().
'case'(Ctx, Clauses, Value) ->
  'case'(Ctx, Clauses, Value, case_clause).

-spec 'case'(t:context(), clauses(), t:t(), atom()) ->
  result().
'case'(Ctx, Clauses, Value, ExceptionTag) ->
  case case_(Ctx, Clauses, Value, []) of
    {[], NewCtx} ->
      {none, add_exception({error, {ExceptionTag, Value}, NewCtx#context.stacktrace}, NewCtx)};
    {UnionTypes, NewCtx} ->
      {t:union(UnionTypes), NewCtx}
  end.

-spec case_(t:context(), clauses(), t:t(), list(t:t())) ->
  {list(t:t()), t:context()}.
case_(Ctx, [], _, UnionTypes) ->
  {UnionTypes, Ctx};
case_(Ctx = #context{ bindings = Bindings }, [{Pattern, Exprs}|Clauses], Value, UnionTypes) ->
  case t:match(Ctx, Pattern, Value) of
    {ok, ClauseCtx} ->
      {UnionType, NewCtx} = block(ClauseCtx, Exprs),
      case_(NewCtx#context{ bindings = Bindings }, Clauses, Value, [UnionType|UnionTypes]);
    {{error, _}, NewCtx} ->
      % TODO ensure binding after unsuccessful match does not change
      case_(NewCtx, Clauses, Value, UnionTypes)
  end.

-spec match(t:context(), t:t(), t:t()) ->
  result().
match(Ctx, Pattern, Value) ->
  {Status, NewCtx} =
    t:match(Ctx, Pattern, Value),
  case Status of
     ok        -> {Value, NewCtx};
    {error, _} -> {none , add_exception({error, {badmatch, Value}, NewCtx#context.stacktrace}, NewCtx)}
  end.

-spec try_catch(t:context(), expr(), clauses()) ->
  result().
try_catch(Ctx0, TryExpr, CatchClauses) ->
  {TryResult, Ctx1} = TryExpr(Ctx0),
  {CatchResult, Unhandled, Ctx2} =
    'catch'(Ctx1#context{exceptions = []}, CatchClauses, Ctx1#context.exceptions, [], []),
  Result = t:union([TryResult|CatchResult]),
  Ctx3   = Ctx2#context{exceptions = Ctx2#context.exceptions ++ Unhandled},
  {Result, Ctx3}.

-spec 'catch'(t:context(), clauses(), t:exceptions(), t:exceptions(), t:ts()) ->
  {t:exceptions(), t:ts(), t:context()}.
'catch'(Ctx, _, [], Unhandled, Results) ->
  {lists:reverse(Unhandled), lists:reverse(Results), Ctx};
'catch'(Ctx, CatchClauses, [Exception|Exceptions], Unhandled, Results) ->
  case 'case'(Ctx, CatchClauses, Exception, []) of
    {none  , NewCtx} -> 'catch'(NewCtx, CatchClauses, Exceptions, [Exception|Unhandled], Results);
    {Result, NewCtx} -> 'catch'(NewCtx, CatchClauses, Exceptions, Unhandled, [Result|Results]   )
  end.

% TODO
% catch
% receive
% list comprehension
% ...

add_exception(Exception, Ctx = #context{exceptions = Exceptions}) ->
  Ctx#context{
    exceptions = [Exception|Exceptions]
  }.

stacktrace(#context{exceptions = Stacktrace}) ->
  Stacktrace.
stacktrace(#context{exceptions = Stacktrace}, _Args) ->
  % TODO
  Stacktrace.
