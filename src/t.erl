-module(t).
-include_lib("macro_pipe/include/macro_pipe.hrl").
-export([match/2, match/3, match/4, as/2, norm/2, union/1, eval_function/2, eval/2]).
-export_type([t/0, bindings/0, var_name/0, exception/0, exceptions/0, stacktrace/0, match_stack/0]).

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
  | {exception, exception()}
.
-type ts() :: list(t()).

-type bindings() :: #{ var_name() => t() } | bindings().
-type var_name() :: term().
-type exception() :: {exception_class(), exception_reason(), stacktrace()}.
-type exception_class() :: exit | error | throw.
-type exception_reason() :: term().
-type exceptions() :: list(exception()).
-type stacktrace() :: list(term()). % TODO refine
-type match_stack() :: list({t(), t()}).

-type match_state_result() :: {ok, state()} | {error, match_stack()}.
-type match_result() :: {ok, bindings()} | {error, match_stack()}.

-type cmp() ::
    '>=' % super type
  | '<=' % sub type
  | '==' % equal type
.

-record(state, {
  bindings = #{}  :: bindings(),
  stack    = []   :: match_stack(),
  cmp      = '>=' :: cmp()
}).
-type state() :: #state{}.

% check_specs(Fun) ->
%   PDs = [exceptions, messages],
%   [erlang:put(PD, []) || PD <- PDs],
%   try
%     _ = Fun(),
%     erlang:get(messages)
%   after
%     [erlang:put(PD, []) || PD <- PDs]
%   end.

-spec match(t(), t()) ->
  match_result().
match(A, B) ->
  match(#{}, '>=', A, B).

-spec match(bindings(), cmp(), t(), t()) ->
  match_result().
match(Bindings, Cmp, A, B) ->
  State = #state{
    bindings = Bindings,
    cmp      = Cmp
  },
  case match(State, A, B) of
    {ok, NewState}     -> {ok, NewState#state.bindings};
    Error = {error, _} -> Error
  end.

-spec match(state(), t(), t()) ->
  match_state_result().
match(State, A, B) ->
  Stack = State#state.stack,
  ?pipe([State||
    {ok, '_'} <- match_('_'#state{stack = [{'_'#state.cmp, A, B}|Stack]}, A, B),
    {ok, '_'#state{stack = Stack}}
  ]).

-define(cmp(Cmp, State), State#state.cmp == Cmp).
match_(State, A, B) when is_list(A); is_list(B) ->
  match_all(State, A, B);
match_(State = #state{bindings = Bindings}, {var, Var}, Type) ->
  case maps:find(Var, Bindings) of
    {ok, VarValue} -> match(State, VarValue, Type);
    error          -> {ok, State#state{bindings = bind(Bindings, Var, Type)}}
  end;
match_(State, Type, {var, Var}) ->
  case maps:find(Var, State#state.bindings) of
    {ok, VarValue} -> match(State, Type, VarValue);
     error         -> match_error(State)
  end;

match_(State, Type, Type) ->
  {ok, State};

match_(State, any, _) ->
  {ok, State};
match_(State, Type, AnyOrNone) when AnyOrNone =:= any; AnyOrNone =:= none -> % Really???
  case Type of
    {list, Type_}      -> match(State, Type_, AnyOrNone);
    {cons, Head, Tail} -> match_all(State, [Head, Tail], AnyOrNone);
    {tuple, Types}     -> match_all(State, Types       , AnyOrNone);
    {union, Types}     -> match_all(State, Types       , AnyOrNone);
    {'fun', Args, Ret} -> match_all(State, [Args, Ret] , AnyOrNone);
    _                  -> {ok, State} % primitive types
  end;

match_(State, {list, TypeA}, {list, TypeB}) ->
  match(State, TypeA, TypeB);
match_(State, ListType = {list, Type}, {cons, Head, Tail}) ->
  union_match_all(State, [Type, ListType], [Head, Tail]);
match_(State, {cons, HeadA, TailA}, {cons, HeadB, TailB}) ->
  match_all(State, [HeadA, TailA], [HeadB, TailB]);
match_(State, {list, _}, nil) when ?cmp('>=', State) ->
  {ok, State};
match_(State, nil, {list, _}) when ?cmp('<=', State) ->
  {ok, State};

match_(State, tuple, Tuple = {tuple, _}) when ?cmp('>=', State)  ->
  match(State, Tuple, any); % just bind vars to any
match_(State, Tuple = {tuple, _}, tuple) when ?cmp('<=', State)  ->
  match(State, Tuple, any);
match_(State, {tuple, TypesA}, {tuple, TypesB}) when length(TypesA) =:= length(TypesB) ->
  match_all(State, TypesA, TypesB);

match_(State, Type, {union, UnionTypes}) when ?cmp('>=', State) ->
  union_match_all(State, Type, UnionTypes);
match_(State, {union, UnionTypes}, Type) when ?cmp('<=', State) ->
  union_match_all(State, Type, UnionTypes);
match_(State, {union, UnionTypes = [_|_]}, Type) when ?cmp('>=', State) ->
  union_match_any(State, UnionTypes, Type);
match_(State, Type, {union, UnionTypes = [_|_]}) when ?cmp('<=', State) ->
  union_match_any(State, UnionTypes, Type);

%%        foo(1 | 2    ) -> 1 | 2.
%% contr: foo(1        ) -> 1 | 2 | 3.
%% co:    foo(1 | 2 | 3) -> 1.
match_(State, {'fun', ArgsA, RetA}, {'fun', ArgsB, RetB}) ->
  % TODO error details with revert is hard to understand
  case match_all(reverse(State), ArgsA, ArgsB) of
    {ok, NewState}     -> match(reverse(NewState), RetA, RetB);
    Error = {error, _} -> Error
  end;

% mb use word 'MetaType' instead of 'F'
match_(State, F, Type) when is_function(F, 0) ->
  match(State, F(), Type);
match_(State, Type, F) when is_function(F, 0) ->
  match(State, Type, F());

match_(State, atom        , {atom, _}   ) when ?cmp('>=', State) -> {ok, State};
match_(State, {atom, _}   , atom        ) when ?cmp('<=', State) -> {ok, State};
match_(State, integer     , {integer, _}) when ?cmp('>=', State) -> {ok, State};
match_(State, {integer, _}, integer     ) when ?cmp('<=', State) -> {ok, State};
match_(State, float       , {float, _}  ) when ?cmp('>=', State) -> {ok, State};
match_(State, {float, _}  , float       ) when ?cmp('<=', State) -> {ok, State};

match_(State, _, _) ->
  match_error(State).

match_error(State) ->
  {error, lists:reverse(State#state.stack)}.


-spec match_all(state(), t() | ts(), t() | ts()) ->
  match_state_result().
match_all(State, A, B) ->
  match_lists(State, A, B, fun match_pair/6, fun match_all/3, {ok, State}).

match_lists(_, [], [], _, _, Default) ->
  Default;
match_lists(State, [HA|TA], [HB|TB], MatchF, F1, _) ->
  MatchF(State, HA, HB, TA, TB, F1);

match_lists(_, _, [], _, _, Default) ->
  Default;
match_lists(State, A, [HB|TB], MatchF, F1, _) ->
  MatchF(State, A, HB, A, TB, F1);

match_lists(_, [], _, _, _, Default) ->
  Default;
match_lists(State, [HA|TA], B, MatchF, F1, _) ->
  MatchF(State, HA, B, TA, B, F1);

match_lists(State, A, B, _, _, _) ->
  match(State, A, B).


match_pair(State, A1, B1, A2, B2, F) ->
  ?pipe([State ||
    {ok, '_'} <- match('_', A1, B1),
    F('_', A2, B2)
  ]).

-spec bind(bindings(), var_name(), t()) ->
  bindings().
bind(Bindings, '_', _) ->
  Bindings;
bind(Bindings, Var, Type) ->
  false = maps:is_key(Var, Bindings), % assert
  maps:put(Var, Type, Bindings).

-spec bind_all(bindings(), list(var_name()), ts()) ->
  bindings().
bind_all(Bindings, [], []) ->
  Bindings;
bind_all(Bindings, [Var|Vars], [Type|Types]) ->
  bind_all(bind(Bindings, Var, Type), Vars, Types).

-spec union_match_any(state(), t() | list(t()), t() | list(t())) ->
  match_state_result().
union_match_any(State, A, B) ->
  match_lists(State, A, B, fun union_match_any_pair/6, fun union_match_any/3, match_error(State)).

union_match_any_pair(State, A1, B1, A2, B2, F) ->
  Result1 = match(State, A1, B1),
  Result2 = F(State, A2, B2),
  case {Result1, Result2} of
    {{ok, NewState1}, {ok, NewState2}} -> {ok, union_states(NewState1, NewState2)};
    {_              , Ok = {ok, _}   } -> Ok;
    {Ok = {ok, _}   , _              } -> Ok;
    {Error          , _              } -> Error
  end.

-spec union_match_all(state(), t() | list(t()), t() | list(t())) ->
  match_state_result().
union_match_all(State, A, B) ->
  match_lists(State, A, B, fun union_match_all_pair/6, fun union_match_all/3, {ok, State}).

union_match_all_pair(State, A1, B1, A2, B2, F) ->
  ?pipe([ok ||
    {ok, NewState1} <- match(State, A1, B1),
    {ok, NewState2} <- F(State, A2, B2),
    {ok, union_states(NewState1, NewState2)}
  ]).

-spec union_states(bindings(), bindings()) ->
  bindings().
union_states(
  #state{bindings = BindingsA},
  #state{bindings = BindingsB}
) ->
  #state{
    bindings = union_bindings(BindingsA, BindingsB)
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
% * сумма tuple
%  * если все поля кроме одного совпадают, то его можно проссумировать
%  * {1, 2} | {1, 3} => {1, 2 | 3}
%  * {2, 2} | {1, 3} =/> {2 | 1, 2 | 3}
% map, functions (и другие?) аналогично
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

-spec norm_all(bindings(), ts()) ->
  ts().
norm_all(Bindings, Types) ->
  lists:map(fun(Type) -> norm(Bindings, Type) end, Types).

%% TODO traverse
-spec norm(bindings(), t()) ->
  t().
norm(Bindings, {list, Type}) ->
  {list, norm(Bindings, Type)};
norm(Bindings, {cons, Head, Tail}) ->
  {cons, norm(Bindings, Head), norm(Bindings, Tail)};
norm(Bindings, {tuple, Types}) ->
  {tuple, norm_all(Bindings, Types)};
norm(Bindings, {union, Types}) ->
  union(norm_all(Bindings, Types), []);
norm(Bindings, {'fun', Args, Ret}) ->
  {'fun', norm_all(Bindings, Args), norm(Bindings, Ret)};
norm(Bindings, Type = {var, Var}) ->
  case maps:find(Var, Bindings) of
    {ok, V} -> norm(Bindings, V);
     error  -> Type
  end;
norm(_, Type) ->
  Type.


-spec reverse(state()) -> state().
reverse(State) ->
  State#state{cmp = reverse_cmp(State#state.cmp)}.

-spec
reverse_cmp(cmp()) -> cmp().
reverse_cmp('>=' ) -> '<=';
reverse_cmp('<=' ) -> '>=';
reverse_cmp('==' ) -> '=='.


% https://www.it.uu.se/research/group/hipe/cerl/doc/core_erlang-1.0.3.pdf
% https://github.com/erlang/otp/blob/41f5ee8cf217d10bd3a736a948d5335f591a2b85/lib/dialyzer/src/dialyzer_typesig.erl#L214
% https://8thlight.com/blog/kofi-gumbs/2017/05/02/core-erlang.html
-type expr() ::
    {value, t() | ts()}
  | {'let', Vars::list({var, var_name()}), Value::expr(), Body::expr()}
  | {match, Pattern::t(), Value::(t() | ts()), Body::expr()}
  | {'case', Value::(t() | ts()), list({t(), expr()})}
.

-spec eval_function(bindings(), expr()) ->
  t().
eval_function(B, Body) ->
  OldExcs = pd_swap(exceptions, []),
  Result  = eval(B, Body),
  NewExcs = pd_swap(exceptions, OldExcs),
  union([Result | [{exception, Exc} || Exc <- NewExcs]]).

% % call_function(F) ->
% %   Result = F(),
% %   {Regular, Excs} =
% %     separate_excs(Result),
% %   lists:foreach(fun exception/1, Excs),
% %   Regular.

% separate_excs({union, UnionTypes}) ->
%   {Excs, Regular} lists:partition(fun is_exception/1, UnionTypes),
%   {union(Regular), Excs};
% separate_excs(Exc = {exception, _}) ->
%   {none, [Exc]};
% separate_excs(Regular) ->
%   {Regular, []}.

% is_exception({exception, _}) -> true;
% is_exception(false         ) -> false.

% eval core erlang expressions
-spec eval(bindings(), expr()) ->
  t().
eval(B, {value, Val}) -> % pseudo core expression
  norm(B, Val);
eval(B, {'let', Vs, Expr1, Expr2}) ->
  case eval(B, Expr1) of
    none -> message({'dead code', Expr2}), none;
    Rs   -> eval(bind_all(Vs, Rs, B), Expr2)
  end;
eval(B, {match, Ps, Vals, Expr}) -> % pseudo core expression
  case match(B, '>=', Ps, Vals) of
    {ok   , NB} -> eval(NB, Expr);
    {error, _ } -> none
  end;
eval(B, {'case', Val, Clauses}) ->
  union_eval([
    {B, {match, Pats, Val, Expr}} || {Pats, Expr} <- Clauses
  ]);

% eval(B, {'try', Expr1, Vs, Expr2, CatchVs, CatchExpr}) ->
%   Exceptions    = pop(exceptions),
%   {Rs, TryExcs} = eval(Expr, B, []),
%   TryExceptions = pop(exceptions),
%   push(exceptions, Exceptions),
%   union_eval([
%     {bind_all(Vs, Rs, B), Expr2}
%     | [{bind_all(CatchVs, TryExc, B), CatchExpr} || TryExc <- TryExcs]
%   ]);
% eval(B, {call, erlang, throw, [Arg]}) ->
%   exception(throw, Arg),
%   none;
% eval(B, {call, M, F, Args}) ->
%   try   erlang:apply(M, F, t:norm(B, Args))
%   catch error:undef -> exception(error, undef)
%   end;
% eval(B, {apply, {'fun', ArgsA, Ret}, ArgsB}) ->
%   ArityA = erlang:length(ArgsA),
%   ArityB = erlang:length(ArgsB),
%   case ArityA =:= ArityB of
%     true ->
%       case match_all(ArgsA, ArgsB, Ret) of
%         {ok, NB} ->
%           norm(NB, Ret);
%         {error, Reason} ->
%           message({'fun spec violated', Reason}),
%           none
%       end;
%     false ->
%       exception(error, {badarity, {F, [ArityA, ArityB]}})
%   end;
% eval(B, {apply, F, Args}) when is_atom(F); is_function(F) ->
%   % TODO badarity
%   try
%     erlang:apply(F, t:norm(B, Args))
%   catch
%     error:{badfun, F} -> exception(error, {badfun, F})
%   end;
% %% raise
eval(B, {primop, match_fail, [Reason]}) ->
  exception(error, norm(B, Reason));
eval(_, Expr) ->
  erlang:error(badexpr, [Expr]).

-spec union_eval(list({bindings(), _Expr})) ->
  t().
union_eval([]) ->
  none;
union_eval([{Bindings, Expr}|TailExprs]) ->
  union([eval(Bindings, Expr), union_eval(TailExprs)]).

%%

-spec exception(exit | error | throw, term()) ->
  none.
exception(Class, Reason) ->
  put_one(exceptions, {Class, Reason, any}), % TODO refine
  none.

exception(Exc) ->
  put_one(exceptions, Exc),
  none.


-spec message(term()) ->
  _.
message(Msg) ->
  put_one(messages, Msg).

put_one(Key, Val) ->
  erlang:put(Key, [Val|get_all(Key)]).

get_all(Key) ->
  case erlang:get(Key) of
    undefined -> [];
    Val       -> Val
  end.

pop_one(Key) ->
  [Head|Tail] = get_all(Key),
  erlang:put(Key, Tail),
  Head.

pop(Key) ->
  Val = erlang:get(Key),
  erlang:erase(Key),
  Val.

pd_swap(Key, NewValue) ->
  CurValue = erlang:get(Key),
  erlang:put(Key, NewValue),
  CurValue.

% -spec traverse(fun((t()) -> t()), t()) ->
%   t().
% traverse(Fun, Type) ->
%   case Type of
%     {list, ListType} ->
%       {list, traverse(Fun, ListType)};
%     {cons, Head, Tail} ->
%       {cons, traverse(Fun, Head), traverse(Fun, Tail)};
%     {tuple, Types} ->
%       {tuple, traverse_all(Fun, Types)};
%     {union, Types} ->
%       union(traverse_all(Fun, Types));
%     {'fun', Args, Ret} ->
%       {'fun', traverse_all(Fun, Args), traverse(Fun, Ret)};
%     _ ->
%       Fun(Type)
%   end.

% -spec traverse_all(fun((t()) -> t()), ts()) ->
%   ts().
% traverse_all(Fun, Types) ->
%   [traverse(Fun, Type) || Type <- Types].

% -spec norm(bindings(), t()) ->
%   t().
% norm(Bindings, Type) ->
%   traverse(
%     fun
%       ({var, Var}) ->
%         case maps:find(Var, Bindings) of
%           {ok, V} -> norm(Bindings, V);
%            error  -> Type
%         end;
%       (Type_) ->
%         Type_
%     end,
%     Type
%   ).
