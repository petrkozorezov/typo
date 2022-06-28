-module(t_pt_type_funs).
-include_lib("astranaut/include/quote.hrl").

-export([parse_transform/2]).

-record(state, {
  module         :: module(),
  functions = [] :: list({atom(), arity()})
}).
-type state() :: #state{}.

-spec parse_transform([erl_parse:abstract_form()], compile:option()) ->
  [erl_parse:abstract_form()].
parse_transform(Forms, _Options) ->
  % Forms.
  {NewForms, _} = lists:mapfoldl(fun transform/2, #state{}, Forms),
  lists:flatten(NewForms).

transform(Attr = {attribute, _, module, Module}, State) ->
  {Attr, State#state{module = Module}};
transform(Attr = {attribute, _, typo_funs, Funs}, State) ->
  {Attr, State#state{functions = State#state.functions ++ Funs}};
transform(Function = {function, _, Name, Arity, _}, State) ->
  case lists:member({Name, Arity}, State#state.functions) of
    true  -> {[function(Function)], State};
    false -> {[         Function ], State}
  end;
transform(Form, State) ->
  {Form, State}.

%%

function({function, Pos, Name, Arity, Clauses}) ->
  {function, Pos, Name, Arity, lists:map(fun type_clause/1, Clauses)}.

type_clause({clause, Pos, Args, Guards, Exprs}) ->
  {clause, Pos, type_exprs(Args), Guards, type_exprs(Exprs)}.

-spec type_exprs(list(erl_parse:abstract_expr())) ->
  list(t:type()).
type_exprs(Exprs) ->
  lists:map(fun type_expr/1, Exprs).

-spec type_expr(erl_parse:abstract_expr()) ->
  t:type().
% type_expr(quote = _V@Var) ->
%   quote({var, _A@Var});
type_expr(quote = _A@Atom) ->
  quote({atom, _A@Atom});
type_expr(quote = _I@Integer) ->
  quote({integer, _I@Integer});
type_expr(quote = _F@Float) ->
  quote({float, _F@Float});
type_expr(quote = []) ->
  quote([]);
type_expr(quote = [_@Head | _@Tail]) ->
  HeadT = type_expr(Head),
  TailT = type_expr(Tail),
  quote({cons, _@HeadT, _@TailT});
type_expr({tuple, _, Values}) ->
  ValuesT = t_pt_utils:list_literal(type_exprs(Values)),
  quote({tuple, _@ValuesT});
type_expr({match, _, {atom, _, 't:untype'}, Expr}) ->
  [UntypeExpr] = astranaut:smap(fun untype_expr/1, [Expr], #{}),
  UntypeExpr;
type_expr({match, Pos, Pattern, Value}) ->
  {match, Pos, type_expr(Pattern), type_expr(Value)};
type_expr({'case', Pos, Value, Clauses}) ->
  {'case', Pos, type_expr(Value), lists:map(fun type_clause/1, Clauses)};
type_expr({call, _, {remote, _, {atom, _, t}, {atom, _, untype}}, [Expr]}) ->
  [UntypeExpr] = astranaut:smap(fun untype_expr/1, [Expr], #{}),
  UntypeExpr;
type_expr({call, Pos, Func, Args}) ->
  {call, Pos, type_call(Func), type_exprs(Args)};
type_expr({'try', Pos, Exprs, Clauses, CatchClauses, After}) ->
  {'try', Pos,
    type_exprs(Exprs),
    lists:map(fun type_clause/1, Clauses),
    lists:map(fun type_clause/1, CatchClauses),
    type_exprs(After)
  };
type_expr({block, Pos, Exprs}) ->
  {block, Pos, type_exprs(Exprs)};
type_expr(Expr) ->
  io:format("~p: unknown expr: ~p~n", [?MODULE, Expr]),
  Expr.

type_call({remote, Pos, Module, Function}) ->
  {remote, Pos, type_expr(Module), type_expr(Function)};
type_call(Func) ->
  type_expr(Func).

untype_expr({match, _, {atom, _, 't:type'}, Expr}) ->
  type_expr(Expr);
untype_expr({call, _, {remote, _, {atom, _, t}, {atom, _, type}}, [Expr]}) ->
  type_expr(Expr);
untype_expr(Node) ->
  Node.
