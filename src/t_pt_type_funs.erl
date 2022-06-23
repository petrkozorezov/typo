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

%% for core transform
%%
%% TODO let(?), case, try, call(?), apply(?), primop
% % inline
% 'case'() ->
%   begin
%     % TODO warn('dead code')
%     {R1, V1} =
%       case t:match(Bindings, Pattern1, V0, Bounds1) of
%         {ok, NewValue, NewBindings} -> {t:normalize(Expr1), NewValue};
%         _                           -> {none, V0}
%       end
%     % ...
%     {Rn, Vn} = % error clause
%       case t:match(Bindings, Pattern1, V0, Bounds1) of
%         {ok, NewValue, NewBindings} -> {t:normalize(Expr1), NewValue};
%         _                           -> {none, V0}
%       end,
%     union([
%       R1,
%       %...
%       Rn
%     ])
%   end.

function({function, Pos, Name, Arity, Clauses}) ->
  {function, Pos, Name, Arity, lists:map(fun clause/1, Clauses)}.

-spec exprs(list(erl_parse:abstract_expr())) ->
  list(t:type()).
exprs(Exprs) ->
  lists:map(fun expr/1, Exprs).

-spec expr(erl_parse:abstract_expr()) ->
  t:type().
expr(quote = _V@Var) ->
  quote({var, _A@Var});
expr(quote = _A@Atom) ->
  quote({atom, _A@Atom});
expr(quote = _I@Integer) ->
  quote({integer, _I@Integer});
expr(quote = _F@Float) ->
  quote({float, _F@Float});
expr(quote = []) ->
  quote([]);
expr(quote = [_@Head | _@Tail]) ->
  HeadT = expr(Head),
  TailT = expr(Tail),
  quote({cons, _@HeadT, _@TailT});
expr({tuple, _, Values}) ->
  ValuesT = t_pt_utils:list_literal(exprs(Values)),
  quote({tuple, _@ValuesT});
expr({match, _, {atom, _, 't:untype'}, Expr}) ->
  [UntypeExpr] = astranaut:smap(fun expr_untype/1, [Expr], #{}),
  UntypeExpr;
expr({match, Pos, Pattern, Value}) ->
  {match, Pos, expr(Pattern), expr(Value)};
expr({'case', Pos, Value, Clauses}) ->
  {'case', Pos, expr(Value), lists:map(fun clause/1, Clauses)};
expr({call, _, {remote, _, {atom, _, t}, {atom, _, untype}}, [Expr]}) ->
  [UntypeExpr] = astranaut:smap(fun expr_untype/1, [Expr], #{}),
  UntypeExpr;
expr({call, Pos, Func, Args}) ->
  {call, Pos, call_fun(Func), exprs(Args)};
expr({'try', Pos, Exprs, Clauses, CatchClauses, After}) ->
  {'try', Pos,
    exprs(Exprs),
    lists:map(fun clause/1, Clauses),
    lists:map(fun clause/1, CatchClauses),
    exprs(After)
  };
expr({block, Pos, Exprs}) ->
  {block, Pos, exprs(Exprs)}.

clause({clause, Pos, Args, Guards, Exprs}) ->
  {clause, Pos, exprs(Args), Guards, exprs(Exprs)}.

call_fun({remote, Pos, Module, Function}) ->
  {remote, Pos, expr(Module), expr(Function)};
call_fun(Func) ->
  expr(Func).

expr_untype({match, _, {atom, _, 't:type'}, Expr}) ->
  call_fun(Expr);
expr_untype({call, _, {remote, _, {atom, _, t}, {atom, _, type}}, [Expr]}) ->
  call_fun(Expr);
expr_untype(Node) ->
  Node.
