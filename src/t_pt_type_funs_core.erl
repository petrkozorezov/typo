-module(t_pt_type_funs_core).
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
transform(Function = {function, _, Name, Arity, _}, State = #state{module = Module}) ->
  case lists:member({Name, Arity}, State#state.functions) of
    true  -> {[function(Function, Module)], State};
    false -> {[         Function         ], State}
  end;
transform(Form, State) ->
  {Form, State}.

%%

-define(CTX, {var, 0, '__Ctx'}).
function({function, Pos, Name, Arity, Clauses}, Module) ->
  Args = [
    {var, Pos, erlang:list_to_atom("__Arg" ++ erlang:integer_to_list(N))}
    || N <- lists:seq(1, Arity)
  ],
  {function, Pos, Name, Arity, [{clause, Pos, Args, [], [
    quote(
      t_expr:function(
        _A@Module, _A@Name,
        {tuple, unquote(t_pt_utils:list_literal(Args))},
        unquote(function_clauses(Clauses))
      )
    )
  ]}]}.

function_clauses([]) ->
  quote([]);
function_clauses([{clause, _, Args, _Guards, Exprs}|Clauses]) ->
  Ctx      = ?CTX,
  ClausesT = function_clauses(Clauses),
  quote([{{tuple, [_L@Args]}, [
    unquote_splicing([quote(fun(_@Ctx) -> _@Expr end) || Expr <- exprs(Exprs)])
  ]} | _@ClausesT]).

exprs(Exprs) ->
  lists:map(fun expr/1, Exprs).

expr({match, _, Pattern, Value}) ->
  Ctx = ?CTX,
  quote(
    t_expr:match(_@Ctx, unquote(Pattern), unquote(Value))
  );
expr({'case', _, Value, Clauses}) ->
  Ctx = ?CTX,
  quote(
    t_expr:'case'(_@Ctx, [unquote_splicing(clauses(Clauses))], unquote(Value))
  );
% expr({'try', Pos, Exprs, Clauses, CatchClauses, After}) ->
%   % TODO
%   {'try', Pos,
%     exprs(Exprs),
%     lists:map(fun clause/1, Clauses),
%     lists:map(fun clause/1, CatchClauses),
%     exprs(After)
%   };
expr({block, _, Exprs}) ->
  Ctx = ?CTX,
  ExprsFuns = [quote(fun(_@Ctx) -> unquote(Expr) end) || Expr <- exprs(Exprs)],
  quote(
    t_expr:block(_@Ctx, [_L@ExprsFuns])
  );
expr(Expr) ->
  Ctx = ?CTX,
  % quote({_@Expr, test}).
  quote({_@Expr, _@Ctx}).

clauses(Clauses) ->
  quote([unquote_splicing(lists:map(fun clause/1, Clauses))]).
clause({clause, _, [Pattern], _Guards, Exprs}) ->
  Ctx = ?CTX,
  quote({
    unquote(Pattern),
    [unquote_splicing([quote(fun(_@Ctx) -> _@Expr end) || Expr <- exprs(Exprs)])]
  }).
