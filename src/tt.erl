%%% types level programming

%%% ttypes attribute
%%% tquote/tunquote
%%% по флагу решать дропать ли типовую информацию

-module(tt).
-include_lib("astranaut/include/quote.hrl").

-export([parse_transform/2]).

-spec parse_transform([erl_parse:abstract_form()], compile:option()) ->
  [erl_parse:abstract_form()].
parse_transform(Forms, _Options) ->
  parse_transform(Forms).

parse_transform([ModuleForm = {attribute, Pos, module, _}|Tail]) ->
  {Forms, Exports} =
    lists:unzip(lists:map(fun transform/1, Tail)),
  [
    ModuleForm,
    {attribute, Pos, export, lists:flatten(Exports)}
  | lists:flatten(Forms)
  ];
parse_transform([Head|Tail]) ->
  [Head|parse_transform(Tail)].

transform(SpecForm = {attribute, Pos, spec, Spec = {{Name, Arity}, _}}) ->
  {[
    spec_check_function(Pos, Spec),
    spec_function(Pos, Spec),
    SpecForm
  ], [
    {spec_check_func_name(Name), 0    },
    {spec_func_name      (Name), Arity}
  ]};
transform(FunctionForm = {function, Pos, Name, Arity, Clauses}) ->
  {[
    FunctionForm,
    impl_function(Pos, Name, Arity, Clauses)
  ], [
    {impl_func_name(Name), Arity}
  ]};
transform(Form) ->
  {Form, []}.

spec_check_function(Pos, {{Name, _Arity}, Clauses}) ->
  {function, Pos, spec_check_func_name(Name), 0,
    [{clause, Pos, [], [],
      lists:map(
        fun(Clause) ->
          spec_check_function_clause(Name, Clause)
        end,
        Clauses
      )
    }]
  }.

spec_check_function_clause(Name, {type, Pos, 'fun', [{type, _, product, Args}, _Result]}) ->
  ArgsTypes = lists:map(fun etype_to_ttype/1, Args),
  SpecCheck = {call, Pos, {atom, Pos, spec_func_name(Name)}, ArgsTypes},
  SpecImpl  = {call, Pos, {atom, Pos, impl_func_name(Name)}, ArgsTypes},
  quote(
    true = t:extends(_@SpecImpl, _@SpecCheck)
  ).

spec_function(Pos, {{Name, Arity}, Clauses}) ->
  {function, Pos, spec_func_name(Name), Arity,
    lists:map(fun spec_function_clause/1, Clauses)
  }.

spec_function_clause({type, Pos, 'fun', [{type, _, product, Args}, Result]}) ->
  {clause, Pos, lists:map(fun etype_to_ttype/1, Args), [], [etype_to_ttype(Result)]}.

impl_function(Pos, Name, Arity, Clauses) ->
  Args = [{var, Pos, erlang:list_to_atom("__Arg" ++ erlang:integer_to_list(N))} || N <- lists:seq(1, Arity)],
  {function, Pos, impl_func_name(Name), Arity, [{clause, Pos, Args, [], [
    quote(
      t:'case'(
        {tuple, unquote(list_literal(Args))},
        unquote(impl_function_clauses(Clauses)),
        #{}
      )
    )
  ]}]}.

list_literal([   ]) -> quote([]);
list_literal([H|T]) -> quote([ _@H | unquote(list_literal(T)) ]).

impl_function_clauses([]) ->
  quote([]);
impl_function_clauses([{clause, _, Args, _Guards, Exprs}|Clauses]) ->
  ArgsTs   = list_literal(exprs_to_ttypes(Args)),
  FunExprs = exprs_to_ttypes(Exprs),
  ClausesT = impl_function_clauses(Clauses),
  quote([{{tuple, _@ArgsTs}, fun() -> _L@FunExprs end} | _@ClausesT]).

spec_check_func_name(Name) ->
  name(Name, "__spec__check").
spec_func_name(Name) ->
  name(Name, "__spec").
impl_func_name(Name) ->
  name(Name, "__impl").
name(Base, Postfix) ->
  erlang:list_to_atom(erlang:atom_to_list(Base) ++ Postfix).

-spec etypes_to_ttypes(list(erl_parse:abstract_type())) ->
  list(t:type()).
etypes_to_ttypes(Types) ->
  lists:map(fun etype_to_ttype/1, Types).

-spec etype_to_ttype(erl_parse:abstract_type()) ->
  t:type().
% spec__etype_to_ttype(Type) ->
%   t:extends({tuple, [{var, 'A'}]}, erl_parse:abstract_type(), fun(Args) -> t:type(maps:get('A', Args)) end).
etype_to_ttype(quote = _A@Atom) ->
  quote({atom, _A@Atom});
etype_to_ttype(quote = _I@Integer) ->
  quote({integer, _I@Integer});
etype_to_ttype(quote = _F@Float) ->
  quote({float, _F@Float});
etype_to_ttype(Type = {type, _, SubType, Args}) ->
  case {SubType, Args} of
    {any   ,  [ ]} -> quote(any);
    {none  ,  [ ]} -> quote(none);
    {list   , [ ]} -> quote({list, any});
    {list   , [V]} -> quote({list, unquote(etype_to_ttype(V))});
    {nil    , [] } -> quote([]);
    {tuple  , any} -> quote(tuple);
    {tuple  ,  Vs} -> quote({tuple, unquote(list_literal(etypes_to_ttypes(Vs)))});
    {union  ,  Vs} -> quote({union, unquote(list_literal(etypes_to_ttypes(Vs)))});
    {atom   , [ ]} -> quote(atom);
    {integer, [ ]} -> quote(integer);
    {float  , [ ]} -> quote(float);
    {_      , _  } -> erlang:error(badarg, [Type])
  end;
etype_to_ttype(Type) ->
  erlang:error(badarg, [Type]).


-spec exprs_to_ttypes(list(erl_parse:abstract_expr())) ->
  list(t:type()).
exprs_to_ttypes(Exprs) ->
  lists:map(fun expr_to_ttype/1, Exprs).

-spec expr_to_ttype(erl_parse:abstract_expr()) ->
  t:type().
expr_to_ttype(quote = _V@Var) ->
  quote({var, _A@Var});
expr_to_ttype(quote = _A@Atom) ->
  quote({atom, _A@Atom});
expr_to_ttype(quote = _I@Integer) ->
  quote({integer, _I@Integer});
expr_to_ttype(quote = _F@Float) ->
  quote({float, _F@Float});
expr_to_ttype(quote = []) ->
  quote([]);
expr_to_ttype(quote = [_@Head | _@Tail]) ->
  HeadT = expr_to_ttype(Head),
  TailT = expr_to_ttype(Tail),
  quote({cons, _@HeadT, _@TailT});
expr_to_ttype({tuple, _, Values}) ->
  ValuesT = list_literal(exprs_to_ttypes(Values)),
  quote({tuple, _@ValuesT}).


%% call
%% case
%% match
%% expr_to_ttype() -> {tuple, lists:map(fun expr_to_ttype/1, Values)}.
