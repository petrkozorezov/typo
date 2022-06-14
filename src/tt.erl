-module(tt).
-include_lib("astranaut/include/quote.hrl").

-export([parse_transform/2]).

-spec parse_transform([erl_parse:abstract_form()], compile:option()) ->
  [erl_parse:abstract_form()].
parse_transform(Forms, _Options) ->
  io:format("~p~n", [Forms]),
  R = parse_transform(Forms),
  io:format("~p~n", [R]),
  R.

parse_transform([ModuleForm = {attribute, Anno, module, _}|Tail]) ->
  {Forms, Exports} =
    lists:unzip(lists:map(fun transform/1, Tail)),
  [
    ModuleForm,
    {attribute, Anno, export, lists:flatten(Exports)}
  | lists:flatten(Forms)
  ];
parse_transform([Head|Tail]) ->
  [Head|parse_transform(Tail)].

transform(SpecForm = {attribute, Anno, spec, Spec = {{Name, Arity}, _}}) ->
  {[
    spec_check_function(Anno, Spec),
    spec_function(Anno, Spec),
    SpecForm
  ], [
    {spec_check_func_name(Name), 0    },
    {spec_func_name      (Name), Arity}
  ]};
transform(FunctionForm = {function, Anno, Name, Arity, Clauses}) ->
  {[
    FunctionForm,
    impl_function(Anno, Name, Arity, Clauses)
  ], [
    {impl_func_name(Name), Arity}
  ]};
transform(Form) ->
  {Form, []}.

spec_check_function(Anno, {{Name, _Arity}, Clauses}) ->
  {function, Anno, spec_check_func_name(Name), 0,
    [{clause, Anno, [], [],
      lists:map(
        fun(Clause) ->
          spec_check_function_clause(Name, Clause)
        end,
        Clauses
      )
    }]
  }.

spec_check_function_clause(Name, {type, Anno, 'fun', [{type, _, product, Args}, _Result]}) ->
  ArgsTypes = lists:map(fun etype_to_ttype/1, Args),
  SpecCheck = {call, Anno, {atom, Anno, spec_func_name(Name)}, ArgsTypes},
  SpecImpl  = {call, Anno, {atom, Anno, impl_func_name(Name)}, ArgsTypes},
  quote(
    true = t:extends(_@SpecImpl, _@SpecCheck)
  ).

spec_function(Anno, {{Name, Arity}, Clauses}) ->
  {function, Anno, spec_func_name(Name), Arity,
    lists:map(fun spec_function_clause/1, Clauses)
  }.

spec_function_clause({type, Anno, 'fun', [{type, _, product, Args}, Result]}) ->
  {clause, Anno, lists:map(fun etype_to_ttype/1, Args), [], [etype_to_ttype(Result)]}.

impl_function(Anno, Name, Arity, Clauses) ->
  Args = [{var, Anno, erlang:list_to_atom("__Arg" ++ erlang:integer_to_list(N))} || N <- lists:seq(1, Arity)],
  {function, Anno, impl_func_name(Name), Arity, [
    {clause, Anno, Args, [], [
      {call, Anno, {remote, Anno, {atom, Anno, t}, {atom, Anno, 'case'}}, [
        {tuple, Anno, [{atom, Anno, tuple}, list_literal(Args)]},
        impl_function_clauses(Anno, Clauses)
      ]}
    ]}
  ]}.

list_literal([   ]) -> {nil, []};
list_literal([H|T]) -> {cons, [], H, list_literal(T)}.

impl_function_clauses(Anno, []) ->
  {nil, Anno};
impl_function_clauses(_, [{clause, Anno, Args, _Guards, Body}|Clauses]) ->
  {cons, Anno,
    {tuple, Anno, [
      {tuple, Anno, [{atom, Anno, tuple}, list_literal(etypes_to_ttypes(Args))]},
      {'fun', Anno, {clauses, [{clause, Anno, [], [], exprs_to_ttypes(Body)}]}}
    ]},
    impl_function_clauses(Anno, Clauses)
  }.

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
etype_to_ttype(V = {atom   , _, _}) -> quote({atom   , _@V});
etype_to_ttype(V = {integer, _, _}) -> quote({integer, _@V});
etype_to_ttype(V = {float  , _, _}) -> quote({float  , _@V});
etype_to_ttype(Type    = {type, Anno, SubType, Args}) ->
  case {SubType, Args} of
    {any   ,  [ ]} -> quote(any);
    {none  ,  [ ]} -> quote(none);
    {list   , [ ]} -> quote({list, any});
    {list   , [V]} -> quote({list, unquote(etype_to_ttype(V))});
    {tuple  , any} -> quote(tuple);
    {tuple  ,  V } -> quote({tuple, unquote(etypes_to_ttypes(V))});
    {union  ,  V } -> quote({list, unquote(list_literal(etypes_to_ttypes(V)))});
    {atom   , [ ]} -> quote(atom   );
    {integer, [ ]} -> quote(integer);
    {float  , [ ]} -> quote(float  );
    {_      , _  } -> erlang:error(badarg, [Type])
  end;
etype_to_ttype(Type) ->
  erlang:error(badarg, [Type]).


-spec exprs_to_ttypes(list(erl_parse:abstract_expr())) ->
  list(t:type()).
etypes_to_ttypes(Exprs) ->
  lists:map(fun expr_to_ttype/1, Exprs).

-spec expr_to_ttype(erl_parse:abstract_expr()) ->
  t:type().
expr_to_ttype(V = {atom   , _, _}) -> quote({atom   , _@V});
expr_to_ttype(V = {integer, _, _}) -> quote({integer, _@V});
expr_to_ttype(V = {float  , _, _}) -> quote({float  , _@V});
expr_to_ttype({nil, Anno}) -> quote(nil);
expr_to_ttype({cons, Anno, HeadA, TailA}) -> {cons, expr_to_ttype(HeadA), expr_to_ttype(TailA)};
expr_to_ttype({tuple, Anno, Values}) -> {tuple, lists:map(fun expr_to_ttype/1, HeadA)};
expr_to_ttype({tuple, Anno, Values}) -> {tuple, lists:map(fun expr_to_ttype/1, HeadA)}.
