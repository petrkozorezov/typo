-module(t_pt_specs_to_type_funs).
-include_lib("astranaut/include/quote.hrl").

-export([parse_transform/2]).

-record(state, {
  module       :: module(),
  attrs  = #{} :: t_pt_utils:attrs()
}).
-type state() :: #state{}.

-spec parse_transform([erl_parse:abstract_form()], compile:option()) ->
  [erl_parse:abstract_form()].
parse_transform([Form = {attribute, Pos, module, Module}|Tail], _Options) ->
  {Forms, State} =
    lists:mapfoldl(
      fun transform/2,
      #state{module = Module},
      Tail
    ),
  [Form | t_pt_utils:attrs_forms(Pos, State#state.attrs)] ++ lists:flatten(Forms);
parse_transform([Head|Tail], Options) ->
  [Head|parse_transform(Tail, Options)].

-spec transform(erl_parse:abstract_form(), state()) ->
  {[erl_parse:abstract_form()], state()}.
transform(Form = {attribute, Pos, type, {Name, Result, Args}}, State) ->
  TypeName = t_pt_utils:name(type, Name),
  TypeFunForm =
    {function, Pos, TypeName, erlang:length(Args), [
      {clause, Pos, t_pt_utils:etypes_to_ttypes(Args), [], [t_pt_utils:etype_to_ttype(Result)]}
    ]},
  {[Form, TypeFunForm], State};
transform(Form = {attribute, Pos, spec, {{Name, Arity}, Clauses}}, State = #state{attrs = Attrs0}) ->
  SpecName = t_pt_utils:name(spec, Name),
  Forms    = [spec_function(Pos, SpecName, Arity, Clauses), Form],
  % Attrs1    = t_pt_utils:add_attr(exports, [{SpecName, Arity}], Attrs0),
  Attrs1   = t_pt_utils:add_attr(typo_funs , [{SpecName, Arity}], Attrs0),
  NewState = State#state{ attrs = Attrs1 },
  {Forms, NewState};
transform(Form, State) ->
  {Form, State}.

spec_function(Pos, SpecName, Arity, Clauses) ->
  {function, Pos, SpecName, Arity,
    lists:map(fun spec_function_clause/1, Clauses)
  }.

%% TODO bounded_fun
% {type,Pos, bounded_fun, [
%   {type, Pos, 'fun', [{type, Pos, product, Args}, Result]},
%   [{type, Pos, constraint, [{atom, Atom, is_subtype}, [{var,Pos,'A'}, {atom,Pos,test}]]}]
%   [{type, Pos, constraint, [{atom, Atom, is_subtype}, [{var,Pos,'A'}, {atom,Pos,test}]]}]
% ]}
spec_function_clause({type, Pos, 'fun', [{type, _, product, Args}, Result]}) ->
  {clause, Pos, [{match, Pos, {atom, Pos, 't:untype'}, Arg} || Arg <- t_pt_utils:etypes_to_ttypes(Args)], [], [
    % {call, Pos, {remote, Pos, {atom, Pos, t}, {atom, Pos, untype}}, [t_pt_utils:etype_to_ttype(Result)]}
    {call, Pos, {remote, Pos, {atom, Pos, t}, {atom, Pos, untype}}, [t_pt_utils:etype_to_ttype(Result)]}
  ]}.
