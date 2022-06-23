-module(t_pt_add_impls).
% -include_lib("astranaut/include/quote.hrl").

-export([parse_transform/2]).

-record(state, {
  attrs = #{} :: t_pt_utils:attrs()
}).
-type state() :: #state{}.

-spec parse_transform([erl_parse:abstract_form()], compile:option()) ->
  [erl_parse:abstract_form()].
parse_transform([Form = {attribute, Pos, module, _}|Tail], _Options) ->
  {Forms, State} = astranaut:smapfold(fun transform/2, #state{}, Tail, #{}),
  [Form | t_pt_utils:attrs_forms(Pos, State#state.attrs)] ++ lists:flatten(Forms);
parse_transform([Head|Tail], Options) ->
  [Head|parse_transform(Tail, Options)].

-spec transform(erl_parse:abstract_form(), state()) ->
  {[erl_parse:abstract_form()], state()}.
transform(Form = {function, Pos, Name, Arity, Clauses}, State = #state{attrs = Attrs}) ->
  ImplName = t_pt_utils:name(impl, Name),
  ImplForm = {function, Pos, ImplName, Arity, Clauses},
  NewState = State#state{ attrs = t_pt_utils:add_attr(typo_funs, [{ImplName, Arity}], Attrs) },
  {[Form, ImplForm], NewState};
transform(Form, State) ->
  {Form, State}.
