-module(t_pt_add_specs_checks).
-include_lib("astranaut/include/quote.hrl").

-export([parse_transform/2]).

% генерация спекчеков
%  * сгенерить спекчек по __spec функции
%  * сгенерить общий spec-check
%  * поэкспортировать всех

%% TODO __spec_check with arity =/= 1

-record(state, {
  attrs       = #{} :: list({atom(), arity()}),
  spec_checks = [] :: list(atom())
}).
-type state() :: #state{}.

-spec parse_transform([erl_parse:abstract_form()], compile:option()) ->
  [erl_parse:abstract_form()].
parse_transform([Form = {attribute, Pos, module, _}|TailForms], _Options) ->
  {NewForms, State} = lists:mapfoldl(fun transform/2, #state{}, TailForms),
  {CheckAllForms, NewState} = check_all_specs(Pos, State),
  [Form|t_pt_utils:attrs_forms(Pos, NewState#state.attrs)] ++ lists:flatten(NewForms ++ [CheckAllForms]);
% parse_transform([], _Options) ->
%   [];
parse_transform([Head|Tail], Options) ->
  [Head|parse_transform(Tail, Options)].

-define(spec_rev, "ceps__"). %% lists:reverse("__spec")
-spec transform(erl_parse:abstract_form(), state()) ->
  {[erl_parse:abstract_form()], state()}.
transform(Form = {function, Pos, SpecName, _, Clauses}, State) ->
  SpecNameStr = erlang:atom_to_list(SpecName),
  case string:prefix(lists:reverse(SpecNameStr), ?spec_rev) of
    nomatch ->
      {Form, State};
    RevBaseNameStr ->
      BaseNameStr   = lists:reverse(RevBaseNameStr),
      BaseName      = erlang:list_to_atom(BaseNameStr),
      SpecCheckName = erlang:list_to_atom(SpecNameStr ++ "_check"),
      ImplName      = erlang:list_to_atom(BaseNameStr ++ "__impl"),
      SpecCheck0 =
        {function, Pos, SpecCheckName, 0, [
          {clause, Pos, [], [], [
            quote(
              lists:flatten([
                unquote_splicing([
                  quote( _A@SpecCheckName(_L@Args) ) || {clause, _, Args, _, _} <- Clauses
                ])
              ])
            )
          ]}
        ]},

      VarT = {var, Pos, 'T'},
      SpecCheck1 =
        {function, Pos, SpecCheckName, 1, [
          {clause, Pos, [VarT], [], [
            quote(
              case t:match(_A@SpecName(_@VarT), _A@ImplName(_@VarT)) of
                {ok, _}        -> [];
                {error, Error} -> [{'spec clause does not match impl', _@VarT, Error}]
              end
            )
          ]}
        ]},
      Exports = [
        {SpecCheckName, 0},
        {SpecCheckName, 1}
      ],
      NewState =
        State#state{
          attrs       = t_pt_utils:add_attr(export, Exports, State#state.attrs),
          spec_checks = [{BaseName, SpecCheckName}|State#state.spec_checks]
        },
      {[Form, SpecCheck0, SpecCheck1], NewState}
  end;
transform(Form, State) ->
  {Form, State}.

check_all_specs(Pos, State) ->
  Name = '__check_all_specs',
  Arity = 0,
  Form =
    {function, Pos, Name, Arity, [
      {clause, Pos, [], [], [
        quote([
            unquote_splicing([
              quote({_A@BaseName, _A@SpecCheck()}) || {BaseName, SpecCheck} <- State#state.spec_checks
            ])
        ])
      ]}
    ]},
  NewState =
    State#state{
      attrs = t_pt_utils:add_attr(export, [{Name, Arity}], State#state.attrs)
    },
  {Form, NewState}.
