-module(t_pt_add_specs_checks).
-include_lib("astranaut/include/quote.hrl").

-export([parse_transform/2]).

% генерация спекчеков
%  * сгенерить спекчек по __spec функции
%  * сгенерить общий spec-check
%  * поэкспортировать всех

% -spec foo() -> ok.
% foo() -> ok.

% foo__spec_check проверка спеки (типовая?)
% foo__spec типовая версия спеки
% foo__impl типовая версия foo
% foo вызывает spec если есть

% bar() -> ok.
% bar__impl типовая версия bar
% bar вызывает impl


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
transform(Form = {function, Pos, SpecName, Arity, Clauses}, State) ->
  SpecNameStr = erlang:atom_to_list(SpecName),
  case string:prefix(lists:reverse(SpecNameStr), ?spec_rev) of
    nomatch ->
      {Form, State};
    RevBaseNameStr ->
      BaseNameStr   = lists:reverse(RevBaseNameStr),
      BaseName      = erlang:list_to_atom(BaseNameStr),
      SpecCheckName = erlang:list_to_atom(SpecNameStr ++ "_check"),
      SpecCheckSpecificName = erlang:list_to_atom(SpecNameStr ++ "_check_"),
      ImplName      = erlang:list_to_atom(BaseNameStr ++ "__impl"),
      SpecCheck0 =
        {function, Pos, SpecCheckName, 0, [
          {clause, Pos, [], [], [
            quote(
              lists:flatten([
                unquote_splicing([
                  quote( _A@SpecCheckSpecificName(_L@Args) ) || {clause, _, Args, _, _} <- Clauses
                ])
              ])
            )
          ]}
        ]},

      Args = [
        {var, Pos, erlang:list_to_atom("A" ++ erlang:integer_to_list(N))}
        || N <- lists:seq(1, Arity)
      ],
      SpecCheck1 =
        {function, Pos, SpecCheckSpecificName, Arity, [
          {clause, Pos, Args, [], [
            quote(
              case t:match(_A@SpecName(_L@Args), _A@ImplName(_L@Args)) of
                {ok, _}        -> [];
                {error, Error} -> [{'spec clause does not match impl', [_L@Args], Error}]
              end
            )
          ]}
        ]},
      Exports = [
        {SpecCheckName, 0},
        {SpecCheckSpecificName, Arity}
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
