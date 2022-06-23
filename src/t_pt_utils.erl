-module(t_pt_utils).
-include_lib("astranaut/include/quote.hrl").

-export_type([attrs/0]).
-export([list_literal/1, name/2, attrs_forms/2, add_attr/3]).
-export([etypes_to_ttypes/1, etype_to_ttype/1]).

list_literal([   ]) -> quote([]);
list_literal([H|T]) -> quote([ _@H | unquote(list_literal(T)) ]).

% _spec_FunName(Ctx, Arg)
% _impl_FunName(Ctx, Arg)
% _check_spec_FunName(Ctx)
% _check_all_specs(Ctx)

% _spec_foo() ->
%   ok.
% _impl_FunName

-spec name(type | spec | impl | check_spec, atom()) ->
  atom().
name(Subj, Name) ->
  erlang:list_to_atom(erlang:atom_to_list(Name) ++ "__" ++ erlang:atom_to_list(Subj)).

%%

-type attrs() :: #{atom() => list(term())}.
-spec attrs_forms(erl_anno:anno(), attrs()) ->
  list(erl_parse:abstract_form()).
attrs_forms(Pos, Attrs) ->
  lists:map(
    fun({Attr, Values}) ->
      {attribute, Pos, Attr, Values}
    end,
    maps:to_list(Attrs)
  ).

add_attr(Attr, NewValues, Attrs) ->
  maps:update_with(Attr, fun(Values) -> Values ++ NewValues end, NewValues, Attrs).

%%

-spec etypes_to_ttypes(list(erl_parse:abstract_type())) ->
  list(t:type()).
etypes_to_ttypes(Types) ->
  lists:map(fun etype_to_ttype/1, Types).

% {user_type,{10,12},test,[{var,{10,17},'A'}]}]}
-spec etype_to_ttype(erl_parse:abstract_type()) ->
  t:type().
etype_to_ttype(quote = _V@Var) ->
  quote({var, _A@Var});
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
