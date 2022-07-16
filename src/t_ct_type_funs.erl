-module(t_ct_type_funs).

%% parse transform API
-export([core_transform/2]).
%% cerl module API looks ugly and hard to use
%% let rely on compiler internals and fix this code if anything changed
-include_lib("compiler/src/core_parse.hrl").

-define(A, anno = Anno).


core_transform(Module = #c_module{attrs = Attrs}, _Opts) ->
  % io:format("~p~n", [Module]),
  TypoFuns = lists:flatten([Funs || {#c_literal{val = typo_funs}, #c_literal{val = Funs}} <- Attrs]),
  % io:format("~p~n", [TypoFuns]),
  Module#c_module{
    defs = lists:map(
      fun(FunDeclr) ->
        {#c_var{name = Name}, #c_fun{}} = FunDeclr,
        case lists:member(Name, TypoFuns) of
          true  -> function(FunDeclr);
          false -> FunDeclr
        end
      end,
      Module#c_module.defs
    )
  }.


function({#c_var{} = Name, #c_fun{ ?A, vars = Args, body = Expr } = Fun}) ->
  {
    Name,
    Fun#c_fun{
      body =
        #c_call{
          anno   = Anno,
          module = #c_literal{?A, val = t},
          name   = #c_literal{?A, val = eval},
          args   = [args_bindings(Anno, Args), #c_literal{?A, val = expr(Expr)}]
        }
    }
  }.

args_bindings(Anno, Vars) ->
  #c_map{
    ?A,
    arg = #c_literal{?A, val = #{}},
    es  = lists:map(fun args_binding/1, Vars)
  }.

args_binding(#c_var{?A, name = Name} = Var) ->
  #c_map_pair{
    ?A,
    op  = #c_literal{?A, val = assoc},
    key = #c_literal{?A, val = Name},
    val = Var
  }.

-spec vars(list(cerl:cerl())) ->
  list(t:var_name()).
vars(Vars) ->
  lists:map(fun var/1, Vars).
-spec var(cerl:cerl()) ->
  t:var_name().
var(#c_var{name = Name}) ->
  Name.

-spec expr(cerl:cerl()) ->
  t:expr().
expr(#c_case{arg = Arg, clauses = Clauses}) ->
  {'case', value(Arg), [
    {values(Ps), expr(Body)}
    || #c_clause{pats = Ps, body = Body} <- Clauses
  ]};
expr(#c_let{arg = Arg, body = Body}) ->
  {'let', vars(Arg), expr(Body)};
expr(#c_primop{name = Name, args = Args}) ->
  {primop, value(Name), values(Args)};
expr(Value) ->
  {value, value(Value)}.

-spec value(cerl:cerl()) ->
  t:t().
value(#c_var{name = Name}) ->
  {var, Name};
value(#c_values{es = ES}) ->
  values(ES);
value(#c_tuple{es = ES}) ->
  list_to_tuple(values(ES));
value(#c_cons{ hd = H, tl = T }) ->
  [value(H)|value(T)];
% value(#c_map{name = Name}) ->
% value(#c_map_pair{name = Name}) ->
value(#c_literal{val = Val}) ->
  Val;
value(Node) ->
  io:format("unknown core value: ~p~n", [Node]),
  Node.

values(ES) ->
  lists:map(fun value/1, ES).


% % erl: {t_untype , Expr}
% % ex:  {:t_untype, Expr}
% type_expr(#c_tuple{es = [#c_literal{val = t_untype}, Expr]}) ->
%   ok.

% type_literal({t_untype, Literal}) ->
%   untype_literal(Literal);
% type_literal(Atom) -> when is_atom(Atom) ->
%   {atom, Atom};


% untype_expr(#c_tuple{es = [#c_literal{val = t_type}, Expr]}) ->
%   ok.

% untype_literal(Literal)
%   ok.

% % test_type_untype(untype = T) ->
% %   untype(
% %     case T of
% %       42 -> {atom, ok}
% %     end
% %   ).

% % test_type_untype(T) ->
% %   case T of
% %     42 -> ok
% %   end.
