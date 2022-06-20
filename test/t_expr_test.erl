-module(t_expr_test).
-include("common.hrl").

%%
%% case
%%
case_empty_test() ->
  ?assertEqual(
    {none, ctx(#{}, [{error, {case_clause, ?a1}, []}])},
    t_expr:'case'(ctx(), [
      {?a2, fun(Ctx) -> {{atom, integer}, Ctx} end}
    ], ?a1)
  ).
case_without_choise_test() ->
  ?assertEqual(
    {{atom, ok}, ctx()},
    t_expr:'case'(ctx(), [
      {?a1, fun(Ctx) -> {{atom, ok}, Ctx} end}
    ], ?a1)
  ).
case_with_choise_test() ->
  ?assertEqual(
    {{atom, ok}, ctx()},
    t_expr:'case'(ctx(), [
      {?a1, fun(Ctx) -> {{atom, ok   }, Ctx} end},
      {?a2, fun(Ctx) -> {{atom, error}, Ctx} end}
    ], ?a1)
  ).
case_var_test() ->
  ?assertEqual(
    {?a1, ctx()},
    t_expr:'case'(ctx(), [
      {{var, 'A'}, fun(Ctx) -> {maps:get('A', Ctx#context.bindings), Ctx} end}
    ], ?a1)
  ).
