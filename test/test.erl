-module(test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("typo/include/t.hrl").

test_mod_test() ->
  dbg:tracer(process, {fun dbg:dhandler/2, standard_error}),
  dbg:p(processes, c),
  % dbg:tpl({t_expr, function, '_'}, x),
  % dbg:tpl({t_expr, 'case', '_'}, x),
  % dbg:tpl({t_expr, 'case_', '_'}, x),
  % dbg:tpl({test_mod, number__impl, '_'}, x),
  dbg:tpl({test_mod, number_union__impl, '_'}, x),
  dbg:tpl({test_mod, number_union__spec, '_'}, x),
  % dbg:tpl({t, match, '_'}, x),
  % dbg:tpl({t_expr, block_, '_'}, x),
  % dbg:tpl({t_expr, block, '_'}, x),
  % dbg:tpl({t_expr, block, '_'}, x),
  R = test_mod:'__check_all_specs'(),
  io:format(standard_error, "~p~n", [R]).
