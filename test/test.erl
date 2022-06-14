-module(test).
-include_lib("eunit/include/eunit.hrl").

number_test() ->
  dbg:tracer(process, {fun dbg:dhandler/2, standard_error}),
  dbg:p(processes, c),
  dbg:tpl({t, extends, '_'}, x),
  % test_mod:number__spec__check(),
  test_mod:number_union__spec__check().
  % test_mod:any__spec__check().

% -export([number_test/0]).
