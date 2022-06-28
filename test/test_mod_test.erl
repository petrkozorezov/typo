-module(test_mod_test).
-include_lib("common.hrl").

number_test() ->
  ?assertEqual([], test_mod:number__spec_check()).

number_union_test() ->
  ?assertEqual([], test_mod:number_union__spec_check()).

number_union_broken_test() ->
  ?with_dbg(begin
  dbg:tpl({t, norm, '_'}, x),
  ?assertEqual([error], test_mod:number_union_broken__spec_check())
  end).
