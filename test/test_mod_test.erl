-module(test_mod_test).
-include_lib("common.hrl").

spec_check_fun(Fun0) ->
  Fun1 = lists:reverse(string:prefix(lists:reverse(Fun0), lists:reverse("_test"))),
  erlang:list_to_atom(Fun1 ++ "__spec_check").

-define(test(Fun, R),
  Fun() ->
    SpecCheckFun = spec_check_fun(??Fun),
    ?assertMatch(R, test_mod:SpecCheckFun())
).
-define(test_w_dbg(Debug, Fun, R),
  Fun() ->
    SpecCheckFun = spec_check_fun(??Fun),
    ?with_dbg(
      Debug,
      ?assertMatch(R, test_mod:SpecCheckFun())
    )
).
-define(ok, []).
-define(err, [{'spec clause does not match impl', _, _}]).
% ?test(id_test         , ?ok ).
?test(any_test        , ?ok ).
?test(int_ok_test     , ?ok ).
?test(int_err_test    , ?err).
?test(float_ok_test   , ?ok ).
?test(float_err_test  , ?err).
?test(union_1_ok_test , ?ok ).
?test(union_1_err_test, ?err).
?test(union_2_ok_test , ?ok ).
?test(union_2_err_test, ?err).
?test(list_1_ok_test  , ?ok ).
?test(list_1_err_test , ?err).
% ?test(list_2_ok_test  , ?ok ).
% ?test(list_2_err_test , ?err).
% ?test(nil_ok_test     , ?ok ).
% ?test(nil_err_test    , ?err).
?test_w_dbg(dbg:tpl({t, match, '_'}, x), nil_ok_test, ?ok).
?test_w_dbg(dbg:tpl({t, match, '_'}, x), nil_err_test, ?err).
?test(tuple_ok_test   , ?ok ).
?test(tuple_err_test  , ?err).
% ?test_w_dbg(dbg:tpl({t, norm, '_'}, x), tuple_err_test, ?err).
% ?test(local_call_ok_test , ?ok ).
% ?test(local_call_err_test, ?err).
