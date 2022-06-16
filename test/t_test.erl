-module(t_test).
-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

% -define(debug(Fmt, Args), ok).
-define(debug(Fmt, Args), ?debugFmt(Fmt, Args)).

%%
%% simple vars
%%
extends_unknown_var_any_test() ->
  ?assertEqual(
    {ok, #{'A' => any}},
    t:extends({var, 'A'}, any, #{})
  ).
extends_unknown_var_primitive_test() ->
  ?assertEqual(
    {ok, #{'A' => atom}},
    t:extends({var, 'A'}, atom, #{})
  ).
extends_unknown_var_known_var_test() ->
  ?assertEqual(
    {ok, #{'B' => {var, 'A'}, 'A' => atom }},
    t:extends({var, 'B'}, {var, 'A'}, #{'A' => atom})
  ).
extends_known_var_known_var_ok_test() ->
  ?assertEqual(
    {ok, #{'A' => atom}},
    t:extends({var, 'A'}, {var, 'A'}, #{'A' => atom})
  ).
extends_known_var_unknown_var_test() ->
  ?assertEqual(
    {error, [{atom, {var, 'B'}}]},
    t:extends({var, 'A'}, {var, 'B'}, #{'A' => atom})
  ).
extends_known_var_known_var_error_test() ->
  ?assertEqual(
    {error, [{atom, integer}]},
    t:extends({var, 'A'}, {var, 'B'}, #{'A' => atom, 'B' => integer})
  ).
extends_unknown_var_unknown_var_test() ->
  ?assertEqual(
    {ok, #{'A' => {var, 'B'}}},
    t:extends({var, 'A'}, {var, 'B'}, #{})
  ).
extends_same_unknown_var_unknown_var_test() ->
  ?assertEqual(
    {ok, #{'A' => {var, 'A'}}},
    t:extends({var, 'A'}, {var, 'A'}, #{})
  ).

%%
%% unions
%%
extends_primitive_union_ok_test() ->
  ?assertEqual(
    {ok, #{}},
    t:extends(atom, {union, [atom, integer]}, #{})
  ).
extends_primitive_union_error_test() ->
  ?assertEqual(
    {error, [{atom, {union, [integer, float]}}]},
    t:extends(atom, {union, [integer, float]}, #{})
  ).
extends_union_primitive_ok_test() ->
  ?assertEqual(
    {ok, #{}},
    t:extends({union, [{atom, a1}, {atom, a2}]}, atom, #{})
  ).
extends_union_primitive_error_test() ->
  ?assertEqual(
    {error, [{{integer, 1}, atom}]},
    t:extends({union, [{integer, 1}, {atom, a2}]}, atom, #{})
  ).
extends_union_union_ok_test() ->
  ?assertEqual(
    {ok, #{}},
    t:extends(
      {union, [{atom, a1}, {atom, a2}]},
      {union, [atom]},
      #{}
    )
  ).
extends_union_union_error_test() ->
  ?assertEqual(
    {error, [{{atom, a1}, {union, [integer]}}]},
    t:extends(
      {union, [{atom, a1}, {atom, a2}]},
      {union, [integer]},
      #{}
    )
  ).

extends_UnionVarAtom_Atom_test() ->
  ?assertEqual(
    {ok, #{'A' => atom}},
    t:extends({union, [{var, 'A'}, {atom, a2}]}, atom, #{})
  ).
extends_UnionVar_UnionAtom_test() ->
  ?assertEqual(
    {ok, #{'A' => {union, [{atom, a1}, {atom, a2}]}}},
    t:extends({union, [{var, 'A'}]}, {union, [{atom, a1}, {atom, a2}]}, #{})
  ).
extends_union_var_complex_test() ->
  ?assertEqual(
    {ok, #{
      'A' => {union,[integer]}, % TODO
      'B' => {union, [integer, atom]},
      'D' => {union, [{var, 'C'}, integer]}
    }},
    t:extends(
      {tuple, [{var, 'A'}, {var, 'B'}, {var, 'D'}]},
      {union, [
        {tuple, [integer, atom, integer]},
        {tuple, [integer, integer, {var, 'C'}]},
        integer
      ]},
      #{'A' => integer}
    )
  ).

extends_list_unknown_var_list_primitive_test() ->
  ?assertEqual(
    {ok, #{'A' => integer}},
    t:extends({list, {var, 'A'}}, {list, integer}, #{})
  ).
extends_cons_list_test() ->
  ?assertEqual(
    {ok, #{}},
    t:extends({cons, {atom, a1}, nil}, {list, atom},#{})
  ).
extends_cons_cons_list_test() ->
  ?assertEqual(
    {ok, #{}},
    t:extends({cons, {atom, a1}, {cons, {atom, a2}, nil}}, {list, atom}, #{})
  ).

extends_binding_tuple_test() ->
  ?assertEqual(
    {ok, #{'A' => 2}},
    t:extends({tuple, [{var, 'A'}, 1]}, {tuple, [2, 1]}, #{})
  ).
extends_unknown_var_any_tuple_any_ok_test() ->
  ?assertEqual(
    {ok, #{'A' => any}},
    t:extends({tuple, [{var, 'A'}, 1]}, tuple, #{})
  ).

extends_test() ->
  ?assert(
    proper:quickcheck(
      ?FORALL(B, t(),
        ?FORALL(A, t_sub(B), begin
          % ?debug("~p ~p", [A, B]),
          case t:extends(A, B, #{}) of
            {ok   , _} -> true;
            {error, _} -> false
          end
        end)
      ),
      [{numtests, 1000}]
    )
  ).

t() ->
  wunion([
    {5 , any},
    {5 , none},
    {1 , {list, ?LAZY(t())}},
    {1 , {cons, ?LAZY(t()), ?LAZY(t())}},
    {1 , nil},
    {1 , tuple},
    {1 , {tuple, list(?LAZY(t()))}},
    {1 , {union, non_empty(list(?LAZY(t())))}},
    {10, atom},
    {10, {atom, atom()}},
    {10, integer},
    {10, {integer, integer()}},
    {10, float},
    {10, {float, float()}}
  ]).

t_sub(SuperT) ->
  SubTs =
    case SuperT of
      any          -> [t()];
      {list, T}    -> [{list, t_sub(T)}, nil] ++ t_sub_cons(T, {list, T});
      {cons, H, T} -> t_sub_cons(H, T);
      nil          -> [];
      tuple        -> t_sub_tuple(list(t()));
      {tuple, Ts}  -> t_sub_tuple(Ts);
      {union, Ts}  -> t_sub_union(Ts);
       atom        -> [{atom, atom()}];
      {atom, _}    -> [];
       integer     -> [{integer, integer()}];
      {integer, _} -> [];
       float       -> [{float, float()}];
      {float, _}   -> [];
      none         -> []
    end,
  union(SubTs ++ [
    SuperT,
    none
  ]).

t_sub_cons(H, T) ->
  [{cons, t_sub(H), ?LAZY(t_sub(T))}].

t_sub_tuple(TsGen) ->
  [{tuple, ?LET(Ts, TsGen, lists:map(fun t_sub/1, Ts))}].

t_sub_union(Ts) ->
  SubTs = union(lists:map(fun t_sub/1, Ts)),
  [{union, non_empty(list(SubTs))}, SubTs].

% t_super(SubT) ->
%   SuperTs =
%     case SubT of
%       any          -> [];
%       {list, T}    -> [{list, t_super(T)}];
%       {cons, H, T} -> [{cons, t_super(H), ?LAZY(t_super(T))}];
%       nil          -> [{list, t()}];
%       tuple        -> [];
%       {tuple, Ts}  -> [{tuple, ?LET(Ts_, Ts, lists:map(fun t_super/1, Ts_))}, tuple];
%       {union, Ts}  -> t_super_union(Ts);
%        atom        -> [];
%       {atom, _}    -> [atom];
%        integer     -> [];
%       {integer, _} -> [integer];
%        float       -> [];
%       {float, _}   -> [float];
%       none         -> [t()]
%     end,
%   union(SuperTs ++ [
%     SubT,
%     % {var, var()},
%     any
%   ]).

% t_super_union(Ts) ->
%   SuperTs = lists:map(fun t_super/1, Ts),
%   [?LET(Ts_, list(t()),
%     union([
%       {union, SuperTs       },
%       {union, SuperTs ++ Ts_}
%     ])
%   )].
