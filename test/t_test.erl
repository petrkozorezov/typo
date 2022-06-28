-module(t_test).
-compile({no_auto_import, [error/1, error/2]}).
-include("common.hrl").

%%
%% simple vars
%%
match_unknownVar_atom_test() ->
  ?assertEqual(
    ok(#{'A' => atom}),
    t:match({var, 'A'}, atom)
  ).
match_atom_unknownVar_test() ->
  ?assertEqual(
    error([{'>=', atom, {var, 'A'}}]),
    t:match(atom, {var, 'A'})
  ).
match_unknownVar_knownVar_test() ->
  ?assertEqual(
    ok(#{'B' => {var, 'A'}, 'A' => atom }),
    t:match(#{'A' => atom}, '>=', {var, 'B'}, {var, 'A'})
  ).
match_knownVar_unknownVar_test() ->
  Bindings = #{'A' => atom},
  ?assertEqual(
    error([
      {'>=', {var, 'A'}, {var, 'B'}},
      {'>=',  atom     , {var, 'B'}}
    ]),
    t:match(Bindings, '>=', {var, 'A'}, {var, 'B'})
  ).
match_knownVar_knownVar_ok_test() ->
  ?assertEqual(
    ok(#{'A' => atom}),
    t:match(#{'A' => atom}, '>=', {var, 'A'}, {var, 'A'})
  ).
match_knownVar_knownVar_error_test() ->
  Bindings = #{'A' => atom, 'B' => integer},
  ?assertEqual(
    error([
      {'>=', {var, 'A'}, {var, 'B'}},
      {'>=',  atom     , {var, 'B'}},
      {'>=',  atom     ,  integer  }
    ]),
    t:match(Bindings, '>=', {var, 'A'}, {var, 'B'})
  ).
match_unknownVar_unknownVar_test() ->
  ?assertEqual(
    ok(#{'A' => {var, 'B'}}),
    t:match({var, 'A'}, {var, 'B'})
  ).
match_same_unknownVar_unknownVar_test() ->
  ?assertEqual(
    ok(#{'A' => {var, 'A'}}),
    t:match({var, 'A'}, {var, 'A'})
  ).

%%
%% any
%%
match_atom_any_test() ->
  ?assertEqual(
    ok(),
    t:match(atom, any)
  ).
match_unknownVar_any_test() ->
  ?assertEqual(
    ok(#{'A' => any}),
    t:match({var, 'A'}, any)
  ).
match_listUnknownVar_any_test() ->
  ?assertEqual(
    ok(#{'A' => any}),
    t:match({list, {var, 'A'}}, any)
  ).

%%
%% none
%%
match_atom_none_test() ->
  ?assertEqual(
    ok(),
    t:match(atom, none)
  ).
match_unknownVar_none_test() ->
  ?assertEqual(
    ok(#{'A' => none}),
    t:match({var, 'A'}, none)
  ).
match_listUnknownVar_none_test() ->
  ?assertEqual(
    ok(#{'A' => none}),
    t:match({list, {var, 'A'}}, none)
  ).

%%
%% list
%%
match_listUnknownVar_listAtom_test() ->
  ?assertEqual(
    ok(#{'A' => atom}),
    t:match({list, {var, 'A'}}, {list, atom})
  ).
match_list_cons_test() ->
  ?assertEqual(
    ok(),
    t:match({list, atom}, {cons, ?a1, nil})
  ).
match_list_consCons_test() ->
  ?assertEqual(
    ok(),
    t:match({list, atom}, {cons, ?a1, {cons, ?a2, nil}})
  ).
match_listUnknownVar_consCons_test() ->
  ?assertEqual(
    ok(#{'A' => ?ua1a2}),
    t:match({list, {var, 'A'}}, {cons, ?a1, {cons, ?a2, nil}})
  ).


match_tupleVartuple_test() ->
  ?assertEqual(
    ok(#{'A' => ?a2}),
    t:match({tuple, [{var, 'A'}, ?a1]}, {tuple, [?a2, ?a1]})
  ).
match_tupleAny_tupleAtom_test() ->
  ?assertEqual(
    ok(),
    t:match(tuple, {tuple, [?a1]})
  ).

%%
%% unions
%%
match_union_atom_ok_test() ->
  ?assertEqual(
    ok(),
    t:match(?ua1a2, ?a1)
  ).
match_union_atom_error_test() ->
  ?assertEqual(
    error([
      {'>=', ?ua1a2, ?a3},
      {'>=', ?a1   , ?a3}
    ]),
    t:match(?ua1a2, ?a3)
  ).
match_unionVar_atom_ok_test() ->
  ?assertEqual(
    ok(#{'A' => atom}),
    t:match({union, [?a1, {var, 'A'}]}, atom)
  ).
match_atom_union_ok_test() ->
  ?assertEqual(
    ok(),
    t:match(atom, ?ua1a2)
  ).
match_atom_union_error_test() ->
  Union = ?ua1a2,
  ?assertEqual(
    error([
      {'>=', ?a1, Union},
      {'>=', ?a1, ?a2  }
    ]),
    t:match(?a1, Union)
  ).
match_union_union_ok_test() ->
  ?assertEqual(
    ok(),
    t:match(
      {union, [atom, integer]},
      ?ua1a2
    )
  ).
match_union_union_error_test() ->
  Union = {union, [integer, float]},
  ?assertEqual(
    error([
      {'>=', Union  , ?ua1a2},
      {'>=', Union  , ?a1   },
      {'>=', integer, ?a1   }
    ]),
    t:match(
      {union, [integer, float]},
      ?ua1a2
    )
  ).
match_unionVar_unionAtom_test() ->
  ?assertEqual(
    ok(#{'A' => ?ua1a2, 'B' => ?ua1a2}),
    t:match({union, [{var, 'A'}, {var, 'B'}]}, ?ua1a2)
  ).
match_unionVar_complex_test() ->
  ?assertEqual(
    ok(#{
      'A' => integer,
      'B' => atom,
      'C' => {var, 'D'}
    }),
    t:match(#{'A' => integer}, '>=',
      {union, [
        {tuple, [{var, 'A'}, {var, 'B'}, {var, 'C'}]},
        {tuple, [integer, atom, {var, 'D'}]},
        {tuple, [integer, atom, integer]},
        integer
      ]},
      {tuple, [integer, atom, {var, 'D'}]}
    )
  ).

match_functions_ok_test() ->
  ?assertEqual(
    ok(),
    t:match(
      {'fun', [atom, ?a2 ], atom},
      {'fun', [atom, atom], ?a3 }
    )
  ).
match_functions_var_test() ->
  ?assertEqual(
    ok(#{
      'A1' => atom,
      'A2' => ?a2,
      'A3' => ?a3
    }),
    t:match(
      {'fun', [{var, 'A1'}, {var, 'A2'}], {var, 'A3'}},
      {'fun', [ atom      ,  ?a2       ],  ?a3       }
    )
  ).

match_functions_error_1_test() ->
  FA = {'fun', [atom, ?a2 ], ?a3 },
  FB = {'fun', [atom, atom], atom},
  ?assertEqual(
    error([
      {'>=', FA  , FB },
      {'>=', ?a3, atom}
    ]),
    t:match(FA, FB)
  ).

match_functions_error_2_test() ->
  FA = {'fun', [atom, atom], atom},
  FB = {'fun', [atom, ?a2 ], ?a1 },
  ?assertEqual(
    error([
      {'>=', FA  , FB },
      {'<=', atom, ?a2}
    ]),
    t:match(FA, FB)
  ).

norm_simple_test() ->
  ?assertEqual(
    {tuple, [{atom, function_clause}, ?a1]},
    t:norm(#{0 => ?a1, 1 => {var, 0}}, {tuple, [{atom, function_clause}, {var, 1}]})
  ).

%%

ok() ->
  ok(#{}).
ok(Bindings) ->
  {ok, Bindings}.

error(Error) ->
  {error, Error}.


% case_empty_test() ->
%   ?assertEqual(
%     {none, ctx(#{}, [{error, {case_clause, ?a1}, []}])},
%     t_expr:'case'(ctx(), [
%       {?a2, fun(Ctx) -> {{atom, integer}, Ctx} end}
%     ], ?a1)
%   ).
% case_without_choise_test() ->
%   ?assertEqual(
%     {{atom, ok}, ctx()},
%     t_expr:'case'(ctx(), [
%       {?a1, fun(Ctx) -> {{atom, ok}, Ctx} end}
%     ], ?a1)
%   ).
% case_with_choise_test() ->
%   ?assertEqual(
%     {{atom, ok}, ctx()},
%     t_expr:'case'(ctx(), [
%       {?a1, fun(Ctx) -> {{atom, ok   }, Ctx} end},
%       {?a2, fun(Ctx) -> {{atom, error}, Ctx} end}
%     ], ?a1)
%   ).
% case_var_test() ->
%   ?assertEqual(
%     {?a1, ctx()},
%     t_expr:'case'(ctx(), [
%       {{var, 'A'}, fun(Ctx) -> {maps:get('A', Ctx#context.bindings), Ctx} end}
%     ], ?a1)
%   ).


%%
%% simple model test
%%
match_test() ->
  ?assert(
    proper:quickcheck(
      ?FORALL(B, t(),
        ?FORALL(A, t_sub(B), begin
          % ?debug("~p ~p", [A, B]),
          case t:match(B, A) of
            {ok, _} -> true;
            _       -> false
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
