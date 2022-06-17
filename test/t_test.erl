-module(t_test).
-include_lib("typo/include/t.hrl").

-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

% -define(debug(Fmt, Args), ok).
-define(debug(Fmt, Args), ?debugFmt(Fmt, Args)).

-compile({no_auto_import, [error/1, error/2]}).

%%
%% simple vars
%%
match_unknownVar_atom_test() ->
  ?assertEqual(
    ok(#{'A' => atom}),
    t:match(opts(), ctx(), {var, 'A'}, atom)
  ).
match_atom_unknownVar_test() ->
  ?assertEqual(
    error({'unknown var', 'A'}),
    t:match(opts(), ctx(), atom, {var, 'A'})
  ).
match_unknownVar_knownVar_test() ->
  ?assertEqual(
    ok(#{'B' => {var, 'A'}, 'A' => atom }),
    t:match(opts(), ctx(#{'A' => atom}), {var, 'B'}, {var, 'A'})
  ).
match_knownVar_unknownVar_test() ->
  Bindings = #{'A' => atom},
  ?assertEqual(
    error({'unknown var', 'B'}, Bindings),
    t:match(opts(), ctx(Bindings), {var, 'A'}, {var, 'B'})
  ).
match_knownVar_knownVar_ok_test() ->
  ?assertEqual(
    ok(#{'A' => atom}),
    t:match(opts(), ctx(#{'A' => atom}), {var, 'A'}, {var, 'A'})
  ).
match_knownVar_knownVar_error_test() ->
  Bindings = #{'A' => atom, 'B' => integer},
  ?assertEqual(
    error({badmatch, atom, integer}, Bindings),
    t:match(opts(), ctx(Bindings), {var, 'A'}, {var, 'B'})
  ).
match_unknownVar_unknownVar_test() ->
  ?assertEqual(
    ok(#{'A' => {var, 'B'}}),
    t:match(opts(), ctx(), {var, 'A'}, {var, 'B'})
  ).
match_same_unknownVar_unknownVar_test() ->
  ?assertEqual(
    ok(#{'A' => {var, 'A'}}),
    t:match(opts(), ctx(), {var, 'A'}, {var, 'A'})
  ).

%%
%% any
%%
match_atom_any_test() ->
  ?assertEqual(
    ok(),
    t:match(opts(), ctx(), atom, any)
  ).
match_unknownVar_any_test() ->
  ?assertEqual(
    ok(#{'A' => any}),
    t:match(opts(), ctx(), {var, 'A'}, any)
  ).
match_listUnknownVar_any_test() ->
  ?assertEqual(
    ok(#{'A' => any}),
    t:match(opts(), ctx(), {list, {var, 'A'}}, any)
  ).

%%
%% none
%%
match_atom_none_test() ->
  ?assertEqual(
    ok(),
    t:match(opts(), ctx(), atom, none)
  ).
match_unknownVar_none_test() ->
  ?assertEqual(
    ok(#{'A' => none}),
    t:match(opts(), ctx(), {var, 'A'}, none)
  ).
match_listUnknownVar_none_test() ->
  ?assertEqual(
    ok(#{'A' => none}),
    t:match(opts(), ctx(), {list, {var, 'A'}}, none)
  ).

%%
%% list
%%
match_listUnknownVar_listAtom_test() ->
  ?assertEqual(
    ok(#{'A' => atom}),
    t:match(opts(), ctx(), {list, {var, 'A'}}, {list, atom})
  ).
match_list_cons_test() ->
  ?assertEqual(
    ok(),
    t:match(opts(), ctx(), {list, atom}, {cons, {atom, a1}, nil})
  ).
match_list_consCons_test() ->
  ?assertEqual(
    ok(),
    t:match(opts(), ctx(), {list, atom}, {cons, {atom, a1}, {cons, {atom, a2}, nil}})
  ).
match_listUnknownVar_consCons_test() ->
  ?assertEqual(
    ok(#{'A' => {union, [{atom, a1}, {atom, a2}]}}),
    t:match(opts(), ctx(), {list, {var, 'A'}}, {cons, {atom, a1}, {cons, {atom, a2}, nil}})
  ).

match_tupleVartuple_test() ->
  ?assertEqual(
    ok(#{'A' => {atom, a2}}),
    t:match(opts(), ctx(), {tuple, [{var, 'A'}, {atom, a1}]}, {tuple, [{atom, a2}, {atom, a1}]})
  ).
match_tupleAny_tupleAtom_test() ->
  ?assertEqual(
    ok(),
    t:match(opts(), ctx(), tuple, {tuple, [{atom, a1}]})
  ).

%%
%% unions
%%
match_union_atom_ok_test() ->
  ?assertEqual(
    ok(),
    t:match(opts(), ctx(), {union, [{atom, a1}, {atom, a2}]}, {atom, a1})
  ).
match_union_atom_error_test() ->
  ?assertEqual(
    error({badmatch, {union, [{atom, a1}, {atom, a2}]}, {atom, a3}}),
    t:match(opts(), ctx(), {union, [{atom, a1}, {atom, a2}]}, {atom, a3})
  ).
match_unionVar_atom_ok_test() ->
  ?assertEqual(
    ok(#{'A' => atom}),
    t:match(opts(), ctx(), {union, [{atom, a1}, {var, 'A'}]}, atom)
  ).
match_atom_union_ok_test() ->
  ?assertEqual(
    ok(),
    t:match(opts(), ctx(), atom, {union, [{atom, a1}, {atom, a2}]})
  ).
match_atom_union_error_test() ->
  ?assertEqual(
    error({badmatch, {atom, a1}, {atom, a2}}),
    t:match(opts(), ctx(), {atom, a1}, {union, [{atom, a1}, {atom, a2}]})
  ).
match_union_union_ok_test() ->
  ?assertEqual(
    ok(),
    t:match(opts(), ctx(),
      {union, [atom, integer]},
      {union, [{atom, a1}, {atom, a2}]}
    )
  ).
match_union_union_error_test() ->
  ?assertEqual(
    error({badmatch, {union, [integer, float]}, {atom, a1}}),
    t:match(opts(), ctx(),
      {union, [integer, float]},
      {union, [{atom, a1}, {atom, a2}]}
    )
  ).
match_unionVar_unionAtom_test() ->
  ResultUnion = {union, [{atom, a1}, {atom, a2}]},
  ?assertEqual(
    ok(#{'A' => ResultUnion, 'B' => ResultUnion}),
    t:match(opts(), ctx(), {union, [{var, 'A'}, {var, 'B'}]}, {union, [{atom, a1}, {atom, a2}]})
  ).
match_unionVar_complex_test() ->
  ?assertEqual(
    ok(#{
      'A' => integer,
      'B' => atom,
      'C' => {var, 'D'}
    }),
    t:match(opts(), ctx(#{'A' => integer}),
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
    t:match(opts(), ctx(),
      {'fun', [{atom, a1}, {atom, a1}],  atom     },
      {'fun', [ atom     ,  atom     ], {atom, a1}}
    )
  ).
match_functions_var_test() ->
  ?assertEqual(
    ok(#{
      'A1' => {atom, a1},
      'A2' => {atom, a2},
      'R'  => {atom, r }
    }),
    t:match(opts(), ctx(),
      {'fun', [{var, 'A1'}, {var, 'A2'}], {var, 'R'}},
      {'fun', [{atom, a1 }, {atom, a2 }], {atom, r }}
    )
  ).
match_functions_error_test() ->
  ?assertEqual(
    error({badmatch, {atom, a1}, atom}),
    t:match(opts(), ctx(),
      {'fun', [ atom     ,  atom     ], {atom, a1}},
      {'fun', [{atom, a1}, {atom, a1}],  atom     }
    )
  ).

%%
%% case
%%
case_empty_test() ->
  ?assertEqual(
    {none, ctx()},
    t:'case'(opts(), ctx(), {atom, a1}, [
      {{atom, a2}, fun(Ctx) -> {{atom, integer}, Ctx} end}
    ])
  ).
case_without_choise_test() ->
  ?assertEqual(
    {{atom, ok}, ctx()},
    t:'case'(opts(), ctx(), {atom, a1}, [
      {{atom, a1}, fun(Ctx) -> {{atom, ok}, Ctx} end}
    ])
  ).
case_with_choise_test() ->
  ?assertEqual(
    {{atom, ok}, ctx()},
    t:'case'(opts(), ctx(), {atom, a1}, [
      {{atom, a1}, fun(Ctx) -> {{atom, ok   }, Ctx} end},
      {{atom, a2}, fun(Ctx) -> {{atom, error}, Ctx} end}
    ])
  ).
case_var_test() ->
  ?assertEqual(
    {{atom, a1}, ctx()},
    t:'case'(opts(), ctx(), {atom, a1}, [
      {{var, 'A'}, fun(Ctx) -> {maps:get('A', Ctx#context.bindings), Ctx} end}
    ])
  ).

%%

ok() ->
  ok(#{}).
ok(Bindings) ->
  {ok, #context{
    bindings = Bindings,
    errors   = []
  }}.

error(Error) ->
  error(Error, #{}).

error(Error, Bindings) ->
  {error, #context{
    bindings = Bindings,
    errors   = [Error]
  }}.

ctx() ->
  ctx(#{}).
ctx(Bindings) ->
  #context{
    bindings = Bindings,
    errors   = []
  }.

opts() ->
  #options{}.

% match_super(A, B)
%   match(A, B);
% match_sub(A, B)
%   match(B, A).

% match_super(Pat, Val)
% match_sub(Pat, Val)

%%
%% simple model test
%%
match_test() ->
  ?assert(
    proper:quickcheck(
      ?FORALL(B, t(),
        ?FORALL(A, t_sub(B), begin
          % ?debug("~p ~p", [A, B]),
          case t:match(opts(), ctx(), B, A) of
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
