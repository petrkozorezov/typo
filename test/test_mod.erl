-module(test_mod).
-compile({parse_transform, t_pt}).
-compile({core_transform, t_ct_type_funs}).
-export([number/1]).
-export([number_union/1]).
-export([number_union_broken/1]).
% -export([id/1]).
% -export([number/1, number_union/1, any/0, list/0, nil/0, tuple/0, id/1]).


% -spec as(boolean()) ->
%   true.
% as(T) ->
%   t:as(T, true).

% -spec
% match(true) -> true.
% match(T   ) -> true = T.

% -type test(T) :: {leaf, T} | test(T).
% -type test(T) :: leaf | T.

-spec
number(i) -> integer();
      (a) -> atom().
number(T) ->
  case T of
    i -> 42;
    a -> '42'
  end.

-spec
number_union(i|a) -> atom() | integer().
number_union(i) -> 42;
number_union(a) -> '42'.

-spec
number_union_broken(i|a) -> atom() | integer().
number_union_broken(i) -> {42};
number_union_broken(a) -> '42'.


% -spec
% any() -> any().
% any() -> '42'.

% -spec
% list() -> list().
% list() -> [42|42].

% -spec
% nil() -> [].
% nil() -> [].

% % TODO
% -spec
% tuple() -> {integer(), atom()}.
% tuple() -> {42, atom}.

% -spec
% id(V) -> V.
% id(V) -> V.

% -spec
% maybe(undefined) -> undefined;
% maybe(T        ) -> T.
% maybe(undefined) -> undefined;
% maybe(V        ) -> V.

% -spec
% maybe1(undefined | T) -> undefined | T.
% maybe1(undefined) -> undefined;
% maybe1(V        ) -> V.
