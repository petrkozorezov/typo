-module(test_mod).
-compile({parse_transform, tt}).
-export([number/1]).

% -export([test_type/0]).

% extends_test() ->
%   t:extends(fun ?MODULE:test_type/0, any).

% test_type() ->
%   foo.


% -spec as(boolean()) ->
%   true.
% as(T) ->
%   t:as(T, true).

% -spec
% match(true) -> true.
% match(T   ) -> true = T.

-spec
number(i) -> integer();
      (a) -> atom().
number(i) ->  42;
number(a) -> '42'.

-spec
number_union(i|a) -> atom() | integer().
number_union(i) ->  42;
number_union(a) -> '42'.

-spec
any() -> any().
any() -> '42'.

-spec
list() -> list().
list() -> [42|42].

-spec
nil() -> [].
nil() -> [].

% TODO
-spec
tuple() -> {integer(), atom()}.
tuple() -> {42, atom}.

% -spec
% id(V) -> V.
id(V) -> V.

% -spec
% maybe(undefined) -> undefined;
% maybe(T        ) -> T.
% maybe(undefined) -> undefined;
% maybe(V        ) -> V.

% maybe__spec__check() ->
%   true = t:extends(maybe__impl__({atom, undefined}), maybe__spec__({atom, undefined})),
%   true = t:extends(maybe__impl__( integer         ), maybe__spec__( integer         )).

% maybe__spec__(T) ->
%   t:case(T,
%     {atom, undefined} -> {atom, undefined},
%     T                 -> T
%   ).

% maybe__impl__(V) ->
%   t:case(V,
%     {atom, undefined} -> {atom, undefined},
%     V                 -> V
%   ).
