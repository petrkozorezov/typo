-module(test_mod).
-compile({parse_transform, t_pt}).
-export([number/1]).
-export([number_union/1]).
-export([id/1]).
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
number_union(i) -> {42};
number_union(a) -> '42'.

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

-spec
id(V) -> V.
id(V) -> V.

% -spec
% maybe(undefined) -> undefined;
% maybe(T        ) -> T.
% maybe(undefined) -> undefined;
% maybe(V        ) -> V.

% -spec
% maybe1(undefined | T) -> undefined | T.
% maybe1(undefined) -> undefined;
% maybe1(V        ) -> V.

% maybe__spec_check() ->
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



% integer(I) ->
%   fun match_integer/2.

% match_integer(Ctx, OtherType) ->
%   ok.

% tree(T) ->
%   union([{leaf, T}, tree(T)]).

% '_spec_number'(untype = {fun t:integer/1, i}) ->
%   integer;
% '_spec_number'(untype = {integer, a}) ->
%   atom.

% '_spec_number'(untype = {atom, i}) ->
%   untype(
%     A = 42,
%     type(_@A)
%   );
% '_spec_number'(a) ->
%   any().

% '_impl_number'(T) ->
%   case T of
%     i -> 'foo'();
%     a -> '42'
%   end.


% spec_number(i) -> integer();
% spec_number(a) -> atom().
% number(i) ->  42;
% number(a) -> '42'.
