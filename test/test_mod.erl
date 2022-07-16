-module(test_mod).
-compile({parse_transform, t_pt}).
-compile({core_transform, t_ct_type_funs}).
-compile(export_all).

% -spec
% id(V) -> V.
% id(V) -> V.

-spec
any() -> any().
any() -> '42'.

-spec
int_ok() -> integer().
int_ok() -> 42.

-spec
int_err() -> integer().
int_err() -> 42.0.

-spec
float_ok() -> float().
float_ok() -> 42.0.

-spec
float_err() -> float().
float_err() -> 42.

-spec
union_1_ok(i) -> integer();
          (a) -> atom().
union_1_ok(i) -> 42;
union_1_ok(a) -> '42'.

-spec
union_1_err(i) -> integer();
           (a) -> atom().
union_1_err(i) -> [42];
union_1_err(a) -> '42'.

-spec
union_2_ok(i|a) -> atom() | integer().
union_2_ok(i) -> 42;
union_2_ok(a) -> '42'.

-spec
union_2_err(i|a) -> atom() | integer().
union_2_err(i) -> {42};
union_2_err(a) -> '42'.

-spec
list_1_ok() -> list().
list_1_ok() -> [42, 42].

-spec
list_1_err() -> list().
list_1_err() -> {42, 42}.

% spec__list_2_ok() -> [integer(), integer()].
%       list_2_ok() -> [42, 42].

% spec__list_2_err() -> [integer(), integer()].
%       list_2_err() -> [42].

-spec
nil_ok() -> [].
nil_ok() -> [].

-spec
nil_err() -> [].
nil_err() -> ok.

-spec
tuple_ok() -> {integer(), atom()}.
tuple_ok() -> {42, atom}.

-spec
tuple_err() -> {integer(), atom()}.
tuple_err() -> {42}.

% -spec
% local_call_ok() -> integer().
% local_call_ok() -> union_1_ok(i).

% -spec
% local_call_err() -> integer().
% local_call_err() -> union_1_ok(a).

% -spec
% maybe(undefined) -> undefined;
% maybe(T        ) -> T.
% maybe(undefined) -> undefined;
% maybe(V        ) -> V.

% -spec
% maybe1(undefined | T) -> undefined | T.
% maybe1(undefined) -> undefined;
% maybe1(V        ) -> V.

% -spec as(boolean()) ->
%   true.
% as(T) ->
%   t:as(T, true).

% -spec
% match(true) -> true.
% match(T   ) -> true = T.

% -type test(T) :: {leaf, T} | test(T).
% -type test(T) :: leaf | T.
