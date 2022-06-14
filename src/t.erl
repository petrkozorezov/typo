%%%
%%% TODO
%%%  - git
%%%  - __spec переписать на t:'case'
%%%  - сделать пропер тест extends путём описания графа подтипов
%%%  - сделать биндинги
%%%  - фреймворк для тестовых кейсов с 'should fail' и 'should pass'
%%%  - добавить в pt обход impl (обойти body)
%%%  - добавить макрос 'as' в pt
%%%  - сделать pt для type
%%%  - сделать мапы
%%%  - логические функции(?) на мапами ('&', '|')
%%%  - unknown type
%%%  - проверка полного покрытия в case
%%%  -
%%%
%%% Вопросы:
%%%  - открытые спеки (вроде id)
%%%  - гарды
%%%  - для чека нужно компилировать otp с pp, как это организовать?
%%%    просто собирать весь otp с типами? было бы удобно как в ts просто подключать библиотекой модули с типовыми функциями
%%%  -
%%%
%%% Релизы:
%%%  - preAlpha
%%%   - что-то работает можно делиться с друзьями
%%%  - Alpha
%%%   - работает прилично, можно делиться с более широким кругом знакомых
%%%   - нормальные ошибки
%%%   - прочекать самого себя (и зависимости?)
%%%  - Beta
%%%   - работает без известных проблем, можно делиться сообществом
%%%   - проверено на больших проектах
%%%  - Release
%%%
%%% Продвижение:
%%%  - Alpha:
%%%   - сделать много примеров постепенно раскрывающих возможности
%%%   - сделать доку
%%%   - написать в чатик посмотреть на обратную связь
%%%  - Beta:
%%%   - попробовать прочекать известные проекты и при наличии ошибок завести issue со ссылкой на отчёт
%%%   - написать в большой чатик
%%%

% #typo #erlang #typechecker
% - тип это функтор
% - у тайпчекинга есть 2 фазы:
%  - сначала с парстрансформами (pt) компиляция кода
%  - запуск (run) скомпилированного кода для проверки типов
% - на стадии pt происходит компиляция типов и спеков в функции которые на вход получают типы и на выход дают результирующий тип

% - pt для:
%  - тела функции
%  - type/spec
% - extend на эрланг
% - extend на rust
% - кеширование?
% - параллелизм

% t - обозначение типа
% extends - проверка на подтип
% as - каст в другой тип
% any, term,..., no_return встроенные типы
% union - тип сумма

-module(t).
-export([extends/2, as/2, 'case'/2, match/2]).

% Type :: any()                 %% The top type, the set of all Erlang terms
%       | none()                %% The bottom type, contains no terms
%       | Atom
%       | Bitstring
%       | flaot()
%       | Fun
%       | Integer
%       | List
%       | Map
%       | Tuple
%       | Union
%       | UserDefined           %% described in Type Declarations of User-Defined Types

% {1, 2    } :: {1, 2} <= {1, any()}
% {1, any()} :: {1, any()}

% [1, 2, any()] :: [any()]

%% TODO подумать про нужность типа term как супер типа для литералов
-type t() ::
    any
  | none
  % | unknown ??
  | {list, t()}
  | {cons, t(), t()}  % TODO t() ??
  |  nil
  |  tuple
  | {tuple, [t()]}
  | {union, [t()]}
  % | {map, #{t() => {t(), Requireness :: boolean()}}
  | {'fun', [t()], t()}
  |  atom
  | {atom, atom()}
  |  integer
  % | {integer, Cond::#{ from => integer(), to => integer() }} % TODO
  | {integer, integer()}
  |  float
  | {float, float()}
  % | string
  % | pid
  % | reference
  % | binary
.

%
% TODO:
%  - bindings
%  - errors
%
-spec extends(t(), t()) ->
  boolean().
extends(_, any) ->
  true;
extends(_, none) ->
  false;
extends(Type, Type) ->
  true;

% '<='
extends({list, TypeA}, {list, TypeB}) ->
  extends(TypeA, TypeB);
extends({list, Type}, {cons, Head, Tail}) ->
  extends(Type, Head) andalso extends(Type, Tail);
extends({cons, Head, Tail}, {list, Type}) ->
  extends(Head, Type) andalso extends(Tail, Type);
extends({cons, HeadA, TailA}, {cons, HeadB, TailB}) ->
  extends(HeadA, HeadB) andalso extends(TailA, TailB);
extends(nil, {list, _}) ->
  true;

extends({tuple, _}, tuple) ->
  true;
extends({tuple, TypesA}, {tuple, TypesB}) when length(TypesA) =:= length(TypesB) ->
  lists:all(
    fun({TypeA, TypeB}) -> extends(TypeA, TypeB) end,
    lists:zip(TypesA, TypesB)
  );
extends({union, UnionTypes}, Type) ->
  lists:all(
    fun(UnionType) -> extends(UnionType, Type) end,
    UnionTypes
  );
extends(Type, {union, UnionTypes}) ->
  lists:any(
    fun(UnionType) -> extends(Type, UnionType) end,
    UnionTypes
  );
% extends({map, MapA}, {map, MapB}) ->
%   map_extends(MapA, MapB);

%%        foo(1 | 2    ) -> 1 | 2.
%% contr: foo(1        ) -> 1 | 2 | 3.
%% co:    foo(1 | 2 | 3) -> 1.
extends({'fun', ArgsA, RetA}, {'fun', ArgsB, RetB}) ->
  extends({tuple, ArgsB}, {tuple, ArgsA}) andalso extends(RetA, RetB);

% TODO pid, reference, binary,

% mb MetaType instead of F
extends(F, Type) when is_function(F, 0) ->
  extends(F(), Type);
extends(Type, F) when is_function(F, 0) ->
  extends(Type, F());

extends({atom, _}, atom) ->
  true;
extends({integer, _}, integer) ->
  true;
extends({float, _}, float) ->
  true;

% extends({var, Name}, Type) ->
%   #{Name => Type};

extends(_, _) ->
  false.

%%
%% converts to type
%%
as(_TypeA, _TypeB) ->
  erlang:error(pt_stub).

-spec 'case'(any(), list({t(), fun(() -> R)})) ->
  R. % when extends(R, t()).
'case'(ValueType, Cases) ->
  {union, [
    Fun()
    || {ClauseType, Fun} <- Cases, extends(ClauseType, ValueType)
  ]}.

-spec match(t(), t()) ->
  t().
match(PatternType, ValueType) ->
  case extends(ValueType, PatternType) of
    true  -> ValueType;
    false -> erlang:error({badmatch, ValueType})
  end.


% % required < optional
% % false < true
% #{
%   any     := integer,
%   integer := any,
% }
% #{
%   any     => integer,
%   integer => any,
% }
% map_extends([{TypeA, OptA}|TailA], [{TypeB, OptB}|TailB]) ->
%   extends(TypeA, TypeB) andalso OptA =< OptB andalso
% map_extends(A, B) ->
