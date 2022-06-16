%%%
%%% TODO
%%%  - разделить на 2 pt
%%%  - github
%%%  - фреймворк для тестовых кейсов с 'should fail' и 'should pass'
%%%  - добавить макрос 'as' в pt
%%%  - сделать pt для type (???)
%%%  - сделать мапы
%%%  - логические функции(?) на мапами ('&', '|')
%%%  - unknown type
%%%  - проверка полноты покрытия в case
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
-export([extends/3, as/2, 'case'/3, match/3, normalize/2]).

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

%% TODO подумать про
%%  - тип term как супер типа для литералов
%%  - term как unknown (плохая идея, будет людей сбивать с толку, unknown нужен явный)
-type t() ::
    any
  | none % 'never' in ts
  % | unknown ??
  | {list, t()}
  | {cons, t(), t()}
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
  | {var, atom()}
.

-type bindings() :: #{ atom() => t() }.


%
% TODO:
%  - errors
%  - map
%
% '<='
-type extends_result() :: {ok, bindings()} | {error, [{t(), t()}]}.
-spec extends(t(), t(), bindings()) ->
  extends_result().

extends({var, Var}, Type, Bindings) ->
  case maps:find(Var, Bindings) of
    {ok, VarValue} -> extends(VarValue, Type, Bindings);
    error          -> {ok, maps:put(Var, Type, Bindings)}
  end;
extends(Type, VarType = {var, Var}, Bindings) ->
  case maps:find(Var, Bindings) of
    {ok, VarValue} -> extends(Type, VarValue, Bindings);
    error          -> {error, [{Type, VarType}]}
  end;

extends(Type, Type, Bindings) ->
  {ok, Bindings};

extends(Type, any, Bindings) ->
  case Type of
    % TODO union, fun
    {list, Type_}      -> extends(Type_, any, Bindings);
    {cons, Head, Tail} -> extends_all([{Head, any}, {Tail, any}], Bindings);
    {tuple, Types}     -> extends_all([{Type_, any} || Type_ <- Types], Bindings);
    {union, Types}     -> extends_all([{Type_, any} || Type_ <- Types], Bindings);
    {'fun', Args, _}   -> extends({tuple, Args}, any, Bindings); % TODO {Ret, any} ??
    _                  -> {ok, Bindings} % primitive types
  end;
extends(none, _, Bindings) ->
  {ok, Bindings};

extends({list, TypeA}, {list, TypeB}, Bindings) ->
  extends(TypeA, TypeB, Bindings);
extends({cons, Head, Tail}, {list, Type}, Bindings) ->
  extends_all([{Head, Type}, {Tail, {list, Type}}], Bindings);
extends({cons, HeadA, TailA}, {cons, HeadB, TailB}, Bindings) ->
  extends_all([{HeadA, HeadB}, {TailA, TailB}], Bindings);
extends(nil, {list, _}, Bindings) ->
  {ok, Bindings};

extends({tuple, Types}, tuple, Bindings) ->
  extends({tuple, Types}, any, Bindings);
extends({tuple, TypesA}, {tuple, TypesB}, Bindings) when length(TypesA) =:= length(TypesB) ->
  extends_all(lists:zip(TypesA, TypesB), Bindings);

extends({union, UnionTypes}, Type, Bindings) ->
  extends_all([{UnionType, Type} || UnionType <- UnionTypes], Bindings);
extends(Type, {union, UnionTypes}, Bindings) ->
  % TODO simplify
  UnionBindingsList =
    lists:filtermap(
      fun(UnionType) ->
        case extends(Type, UnionType, Bindings) of
          {ok, SubBindings} -> {true, SubBindings};
          {error, _       } -> false
        end
      end,
      UnionTypes
    ),
  case UnionBindingsList of
    [] ->
      % TODO {union, []}
      % extends(Type, none, Bindings)
      {error, [{Type, {union, UnionTypes}}]};
    _ ->
      UnionBindings =
        lists:foldl(
          fun(BindingsA, BindingsAcc) ->
            maps:merge_with(
              fun(_, TypeA, TypeB) ->
                {union, Types} = union(TypeA, TypeB),
                fold_union(Types, [])
              end,
              BindingsA,
              BindingsAcc
            )
          end,
          Bindings,
          UnionBindingsList
        ),
      {ok, UnionBindings}
  end;
%%        foo(1 | 2    ) -> 1 | 2.
%% contr: foo(1        ) -> 1 | 2 | 3.
%% co:    foo(1 | 2 | 3) -> 1.
extends({'fun', ArgsA, RetA}, {'fun', ArgsB, RetB}, Bindings) ->
  extends_all([{{tuple, ArgsB}, {tuple, ArgsA}}, {RetA, RetB}], Bindings);

% mb use word 'MetaType' instead of 'F'
extends(F, Type, Bindings) when is_function(F, 0) ->
  extends(F(), Type, Bindings);
extends(Type, F, Bindings) when is_function(F, 0) ->
  extends(Type, F(), Bindings);

extends({atom, _}, atom, Bindings) ->
  {ok, Bindings};
extends({integer, _}, integer, Bindings) ->
  {ok, Bindings};
extends({float, _}, float, Bindings) ->
  {ok, Bindings};

extends(TypeA, TypeB, _) ->
  {error, [{TypeA, TypeB}]}.

-spec extends_all(list({t(), t()}), bindings()) ->
  extends_result().
extends_all([], Bindings) ->
  {ok, Bindings};
extends_all([{TypeA, TypeB}|Types], Bindings) ->
  case extends(TypeA, TypeB, Bindings) of
    Error={error, _}  -> Error;
    {ok, NewBindings} -> extends_all(Types, NewBindings)
  end.

%%
%% Sum two types
%%
% TODO mb fold?
-spec union(t(), t()) ->
  t().
union({union, TypesA}, {union, TypesB}) ->
  {union, TypesA ++ TypesB};
union({union, Types}, Type) ->
  {union, Types ++ [Type]};
union(Type, {union, Types}) ->
  {union, [Type|Types]};
union(TypeA, TypeB) ->
  {union, [TypeA, TypeB]}.

%%
%% converts to type
%%
as(_TypeA, _TypeB) ->
  erlang:error(pt_stub).

-spec 'case'(any(), list({t(), fun(() -> t())}), bindings()) ->
  t().
'case'(ValueType, Cases, Bindings) ->
  {union, [
    Fun(NewBindings)
    ||  {ClauseType, Fun} <- Cases,
        {ok, NewBindings} <- extends(ClauseType, ValueType, Bindings)
  ]}.

-spec match(t(), t(), bindings()) ->
  bindings().
match(PatternType, ValueType, Bindings) ->
  case extends(ValueType, PatternType, Bindings) of
    {error, _       } -> erlang:error(badmatch, [PatternType, ValueType, Bindings]);
    {ok, NewBindings} -> NewBindings
  end.

-spec normalize_all(list(t()), bindings()) ->
  list(t()).
normalize_all(Types, Bindings) ->
  lists:map(fun(Type) -> normalize(Type, Bindings) end, Types).

-spec normalize(t(), bindings()) ->
  t().
normalize({list, Type}, Bindings) ->
  {list, normalize(Type, Bindings)};
normalize({cons, Head, Tail}, Bindings) ->
  {cons, normalize(Head, Bindings), normalize(Tail, Bindings)};
normalize({tuple, Types}, Bindings) ->
  {tuple, normalize_all(Types, Bindings)};
normalize({union, Types}, Bindings) ->
  fold_union(normalize_all(Types, Bindings), []);
normalize({'fun', Args, Ret}, Bindings) ->
  {'fun', normalize_all(Args, Bindings), normalize(Ret, Bindings)};
normalize(Type = {var, 'V'}, Bindings) ->
  case maps:find('V', Bindings) of
    {ok, V} -> normalize(V, Bindings);
     error  -> Type
  end;
normalize(Type, _) ->
  Type.

-spec fold_union(list(t()), list(t())) ->
  none | any | {union, list(t())}.
fold_union([                   ], [] ) -> none;
fold_union([                   ], Acc) -> {union, lists:reverse(Acc)};
fold_union([ any          |_   ], _  ) -> any;
fold_union([ none         |Tail], Acc) -> fold_union(Tail, Acc);
fold_union([{union, Types}|Tail], Acc) -> fold_union(Types ++ Tail, Acc);
fold_union([ Head         |Tail], Acc) ->
  NewAcc =
    case lists:member(Head, Acc) of
      true  -> Acc;
      false -> [Head|Acc]
    end,
  fold_union(Tail, NewAcc).



% номализация блока
% цепочка матчей

% id__impl(__Arg1) ->
%   t:'case'({tuple, [__Arg1]}, [
%     {{tuple, [{var, 'V'}]}, fun(Bindings) ->
%       Bindings1 = t:match({tuple, [{var, 'K'}]}, {tuple, [{atom, atom}]}),
%       t:normalize({var, 'K'}, Bindings1)
%     end}
%   ], #{}).

% id__impl(__Arg1) ->
%   Bindings = t:match({tuple, [{var, 'V'}]}, {tuple, [__Arg1]}),
%   t:normalize({var, 'V'}, Bindings).


% traverse(Fun, Type, Bindings) ->


% %% 2 уровня pt
% %%  - генерация spec type_func для всех функций и их спеков
% %%  - преобразование всех type_func:
% %%   - проверка на чистоту
% %%   - замена match и case на extends с bindings
% -types([test_type/1]).

% -spec type__test_type(type()) ->
%   type().
% type__test_type(In) ->
%   A = fun t:integer/0,
%   B = {fun integer/1, 1},
%   case In of
%     atom -> {atom, test};
%     {tuple, [test]} -> {atom, test};
%   end.

% %% идеальный вид типовых функций
% %% вызовы других типовых функций конвертируются в тип t()
% %% (как понять вызываемая функия типовая или нет?)
%   C = {tuple, [integer]}
%   {tuple, [A]} = C,
% match => A = integer
%   % B = integer(1),
%   case In of
%     atom() -> atom(test);
%     tuple(test, C) -> tuple(A, C);
%   end.



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
