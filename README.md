Typo type checker
=================

 TODO
  * разделить на 2 pt
  * github
  * фреймворк для тестовых кейсов с 'should fail' и 'should pass'
  * добавить макрос 'as' в pt
  * сделать pt для type (???)
  * сделать мапы
  * логические функции(?) на мапами ('&', '|')
  * unknown type
  * add anno
  * проверка полноты покрытия в case
  * тип term как супер типа для литералов
  * term как unknown (плохая идея, будет людей сбивать с толку, unknown нужен явный)
  * заменить return на next (?) будет быстрее (а значимо ли?), но может стать пипец как сложно

 Вопросы:
  * открытые спеки (вроде id)? -- будет сравниваться открытый тип `{var, Var}`
  * гарды? -- при проходе через гард добавлять к типу '&'
  * для чека нужно компилировать otp с pp, как это организовать?
    просто собирать весь otp с типами? было бы удобно как в ts просто подключать библиотекой модули с типовыми функциями
    -- можно сделать хак маппингом модулей и виде библиотеки собирать модули с немного другими именами,
    либо на лету компилировать с типами текущий otp из debug_info(что размеется удобнее будет)
  * как запускать чек? -- сделать общую на модуль функцию `__check_specs` и её дёргать в каждом модуле

 Релизы:
  * preAlpha
   * что-то работает можно делиться с друзьями
  * Alpha
   * работает прилично, можно делиться с более широким кругом знакомых
   * нормальные ошибки
   * прочекать самого себя (и зависимости?)
  * Beta
   * работает без известных проблем, можно делиться сообществом
   * проверено на больших проектах
  * Release

 Продвижение:
  * Alpha:
   * сделать много примеров постепенно раскрывающих возможности
   * сделать доку
   * написать в чатик посмотреть на обратную связь
  * Beta:
   * попробовать прочекать известные проекты и при наличии ошибок завести issue со ссылкой на отчёт
   * написать в большой чатик


Идеи и наброски
---------------
 * тип это функтор
 * у тайпчекинга есть 2 фазы:
  * сначала с парстрансформами (pt) компиляция кода
  * запуск (run) скомпилированного кода для проверки типов
 * на стадии pt происходит компиляция типов и спеков в функции которые на вход получают типы и на выход дают результирующий тип

 * pt для:
  * тела функции
  * type/spec
 * extend на эрланг
 * extend на rust
 * кеширование?
 * параллелизм

 * t - обозначение типа
 * `match` - проверка на подтип с биндингом переменных
 * as - каст в другой тип
 * any, term,..., no_return встроенные типы
 * union - тип сумма

 * генерация spec type_func для всех функций и их спеков
 * преобразование всех type_func:
  * проверка на чистоту
  * замена match и case на match с bindings


Types programming
-----------------

Typescript:
```typescript
type Tree<T> = ['leaf', T] | Tree<T>
```

Plain erlang types:
```erlang
-type tree(T) ::
  {leaf, T} | tree(T).
```

Typo type:
```erlang
-types([tree/1]).
-spec tree(t:t()) ->
  t:t().
tree(T) ->
  t:union([{leaf, T}, tree(T)]).
```

Will transform to code for Typo checker:
```erlang
tree__type(Arg) ->
  t:'case'({tuple, [Arg]},
    [
      {tuple, [{var, 'T'}]},
      fun(Bindings) ->
        % resolving local variables and simplifying types (e.g. '{union, []}' 'means none')
        t:normalize(
          {union, [
            {tuple, [{atom, leaf}, {var, 'T'}]},
            {fun tree__type/1, {var, T}}
          ]},
          Bindings
        )
      end
    ]
  end.
```

Фазы
----

Есть 2 типа компиляции:
 * релизная, в ней вся типовая информация дропается
 * для тайпчекинга, в ней происходят следующие трансформации
  * все типы конвертируются в type functions (type to ttype) `foo__type`.
  * все спеки конвертируются в:
   * type functions (spec to ttype) `foo__spec`.
   * проверочные функции (spec to ttype) `foo__spec_check` вызывая которые, можно убедиться, что спек правильный.
  * все функции конвертируются в type functions (expr to ttype) `foo__impl` .

