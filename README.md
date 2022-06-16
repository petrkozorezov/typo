Typo type checker
=================

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

