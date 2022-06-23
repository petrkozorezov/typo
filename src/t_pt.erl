-module(t_pt).
-export([parse_transform/2]).
-include_lib("macro_pipe/include/macro_pipe.hrl").

-spec parse_transform([erl_parse:abstract_form()], compile:option()) ->
  [erl_parse:abstract_form()].
parse_transform(Forms, Options) ->
  io:format("hello~n"),
  ?pipe([Forms ||
    t_pt_add_impls         :parse_transform('_', Options),
    t_pt_specs_to_type_funs:parse_transform('_', Options),
    t_pt_type_funs         :parse_transform('_', Options),
    t_pt_add_specs_checks  :parse_transform('_', Options),
    t_pt_type_funs_core    :parse_transform('_', Options)
  ]).


% #{
%   type_funs := [{atom(), arity()}]
%   exports   := [{atom(), arity()}]
%   types     := #{{atom(), arity()} => type()}
%   specs     := #{{atom(), arity()} => spec()}
%   funs      := #{{atom(), arity()} => function()}
% }

% компиляция типов и спеков
%  * скомпилировать спеки (number_spec_)
%  * скомпилировать типы (number_spec_)
%  * если функция экспортированна, то экспортировать

% пройти по всем функциям модуля кроме type_funs
%  * сделать имплементацию (Fun_impl_)
%  * в имплементациях поменять все внешние вызовы на _impl_

% генерация спекчеков
%  * сгенерить спекчек по _spec_ функции
%  * сгенерить общий spec-check
%  * поэкспортировать всех
