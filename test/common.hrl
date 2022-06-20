-include_lib("typo/include/t.hrl").

-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

% -define(debug(Fmt, Args), ok).
-define(debug(Fmt, Args), ?debugFmt(Fmt, Args)).

-define(a1, {atom, a1}).
-define(a2, {atom, a2}).
-define(a3, {atom, a3}).
-define(ua1a2, {union, [?a1, ?a2]}).

ctx() ->
  ctx(#{}).
ctx(Bindings) ->
  ctx(Bindings, []).
ctx(Bindings, Exceptions) ->
  #context{
    bindings   = Bindings,
    exceptions = Exceptions
  }.
