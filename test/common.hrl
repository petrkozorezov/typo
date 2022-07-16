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

-define(with_dbg(Debug, Expr),
  dbg:tracer(process, {fun dbg:dhandler/2, standard_error}),
  dbg:p(processes, c),
  Debug,
  try
    Expr
  after
    dbg:stop()
  end
).
% ?with_dbg(dbg:tpl({t, norm, '_'}, x), test())
