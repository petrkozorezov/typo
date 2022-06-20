-record(options, {
  reverse_flag = false :: boolean()
  % term = any % by default term = t:unknown
}).
-record(context, {
  options     = #options{} :: t:options(),
  bindings    = #{}        :: t:bindings(),
  exceptions  = []         :: t:exceptions(),
  stacktrace  = []         :: list(term()), % TODO refine
  messages    = []         :: list(term()), % TODO refine
  match_stack = []         :: t:match_stack()
}).
