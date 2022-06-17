-record(options, {
  reverse_flag = false :: boolean()
}).
-record(context, {
  bindings     :: t:bindings(),
  errors       :: list(t:error())
}).
