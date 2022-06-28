% -record(context, {
%   options     = #options{} :: t:options(),                 % pd?
%   bindings    = #{}        :: t:bindings(),                % в параметры
%   exceptions  = []         :: t:exceptions(),              % в тип
%   stacktrace  = []         :: list(term()), % TODO refine  % pd or plain stacktraces
%   messages    = []         :: list(term()), % TODO refine  % pd?
%   match_stack = []         :: t:match_stack()              % можно перенести параметром в match
% }).
