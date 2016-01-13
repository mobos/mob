-define(setup(F), {setup, fun start/0, fun teardown/1, F}).
-define(pass, ?_assert(true)).
-define(fail, ?_assert(false)).
-define(one_any_arg(X), fun(_) -> X end).
-define(two_any_args(X), fun(_, _) -> X end).
-define(return(V), V).
-define(receiving(Data, Asserts), receive Data ->  Asserts;  _ ->  [?fail]  end).
