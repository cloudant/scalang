#!/usr/bin/env escript
%%! -sname link_delivery@localhost -setcookie test

main([]) ->
  process_flag(trap_exit, true),
  _Pid = spawn_link(fun() ->
      process_flag(trap_exit, true),
      {mbox_break, scala_break@localhost} ! self(),
      receive
        {'EXIT', _From, Reason} -> {scala_break, scala_break@localhost} ! Reason;
        M -> exit(M)
      end
    end),
  receive
    {'EXIT', _, _} ->
      halt(),
      receive after infinity -> 0 end
  end.

