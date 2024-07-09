#!/usr/bin/env escript
%%! -sname echo@localhost -setcookie test

main([]) ->
  register(echo, self()),
  io:format("ok~n"),
  receive
    {Pid, Msg} -> Pid ! Msg
  end.
