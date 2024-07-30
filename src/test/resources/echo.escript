#!/usr/bin/env escript
%%! -sname echo@localhost -setcookie test

main([]) ->
  register(echo, self()),
  io:format("ok~n"),
  loop().

loop() ->
  receive
    {_Pid, stop} -> exit(stopped);
    {Pid, Msg} -> Pid ! Msg
  end,
  loop().
