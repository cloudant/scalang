#!/usr/bin/env escript
%%! -sname receive_connection@localhost -setcookie test

main([]) ->
  ok = net_kernel:monitor_nodes(true, [{node_type,all}]),
  io:format("ready~n"),
  receive
    {nodeup, Node, _} -> io:format("~p~n", [Node])
  end,
  receive
    ok -> ok
  end.
