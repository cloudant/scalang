#!/usr/bin/env escript
%%! -sname monitor@localhost -setcookie test

main([]) ->
    {mbox_monitor, scala_monitor@localhost} ! self(),
    loop().

loop() ->
    receive
        {monitor, Pid} ->
            Ref = monitor(process, Pid),
            respond(Ref),
            loop();
        {demonitor, Ref} ->
            demonitor(Ref),
            respond({demonitor, Ref}),
            loop();
        {'DOWN', _, _, _, Reason}  ->
            respond({down, Reason}),
            loop();
        {exit, Reason} ->
            exit({exit, Reason})
    after 2000 ->
            respond(timeout)
    end.

respond(Msg) ->
    {scala_monitor, scala_monitor@localhost} ! Msg.


