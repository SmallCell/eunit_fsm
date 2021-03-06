%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%% seq_trace: demo
%%% @end
%%% Created : 14 Dec 2014 by vlad <lib.aca55a@gmail.com>

-module(sedex).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_fsm/include/eunit_seq_trace.hrl").

-compile(export_all).


loop_initiator(Port) ->
    receive
        {Port, _Message} ->
            ?testTraceItit(17, ['receive', print]),
            ?testTracePrint(17,"**** Trace Started ****"),
            call_server ! {self(),the_message};
        {ack, _Ack} ->
            ok
    end,
    loop_initiator(Port).

loop_port_controller() ->
    receive
        {PortController,Message} ->
            Ack = {received, Message},
            ?testTracePrint(17,"We are here now"),
            PortController ! {ack,Ack}
    end,
    loop_port_controller().


dialog_trace_test_() ->
    {foreach,
     fun() ->
             erlang:display(setup)
     end,
     fun(_) ->
             erlang:display(cleanup),
             catch exit(whereis(initiator), ok),
             catch exit(whereis(call_server), ok)
     end,
     [
      ?_testTrace(
         begin
             register(initiator, spawn(?MODULE, loop_initiator, [port])),
             register(call_server, spawn(?MODULE, loop_port_controller, [])),
             P = whereis(initiator),            
             P ! {port,message},
             receive after 300 -> ok end
         end)
     %% ,

     %%  ?_testTraceSpec(
     %%     [
     %%      {print, initiator, "**** Trace Started ****"},
     %%      {'receive', initiator, call_server, {whereis(initiator), the_message}},
     %%      {print, call_server, "We are here now"},
     %%      %% {print, call_server, "We are here now"},
     %%      {'receive', call_server, initiator, {ack, {received,the_message}}}
     %%     ],
     %%     begin
     %%         register(initiator, spawn(?MODULE, loop_initiator, [port])),
     %%         register(call_server, spawn(?MODULE, loop_port_controller, [])),
     %%         P = whereis(initiator),            
     %%         P ! {port,message},
     %%         receive after 300 -> ok end
     %%     end)
     ]}.

