%%% File    : eunit_fsm.hrl
%%% Description : Define macro for eunit FSM test

%% Usage example
%% fsm_state_test_() ->
%%     {foreach,
%%      fun ()  -> {ok, P} = start(?SECRET), P end,
%%      fun (_) -> stop() end,
%%      [
%%       ?fsm_test(whereis(locker),"Started Properly Test",
%% 		[{state,is,unlocked},		 
%% 		 {loopdata,is,[?SECRET]}
%% 		]),
%%       ?fsm_state(whereis(locker), unlocked),
%%       ?fsm_data(whereis(locker), [?SECRET])
%%      ]  
%%     }.

-ifndef(EUNIT_FSM_HRL).
-define(EUNIT_FSM_HRL, true).

-define(_fsm_test(Id, Title, CmdList),
       {Title, fun() -> [eunit_fsm:translateCmd(Id, Cmd)
			 || Cmd <- CmdList]
	       end}).

-define(_with_fsm_test(Title, CmdList),
	fun(Pid) -> 
            [eunit_fsm:translateCmd(Pid, Cmd)
                          || Cmd <- CmdList]
        end
).

-define(fsm_state(Id, StateName),
	eunit_fsm:translateCmd(Id, {state,is,StateName})).

-define(fsm_data(Id, Data),
	eunit_fsm:translateCmd(Id, {loopdata,is,Data})).

-define(srv_data(Id, Data),
	eunit_fsm:translateCmd(Id, {srvdata,is,Data})).

-define(_fsm_state(Id, StateName),
	{??StateName, fun() -> ?fsm_state(Id, StateName) end}).

-define(_lift_fsm_state(StateName),
	fun(Pid) -> ?_fsm_state(Pid, StateName) end).

-define(_fsm_data(Id, Data), fun() -> ?fsm_data(Id, Data) end).
-define(_lift_fsm_data(Data),
	fun(Pid) -> ?_fsm_data(Pid, Data) end).

-define(_srv_data(Id, Data), fun() -> ?srv_data(Id, Data) end).
-define(_lift_srv_data(Data),
	fun(Pid) -> ?_srv_data(Pid, Data) end).

-define(exit_on_error(F, E), case F of E -> exit(error); Other -> Other end).

-endif. % EUNIT_FSM_HRL
