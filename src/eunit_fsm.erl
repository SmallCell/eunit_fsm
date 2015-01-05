%%% File    : eunit_fsm.erl
%%% Description : Tester for FSM

-module(eunit_fsm).

-export([translateCmd/2, get_status/2]).

-define(Expr(E),??E).

get_status(Pid, Which) ->
    {status, Pid, _Mod, List} = sys:get_status(Pid),
    AllData = lists:flatten([ X || {data, X} <- lists:last(List)]),
    proplists:get_value(Which, AllData).

translateCmd(Id, {state, is, X}) ->
    case get_status(Id, "StateName") of
	X -> true;
	V -> erlang:error({state_match_failed,
			    [{module, ?MODULE},
			     {line, ?LINE},
			     {expected, X},
			     {value, V}]})
    end;
translateCmd(_Id, {call, M, F, A, X}) ->
    case apply(M, F, A) of
	X -> ok;
	V -> erlang:error({function_call_match_failed,
			    [{module, ?MODULE},
			     {line, ?LINE},
			     {expression, ?Expr(apply(M, F, A))},
			     {expected, X},
			     {value, V}]})
    end;
%% gen_gsm
translateCmd(Id, {loopdata, is, X}) ->
    case tl(tuple_to_list(get_status(Id, "StateData"))) of
	X -> true;
	V -> erlang:error({loopdata_match_failed,
			    [{module, ?MODULE},
			     {line, ?LINE},
			     {expected, X},
			     {value, V}]})
    end;
translateCmd(Id, {loopdata, match, Xs}) ->
    Expected = lists:zip(Xs, tl(tuple_to_list(get_status(Id, "StateData")))),    
    lists:all(fun ({X, V}) -> compare(?LINE, X, V) end, Expected);
%% gen_server
translateCmd(Id, {srvdata, is, X}) ->
    case tl(tuple_to_list(get_status(Id, "State"))) of
	X -> true;
	V -> erlang:error({srvdata_match_failed,
			    [{module, ?MODULE},
			     {line, ?LINE},
			     {expected, X},
			     {value, V}]})
    end;
translateCmd(Id, {srvdata, match, Xs}) ->    
    Expected = lists:zip(Xs, tl(tuple_to_list(get_status(Id, "State")))),    
    lists:all(fun ({X, V}) -> compare(?LINE, X, V) end, Expected).
    
compare(_L, any, _V) -> true;
compare(L, F, V) when is_function(F) -> 
    case F(V) of
        true -> true;
        _ -> erlang:error({match_failed,
                           [{module, ?MODULE},
                            {line, L},
                            {expected, F},
                            {value, V}]})
    end;
compare(L, X, V) ->
    if X == V -> true;
       true -> erlang:error({match_failed,
                             [{module, ?MODULE},
                              {line, L},
                              {expected, X},
                              {value, V}]})
    end.


