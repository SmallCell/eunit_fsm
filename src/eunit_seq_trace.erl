%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%%
%%% @end
%%% Created : 15 Dec 2014 by vlad <lib.aca55a@gmail.com>

-module(eunit_seq_trace).
-export([tracer/0, tracer_spec/2,process_name/1]).

-include_lib("eunit/include/eunit.hrl").

-define(debug(Spec, Args),
        io:fwrite(user, <<"~w: ~s\n">>, [self(), io_lib:format(Spec, Args)])).


%% tracer code
tracer() ->
    {LB, {_,_,From,To,_} = TI,TS} = receive
                      {seq_trace,Label,TraceInfo} ->
                          {Label,TraceInfo,false};
                      {seq_trace,Label,TraceInfo,Ts} ->
                          {Label,TraceInfo,Ts}
                  end,
    FromTo =  lists:subtract([process_name(From), process_name(To)], 
                             [user, user_drv, error_logger, code_server, lager_event, file_server_2]),
    if length(FromTo) == 2 -> print_trace(LB, TI, TS); true -> ok end,
    tracer().

tracer_spec(Spec, Master) ->
    Errors = tracer_spec_loop(Spec, []),
    Master ! {results, Errors}.

tracer_spec_loop([], Errors) ->
    Errors;
tracer_spec_loop([Msg | Rest], Errors) ->
    {MaybeMsg, ErrorPlus} = receive
                              {seq_trace,Label,TraceInfo} ->
                                  print_trace(Label,TraceInfo,false),
                                  check_trace(Msg, TraceInfo);
                              {seq_trace,Label,TraceInfo,Ts} ->
                                  print_trace(Label,TraceInfo,Ts),
                                  check_trace(Msg, TraceInfo);
                              Other ->
                                    ?debugFmt(">> ignored tracer msg ~p ~n", [Other]),
                                    {[], [{ignored, Other}]}
               end,
    tracer_spec_loop(MaybeMsg ++ Rest, Errors ++ ErrorPlus).

                                   
print_trace(Label,TraceInfo,false) ->
    ?debug(" seq(~p)~s",[Label, format_trace(TraceInfo)]);
print_trace(Label,TraceInfo,Ts) ->
    ?debug(" seq(~p)TS(~s)~s",[Label,timestamp(Ts), format_trace(TraceInfo)]).

format_trace({print,Serial,From,_,Info}) ->
    io_lib:format("~p print from '~p' : ~p", [Serial,process_name(From),Info]);
format_trace({'receive',Serial,From,To,Message}) ->
    io_lib:format("~p received '~p' <- '~p' : ~p",
                  [Serial,process_name(To),process_name(From),Message]);
format_trace({send,Serial,From,To,Message}) ->
    io_lib:format("~p sent '~p' -> '~p' : ~p",
                 [Serial,process_name(From),process_name(To),Message]).

check_trace(OneOf, TraceInfo) when is_list(OneOf) ->
    Rest = lists:foldl(fun(Msg, Acc) -> case check_trace(Msg, TraceInfo) of
                                             true -> Acc; false -> [Msg | Acc]
                                         end end, [], OneOf),
    if Rest == [] -> []; true -> [Rest] end;
check_trace(Msg, TraceInfo) ->
    case compare_trace(Msg, TraceInfo) of
        false -> {[Msg], [{skip, TraceInfo}]};
        true -> {[], []}
    end.

compare_trace(Ex, {Type, _Serial, From, _To, Data}) when size(Ex) == 3 ->
    {print, ExFrom, ExMsg} = Ex,
    (print == Type) and (ExFrom == process_name(From)) and (ExMsg == Data);
compare_trace({Type, NameFrom, NameTo, ExpectedData},
              {Type, _Serial, From, To, Data}) ->
    ProcessFrom = process_name(From),
    ProcessTo = process_name(To),
    case {NameFrom, NameTo} of
       {ProcessFrom, ProcessTo} ->
            struct_compare(ExpectedData, Data);
        _ ->
            false
    end;
compare_trace(_A, _B) ->
    false.

%% private functions

struct_compare(LHS, RHS) when is_list(LHS), is_list(RHS) ->
    struct_compare_list(LHS, RHS);
struct_compare(LHS, RHS) when is_tuple(LHS), is_tuple(RHS) ->
    struct_compare_list(tuple_to_list(LHS), tuple_to_list(RHS));
struct_compare(LHS, RHS) when is_atom(LHS), is_atom(RHS) ->
    LHS == RHS;
struct_compare(_LHS, _RHS) ->
    true.

struct_compare_list([], []) -> true;
struct_compare_list(_LHS, []) -> false;
struct_compare_list([], _RHS) -> false;
struct_compare_list([L | LHS], [R | RHS]) ->
    struct_compare(L, R) and struct_compare_list(LHS, RHS).

timestamp(Now) -> 
    {_, _, Micros} = Now, 
    {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_local_time(Now), 
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~p", 
                  [YY, MM, DD, Hour, Min, Sec, Micros]).

process_name(Port) when is_port(Port)->
    case erlang:port_info(Port,registered_name) of
        {registered_name, Name} -> Name;
        _ -> erlang:port_to_list(Port)                        
    end;
process_name(Pid) when is_pid(Pid)->
    case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} -> Name;
        _ -> pid_to_list(Pid)
    end;
process_name(_V) ->
    noname.
