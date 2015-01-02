%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%%
%%% @end
%%% Created : 15 Dec 2014 by vlad <lib.aca55a@gmail.com>

-ifndef(EUNIT_SEQ_TRACE_HRL).
-define(EUNIT_SEQ_TRACE_HRL, true).

-define(_testTraceSpec(Trace, Expr),
        {?LINE, fun () ->
                        %% SPAWN TRACES
                        Pid = spawn(eunit_seq_trace,tracer_spec,[Trace, self()]),
                        seq_trace:set_system_tracer(Pid), % set Pid as the system tracer
                        %% SEED BLOCK
                        (Expr),
                        %% RESULTS
                        receive
                            {results, Errors} ->
                                ?debugFmt("Errors: ~p~n", [Errors]),
                                ?assertEqual(0, length(Errors))
                        end
                end
        }).

-define(_testTrace(Expr),
        {?LINE, fun () ->
                        %% SPAWN TRACES
                        Pid = spawn(eunit_seq_trace,tracer,[]),
                        seq_trace:set_system_tracer(Pid), % set Pid as the system tracer
                        %% SEED BLOCK
                        (Expr)
                end
        }).

-ifdef(NOTRACE).
-define(testTraceItit(Label, Tokens), ok).
-define(testTracePrint(Label, Msg), ok).
-else.
-define(testTraceItit(Label, Tokens),
        begin
            ((fun () ->
                      seq_trace:set_token(label, Label),
                      lists:map(fun (T) -> seq_trace:set_token(T, true) end,Tokens)
              end)()),
            ok
        end).
-define(testTracePrint(Label, Msg), seq_trace:print(Label,Msg)).

-endif.

-endif. % EUNIT_SEQ_TRACE_HRL

