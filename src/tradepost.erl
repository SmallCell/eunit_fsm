%%%-------------------------------------------------------------------
%%% @author Gianfranco <zenon@zen.home>
%%% @copyright (C) 2010, Gianfranco
%%% Created :  2 Sep 2010 by Gianfranco <zenon@zen.home>
%%%-------------------------------------------------------------------
-module(tradepost).
-behaviour(gen_fsm).

%% API
-export([start_link/0,
	 stop/1,seller_identify/2,seller_insertitem/3,withdraw_item/2]).


%% States
-export([pending/2, pending/3, ready/3, item_received/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-type password() :: atom().
-type date_time() :: tuple(Date::calendar:date(),
			   Time::calendar:time()).
-record(state, {object :: atom(),
		cash   :: pos_integer(),
		seller :: password(),
		buyer  :: password(),
		time   :: date_time()
	       }).


%% Test API
-define(VZTEST, true).
-ifdef(VZTEST).

-export([which_tp/0, start_tp/0, stop_tp/0,
	 seller_identify_tp/1, seller_insertitem_tp/2, withdraw_item_tp/1]).

which_tp() ->
    whereis(tradepost).

start_tp() ->
    %% io:format(user, "~n>> Start~n", []),
    {ok, Pid} = tradepost:start_link(),
    register(tradepost, Pid).

stop_tp() ->
    %% io:format(user, "~n>> Stop~n", []),
    Pid = which_tp(),
    true = unregister(tradepost),
    tradepost:stop(Pid).

seller_identify_tp(Password) ->
    seller_identify(which_tp(), Password).

seller_insertitem_tp(Item,Password) ->
    seller_insertitem(which_tp(), Item, Password).
	
withdraw_item_tp(Password) ->
    withdraw_item(which_tp(), Password).

-endif.

%%% API
start_link() -> gen_fsm:start_link(?MODULE, [], []).

stop(Pid) -> gen_fsm:sync_send_all_state_event(Pid,stop).

seller_identify(TradePost,Password) ->
    gen_fsm:sync_send_event(TradePost,{identify_seller,Password}).

seller_insertitem(TradePost,Item,Password) ->
    gen_fsm:sync_send_event(TradePost,{insert,Item,Password}).

withdraw_item(TradePost,Password) ->
    gen_fsm:sync_send_event(TradePost,{withdraw,Password}).

%%--------------------------------------------------------------------
pending(_Event,LoopData) -> {next_state,pending,LoopData}.

pending({identify_seller,Password},_Frm,LoopD = #state{seller=undefined}) ->
    {reply,ok,ready,LoopD#state{seller=Password}};
pending({insert,_,_},_Frm,LoopD) ->
    {reply,error,ready,LoopD};
pending({withdraw,_},_Frm,LoopD) ->
    {reply,error,ready,LoopD}.

ready({identify_seller,Password},_Frm,LoopD = #state{seller=Password}) ->
    {reply,ok,ready,LoopD};
ready({identify_seller,_},_,LoopD) ->
    {reply,error,ready,LoopD};
ready({insert,Item,Password},_Frm,LoopD = #state{seller=Password}) ->
    {reply,ok,item_received,LoopD#state{object=Item}};
ready({insert,_,_},_Frm,LoopD) ->
    {reply,error,ready,LoopD};
ready({withdraw,_},_Frm,LoopD) ->
    {reply,error,ready,LoopD}.

item_received({withdraw,Password},_Frm,LoopD = #state{seller=Password}) ->
    {reply,ok,ready,LoopD#state{object=undefined}};
item_received({withdraw,_},_Frm,LoopD) ->
    {reply,error,item_received,LoopD};
item_received({insert,_,_},_Frm,LoopD) ->
    {reply,error,item_received,LoopD}.

%%--------------------------------------------------------------------
handle_sync_event(stop,_From,_StateName,LoopData) ->
    {stop,normal,ok,LoopData};
handle_sync_event(_E,_From,StateName,LoopData) ->
    {reply,ok,StateName,LoopData}.

%%--------------------------------------------------------------------
init([]) -> {ok, pending, #state{}}.
handle_event(_Event, StateName, State) ->{next_state, StateName, State}.
handle_info(_Info, StateName, State) -> {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) -> ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.
