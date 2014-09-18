%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%%
%%% @end
%%% Created : 17 Sep 2014 by vlad <lib.aca55a@gmail.com>

-module(tradepost_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/eunit_fsm.hrl").

-import(tradepost, [which_tp/0, start_tp/0, stop_tp/0,
		    seller_identify_tp/1, seller_insertitem_tp/2, withdraw_item_tp/1]).


fsm_tradepost_test_() ->
    {foreach,
     fun ()  -> start_tp()end,
     fun (_) -> stop_tp() end,
     [
      % Initialy (after start) in pending state and no loop data
      ?_fsm_state(which_tp(), pending),
      ?_fsm_data(which_tp(), [undefined,undefined,undefined,undefined,undefined]),
      %% From Pending, identify seller, then state should be ready
      %% loopdata should now contain seller_password
      ?_fsm_test(which_tp(), "Identify seler Test",
		 [
		  {call, tradepost, seller_identify, [which_tp(), seller_password], ok},
		  {state, is, ready},
		  {loopdata, is, [undefined,undefined, seller_password, undefined,undefined]}
		 ]),
      ?_fsm_test(which_tp(), "Insert/Withdraw Test",
		 [
		  {call, tradepost, seller_identify, [which_tp(), seller_password], ok},
		  {state, is, ready},
		  {loopdata, is, [undefined,undefined, seller_password, undefined,undefined]},

		  {call, tradepost, seller_insertitem, [which_tp(), playstation, seller_password], ok},
		  {state, is, item_received},
		  {loopdata, is, [playstation, undefined, seller_password, undefined,undefined]},
		  {call, tradepost, withdraw_item, [which_tp(), seller_password], ok},
		  {state, is, ready},

		  {call, tradepost, seller_insertitem, [which_tp(), playstation, seller_password], ok},
		  {state, is, item_received}
		 ])
     ]
    }.
