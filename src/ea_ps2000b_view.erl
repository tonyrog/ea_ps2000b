%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%      EA PS 2000B view/control
%%% @end
%%% Created : 18 Aug 2015 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(ea_ps2000b_view).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0]).
-export([set_current/1, set_voltage/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(debug(F,A), io:format((F),(A))).

-record(state,
	{
	  uart,    %% uart device to ea_ps2000b
	  info,    %% device info
	  nv,      %% nominal voltage
	  na,      %% nominal current
	  timer    %% read actual value timer
	}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(hex_epx),
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

set_voltage(V) when is_number(V) ->
    gen_server:call(?SERVER, {set_voltage, float(V)}).

set_current(A) when is_number(A) ->
    gen_server:call(?SERVER, {set_current, float(A)}).
    
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    application:start(hex_epx),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    X = 15,
    Y = 15,
    W = 130,
    H = 60,
    hex_epx:init_event(out,
		       [{id,"voltage"},{type,value},
			{value, 0.0}, {format, "~.2f"},
			{font,[{name,"Arial"},{size,40}]},
			{halign,center},{valign,center},
			{x,X},{y,Y},{width,W},{height,H}]),
    hex_epx:init_event(out,
		       [{id,"voltage.border"},{type,rectangle},
			{color,black},{x,X},{y,Y},{width,W},{height,H}]),
    hex_epx:init_event(out,
		       [{id,"voltage.text"},{type,text},
			{font,[{name,"Arial"},{slant,roman},{size,10}]},
			{text,"voltage"},
			{halign,center},
			{x,X},{y,Y+H+1},
			{width,W},{height,10}]),
    hex_epx:init_event(out,
		       [{id,"current"},{type,value},
			{value, 0.0}, {format, "~.2f"},
			{font,[{name,"Arial"},{size,40}]},
			{halign,center},{valign,center},
			{x,X+W+X+X},{y,Y},{width,W},{height,H}]),
    hex_epx:init_event(out,
		       [{id,"current.border"},{type,rectangle},{color,black},
			{x,X+W+X+X},{y,Y},{width,W},{height,H}]),
    hex_epx:init_event(out,
		       [{id,"current.text"},{type,text},
			{font,[{name,"Arial"},{slant,roman},{size,10}]},
			{text,"current"},
			{halign,center},
			{x,X+W+X+X},{y,Y+H+1},
			{width,W},{height,10}]),

    hex_epx:init_event(in,
		       [{id,"remote"},{type,switch},{halign,center},
			{x,10},{y,210},{width,80},{height,20},
			{font,[{name,"Arial"},{size,12}]},
			{fill,solid}, {color,orange},{text, "CONTROL"}]),
    hex_epx:init_event(in,
		       [{id,"output"},{type,switch},{halign,center},
			{x,230},{y,210},{width,80},{height,20},
			{font,[{name,"Arial"},{size,12}]},
			{fill,solid},{color,yellow},{text, "OUTPUT"}]),

    hex_epx:add_event([{id,"remote"}], remote_switch, 
		      fun(Signal,Env) ->
			      gen_server:cast(?MODULE, {Signal,Env})
		      end),
    hex_epx:add_event([{id,"output"}], output_switch, 
		      fun(Signal,Env) ->
			      gen_server:cast(?MODULE, {Signal,Env})
		      end),
    {ok,U} = telegram:open("/dev/ttyACM0"),
    {ok,NV} = telegram:get_nominal_voltage(U),
    {ok,NA} = telegram:get_nominal_current(U),
    _ = telegram:switch_to_manual_control(U),
    _ = telegram:switch_power_output_off(U),
    Timer = erlang:start_timer(100, self(), get_state),
    {ok, #state{ uart=U, nv=NV, na=NA, timer=Timer }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({set_voltage,V}, _From, State) when is_float(V) ->
    Reply = hex_epx:output([{id,"voltage"}], [{value,V}]),
    {reply, Reply, State};
handle_call({set_current,A}, _From, State) when is_float(A) ->
    Reply = hex_epx:output([{id,"current"}], [{value,A}]),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({remote_switch,[{value,1}]}, State) ->
    _R = telegram:switch_to_remote_control(State#state.uart),
    {noreply, State};
handle_cast({remote_switch,[{value,0}]}, State) ->
    _R = telegram:switch_to_manual_control(State#state.uart),
    {noreply, State};

handle_cast({output_switch,[{value,1}]}, State) ->
    _R = telegram:switch_power_output_on(State#state.uart),
    {noreply, State};
handle_cast({output_switch,[{value,0}]}, State) ->
    _R = telegram:switch_power_output_off(State#state.uart),
    {noreply, State};

handle_cast(_Msg, State) ->
    ?debug("handle_cast: got message ~p\n", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({timeout,_T,get_state}, State) ->
    {ok,PowerState} = telegram:get_state(State#state.uart,
					 State#state.nv,State#state.na),
    V = proplists:get_value(actual_voltage, PowerState, 0.0),
    A = proplists:get_value(actual_current, PowerState, 0.0),
    hex_epx:output([{id,"voltage"}], [{value,V}]),
    hex_epx:output([{id,"current"}], [{value,A}]),
    Timer = erlang:start_timer(100, self(), get_state),
    {noreply, State#state{ timer=Timer }};
handle_info(_Info, State) ->
    ?debug("handle_info: got info ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
