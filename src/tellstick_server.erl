-module(tellstick_server).
-behaviour(gen_server).

-include("../include/tellstick.hrl").

-export([start_link/0, send_to_port/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, add_or_get_device/2]).
-export([get_all_devices/0,get_all_temperatures/0,get_all_humidities/0, get_all_rainrates/0, get_all_raintotals/0]).
-export([get_device/1, get_humidity/1,get_temperature/1, get_rainrate/1, get_raintotal/1]).
-export([device/2]).
-record(state, {port}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->

    process_flag(trap_exit, true),

    ets:new(humidity_table, [named_table, set, {keypos, #humidity.id}, public]),
    ets:new(rainrate_table, [named_table, set, {keypos, #rainrate.id}, public]),
    ets:new(raintotal_table, [named_table, set, {keypos, #raintotal.id}, public]),
    ets:new(temperature_table, [named_table, set, {keypos, #temperature.id}, public]),
    ets:new(device_table, [named_table, set, {keypos, #device.id}, public]),

    %%PrivDir = code:priv_dir(tellstick),
    PrivDir = priv_dir(?MODULE),
    SharedLib = "tellstick_drv",

    case erl_ddll:load_driver(PrivDir, SharedLib) of
	ok -> ok;
	{error, already_loaded} -> ok;
	_ -> exit({error, could_not_load_driver})
    end,

    Self = self(),
    
    io:format("Startig ~p pid ~p~n ", [?MODULE, Self]),

    Port = open_port({spawn, SharedLib}, []),
    {ok, #state{port=Port}}.


priv_dir(Mod) ->
    Ebin = filename:dirname(code:which(Mod)),
        filename:join(filename:dirname(Ebin), "priv").

get_next(_Table, '$end_of_table', Acc) ->
    Acc;
get_next(Table, Prev, Acc) ->
    [Val] = ets:lookup(Table, Prev),
    get_next(Table, ets:next(Table, Prev), [Val|Acc]).

get_all_devices() ->
    Table = device_table,
    First = ets:first(Table),
    get_next(Table, First, []).

get_all_temperatures() ->
    Table = temperature_table,
    First = ets:first(Table),
    get_next(Table, First, []).

get_all_humidities() ->
    Table = humidity_table,
    First = ets:first(Table),
    get_next(Table, First, []).

get_all_rainrates() ->
    Table = rainrate_table,
    First = ets:first(Table),
    get_next(Table, First, []).

get_all_raintotals() ->
    Table = raintotal_table,
    First = ets:first(Table),
    get_next(Table, First, []).

lookup(Table, Id) ->
    Val = ets:lookup(Table, Id),
    case Val of
	[] -> not_found;
	List -> [Ret] = List, 
		Ret
    end.
    

get_device(Id) ->
    lookup(device_table, Id).

get_temperature(Id) ->
    lookup(temperature_table, Id).

get_humidity(Id) ->
    lookup(humidity_table, Id).

get_rainrate(Id) ->
    lookup(rainrate_table, Id).

get_raintotal(Id) ->
    lookup(raintotal_table, Id).

device(Id, on) ->
    send_to_port([1, Id]);
device(Id, off) ->
    send_to_port([2, Id]).

send_to_port(Msg) ->
    gen_server:call(?MODULE, {send, Msg}).

trim_str([H|T], Acc) ->
    case H of
	0 ->
	    Acc;
	Character ->
	    trim_str(T, Acc++[Character])
    end;
trim_str([], Acc) ->
    Acc.


str_to_float(Str) ->
    TrimStr = trim_str(Str, []),
    
    try float(list_to_integer(TrimStr)) 
    catch error:_ -> list_to_float(TrimStr) 
    end.



add_or_get_device(Table, Key) when is_integer(Key) ->
    case ets:lookup(Table, Key) of
	[] ->
	    Val=#device{id=Key},
	  %%  io:format("Val=~p~n", [Val]),
	    ets:insert(Table, Val);
	_ ->
	    ok
    end,
    
    [Ret] = ets:lookup(Table, Key),
    Ret.
			      
%% callbacks

handle_call({send, Msg}, _From, #state{port = Port} = State) ->
  %%  io:format("Send ~p to port!~n", [Msg]),
    Port ! {self(), {command, Msg}},
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    io:format("handle_call~n", []),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    io:format("handle_cast~n", []),
    {noreply, State}.



handle_info({sensor_event, Id, DataType, Value}, State) ->

    case DataType of
	1 ->
	    ets:insert(temperature_table, 
		       #temperature{id=Id, 
				    value=str_to_float(Value), 
				    last_update_time=erlang:now()});
	2 ->
	    ets:insert(humidity_table, 
		       #humidity{id=Id, 
				 value=str_to_float(Value), 
				 last_update_time=erlang:now()});
	4 ->
	    ets:insert(rainrate_table, 
		       #rainrate{id=Id, 
				 value=str_to_float(Value), 
				 last_update_time=erlang:now()});
	8 ->
	    ets:insert(raintotal_table, 
		       #raintotal{id=Id, 
				 value=str_to_float(Value), 
				 last_update_time=erlang:now()});
	_ ->
	    ok
    end,
        
    {noreply, State};
    
handle_info({device_change, Id, Method, _Data}, State) ->
    io:format("Device Change Id=~p, Method=~p~n", [Id, Method]),
    {noreply, State};    

handle_info({device_event, Id, LastSentCommand, _Data}, State) ->
    Prev = add_or_get_device(device_table, Id),
    New = Prev#device{state=LastSentCommand, 
		      last_state_change_time=erlang:now()},
    ets:insert(device_table, New),
    {noreply, State};    

handle_info({device_name, Id, _NameLength, Name}, State) ->
    Prev = add_or_get_device(device_table, Id),
    ets:insert(device_table, Prev#device{name=Name}),
    {noreply, State};    

handle_info({output_conf, RetVal}, State) ->
    case RetVal of
	0 -> ok;
	Other ->
	    io:format("Negative confirmation from tellstick cmd = ~p~n", [Other])
    end,
    {noreply, State};    

handle_info({data, Data}, State) ->
    io:format("Data=~p~n", [Data]),
    {noreply, State};    

handle_info(Info, State) ->
    io:format("handle_info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{port = Port} = State) ->
    Port ! {self(), close},
    receive
	{Port, closed} ->
	    ok %%exit(normal)
    end,
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



