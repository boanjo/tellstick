-module(tellstick_server).
-behaviour(gen_server).


-export([start_link/0, say_hello/0, is_wanted/1, send_to_port/1, print_all/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, get_device/2, get_all_devices/0,get_all_temperatures/0,get_all_humidities/0]).
-export([get_temperature/1, get_humidity/1, device/2]).
-record(state, {port}).
-record(device, {id, name, state, value, last_state_change_time}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->

    process_flag(trap_exit, true),

    ets:new(humidity_table, [named_table, set]),
    ets:new(temperature_table, [named_table, set]),
    ets:new(device_table, [named_table, set, {keypos, #device.id}, public]),

    PrivDir = code:priv_dir(tellstick),
    SharedLib = "tellstick_drv",
    case erl_ddll:load_driver(PrivDir, SharedLib) of
	ok -> ok;
	{error, already_loaded} -> ok;
	_ -> exit({error, could_not_load_driver})
    end,

    Self = self(),
    
    io:format("Startig ~p pid ~p~n ", [?MODULE, Self]),

%%    register(tellstick_port, Self),
    Port = open_port({spawn, SharedLib}, []),
    {ok, #state{port=Port}}.

say_hello() ->
    
    gen_server:call(?MODULE, hello).

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

get_temperature(out) ->
    [Val] = ets:lookup(temperature_table, 215), 
    Val;
get_temperature(in) ->
    [Val] = ets:lookup(temperature_table, 135), 
    Val.

get_humidity(out) ->
    [Val] = ets:lookup(humidity_table, 215), 
    Val;
get_humidity(in) ->
    [Val] = ets:lookup(humidity_table, 135), 
    Val.

device(Id, on) ->
    send_to_port([1, Id]);
device(Id, off) ->
    send_to_port([1, Id]).



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


is_wanted(Id) ->
    case Id of
	215 ->
	    ok;
	135 ->
	    ok;
	_ ->
	    nok
    end.

get_device(Table, Key) when is_integer(Key) ->
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
			      
			      

print_next(_Table, '$end_of_table') ->
    ok;
print_next(Table, Prev) ->
    [{Id, Val, Time}] = ets:lookup(Table, Prev),
    io:format("~p: ~p ~p ~n", [Id, Val, Time]),
    print_next(Table, ets:next(Table, Prev)).

print_all(Table) ->
    First = ets:first(Table),
    print_next(Table, First).

%% callbacks

handle_call(hello, _From, State) ->
    
    io:format("Hello from server!~n", []),
    {reply, ok, State};

handle_call({send, Msg}, _From, #state{port = Port} = State) ->
    
    io:format("Send to port!~n", []),
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
	    ets:insert(temperature_table, {Id, str_to_float(Value), erlang:now()});
	2 ->
	    ets:insert(humidity_table, {Id, str_to_float(Value), erlang:now()});
	_ ->
	    ok
    end,
        
    {noreply, State};
    
handle_info({device_change, Id, Method, _Data}, State) ->
    io:format("Device Change Id=~p, Method=~p~n", [Id, Method]),
    {noreply, State};    

handle_info({device_event, Id, LastSentCommand, _Data}, State) ->
    Prev = get_device(device_table, Id),
    New = Prev#device{state=LastSentCommand, last_state_change_time=erlang:now()},
    ets:insert(device_table, New),
    {noreply, State};    

handle_info({device_name, Id, _NameLength, Name}, State) ->
    Prev = get_device(device_table, Id),
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



