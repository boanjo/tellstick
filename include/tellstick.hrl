-ifndef(tellstick).
-define(tellstick, ok).

-record(device, {id, name, state, value, last_state_change_time}).
-record(temperature, {id, value, last_update_time}).
-record(humidity, {id, value, last_update_time}).
-record(rainrate, {id, value, last_update_time}).
-record(raintotal, {id, value, last_update_time}).


-endif.
