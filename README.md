# ea_ps2000b

Control the Elektro Automatik PS 2000B series from Erlang

Typical usage ( linux in this case )

    {ok,U} = telegram:open("/dev/ttyACM0").
    telegram:switch_to_remote_control(U).
    telegram:get_actual_state(U).

    telegram:set_actual_voltage(U, 12.0).
    telegram:switch_power_output_on(U).
    telegram:get_actual_state(U).

