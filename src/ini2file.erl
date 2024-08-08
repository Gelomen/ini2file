-module(ini2file).

-export([
    init/1
]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = ini2file_prv_compile:init(State),
    {ok, State2} = ini2file_prv_clean:init(State1),
    {ok, State2}.
