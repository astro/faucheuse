-module(faucheuse).


-behaviour(application).

-export([start/2]).


start(normal, _Args) ->
    code:add_path("vendor/erlxslt"),
    code:add_path("vendor/iserve/ebin"),
    application:start(sasl),
    mnesia:create_schema([node()]),
    mnesia:start(),
    entities:init(),
    storage:init(),
    harvester_sup:start_link();

start(_, _) ->
    {error, badarg}.
