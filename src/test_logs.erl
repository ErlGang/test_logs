-module(test_logs).

-include("test_logs.hrl").

-export([add_handler/0,
         remove_handler/0,
         set_pid/0,
         get_unmatched_log_events/0]).

-define(LOGGER_HANDLER, test_logger_handler).


add_handler() ->
    ok = logger:add_handler(?LOGGER_HANDLER, ?LOGGER_HANDLER, #{level => debug}).


remove_handler() ->
    ok = logger:remove_handler(?LOGGER_HANDLER).


set_pid() ->
    ok = logger:set_handler_config(?LOGGER_HANDLER, config, self()).


get_unmatched_log_events() ->
    {messages, Messages} = process_info(self(), messages),
    [ LogEvent || ?LOG_EVENT_MESSAGE(LogEvent) <- Messages ].
