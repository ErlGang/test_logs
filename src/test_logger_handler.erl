-module(test_logger_handler).

-include("test_logs.hrl").

-export([log/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% logger_handler callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log(LogEvent, Config) ->
    %% The return value from this function is ignored by Logger
    case maps:get(config, Config, undefined) of
        Pid when is_pid(Pid) ->
            erlang:send(Pid, ?LOG_EVENT_MESSAGE(LogEvent));
        _ -> ok
    end.
