-define(assertReceive(Pattern, Timeout),
        begin
            ((fun() ->
                  receive
                      Pattern -> ok
                  after
                      Timeout ->
                          erlang:error({assertReceive,
                                        [{module, ?MODULE},
                                         {line, ?LINE},
                                         {pattern, (??Pattern)},
                                         {timeout, Timeout},
                                         process_info(self(), messages)]})
                  end
              end)())
        end).

-define(assertLogEvent(LogEventPattern),
        ?assertReceive(?LOG_EVENT_MESSAGE(LogEventPattern), 100)).

-define(assertLogEvent(MsgPattern, Level, MetaPattern),
        ?assertLogEvent(?LOG_EVENT_PATTERN(MsgPattern, Level, MetaPattern))).

-define(LOG_EVENT_MESSAGE(LogEvent), {log_event, LogEvent}).

-define(LOG_EVENT_PATTERN(MsgPattern, Level, MetaPattern),
        #{
          meta := MetaPattern,
          msg := MsgPattern,
          level := Level
         }).
