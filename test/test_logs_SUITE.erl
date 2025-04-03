-module(test_logs_SUITE).
-behavior(ct_suite).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("test_logs.hrl").

-compile([export_all, nowarn_export_all]).

-define(MFA_META, #{mfa := {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}}).

-define(MILLISECONDS(Action),
        element(1, timer:tc(fun() -> Action end, millisecond))).

-define(ASSERT_RECEIVE_ERROR(Timeout, Messages),
        {assertReceive, [{module, ?MODULE},
                         {line, _},
                         {pattern, "" ++ _},
                         {timeout, Timeout},
                         {messages, Messages}]}).

-define(assertNoDelay(Action), ?assert(?MILLISECONDS(Action) < 2)).

-define(assertTimeout(Action, Timeout), ?assert(?MILLISECONDS(Action) >= Timeout)).

-ifdef(DUMMY_MACRO_FOR_EFMT).
%% this is a workaround to avoid efmt failures when using macros
%% with guard expressions:
%%   https://github.com/sile/efmt/issues/98
-define(assertReceive(Pattern, Timeout), ok).
-endif.


all() ->
    [assertReceive_test,
     assertLogEvent_test,
     assertLogEvent_report_test,
     get_unmatched_log_events_test,
     gen_server_termination_test].


init_per_suite(Config) ->
    LoggerPrimaryConfig = logger:get_primary_config(),
    ok = logger:set_primary_config(level, debug),
    test_logs:add_handler(),
    [{logger_config, LoggerPrimaryConfig} | Config].


end_per_suite(Config) ->
    test_logs:remove_handler(),
    LoggerPrimaryConfig = proplists:get_value(logger_config, Config),
    ok = logger:set_primary_config(LoggerPrimaryConfig),
    Config.


assertReceive_test(_Config) ->
    Timeout = 1000,
    [ self() ! Msg || Msg <- [a, {e1, e2}, [i1, i2], #{k1 => v1, k2 => v2}] ],

    ?assertNoDelay(?assertReceive(#{k1 := _}, 0)),
    ?assertNoDelay(?assertReceive(List when is_list(List), Timeout)),

    ?assertNoDelay(?assertError(?ASSERT_RECEIVE_ERROR(0, [a, {e1, e2}]),
                                ?assertReceive(#{k1 := _}, 0))),

    ?assertTimeout(?assertError(?ASSERT_RECEIVE_ERROR(Timeout, [a, {e1, e2}]),
                                ?assertReceive(List when is_list(List), Timeout)),
                   Timeout).


assertLogEvent_test(_Config) ->
    Timeout = 100,

    test_logs:set_pid(),

    [ self() ! Msg || Msg <- [a, b] ],

    ?LOG_DEBUG("debug"),
    ?LOG_INFO("info123"),
    ?LOG_NOTICE("notice"),
    ?LOG_WARNING("warning ~p", [arg1]),

    [ self() ! Msg || Msg <- [c, d] ],

    ?LOG_ERROR(<<"error">>),
    ?LOG_CRITICAL(<<"critical123">>),
    ?LOG_ALERT(<<"alert">>),
    ?LOG_EMERGENCY(<<"emergency ~p">>, [arg1]),

    ?assertLogEvent({string, "debug" ++ _}, debug, ?MFA_META),
    ?assertLogEvent({string, <<"error", _/binary>>}, error, ?MFA_META),
    ?assertLogEvent({string, "info" ++ _}, info, ?MFA_META),
    ?assertLogEvent({string, <<"critical", _/binary>>}, critical, ?MFA_META),
    ?assertLogEvent({string, "" ++ _}, notice, ?MFA_META),
    ?assertLogEvent({string, <<"", _/binary>>}, alert, ?MFA_META),
    ?assertLogEvent({"warning" ++ _, [arg1]}, warning, ?MFA_META),
    ?assertLogEvent({<<"emergency", _/binary>>, [arg1]}, emergency, ?MFA_META),

    ?assertTimeout(
      ?assertError(?ASSERT_RECEIVE_ERROR(Timeout, [a, b, c, d]),
                   ?assertLogEvent({string, "debug" ++ _}, debug, ?MFA_META)),
      Timeout).


assertLogEvent_report_test(_Config) ->
    Timeout = 100,

    test_logs:set_pid(),

    [ self() ! Msg || Msg <- [a, b] ],

    ?LOG_DEBUG(#{debug => debug}),
    ?LOG_INFO(#{}),
    ?LOG_NOTICE(#{notice => notice, notice2 => "notice"}),

    [ self() ! Msg || Msg <- [c, d] ],

    ?LOG_WARNING([{warning, warning}, {warning2, "warning"}]),
    ?LOG_ERROR([{error, error}]),
    %% test "" ++ _ pattern for matching lists.
    ?LOG_CRITICAL(#{critical => []}),
    ?LOG_ALERT([{alert, alert}, {alert2, "alert"}]),
    %% an empty list is treated as an empty string.
    ?LOG_EMERGENCY([]),

    ?assertLogEvent({report, #{}}, debug, ?MFA_META),
    ?assertLogEvent({report, #{}}, info, ?MFA_META),
    ?assertLogEvent({report, #{notice := _}}, notice, ?MFA_META),
    ?assertLogEvent({report, [{warning, warning} | _]}, warning, ?MFA_META),
    ?assertLogEvent({report, [{error, error} | _]}, error, ?MFA_META),
    %% "" ++ _ is a pattern that matches empty and non-empty list.
    ?assertLogEvent({report, #{critical := "" ++ _}}, critical, ?MFA_META),
    ?assertLogEvent({report, "" ++ _}, alert, ?MFA_META),
    %% an empty list is treated as an empty string.
    ?assertLogEvent({string, ""}, emergency, ?MFA_META),

    ?assertTimeout(
      ?assertError(?ASSERT_RECEIVE_ERROR(Timeout, [a, b, c, d]),
                   ?assertLogEvent({report, #{}}, debug, ?MFA_META)),
      Timeout).


get_unmatched_log_events_test(_Config) ->
    test_logs:set_pid(),

    [ self() ! Msg || Msg <- [a, b] ],

    ?LOG_DEBUG("debug"),
    ?LOG_INFO("info123"),
    ?LOG_NOTICE("notice"),
    ?LOG_WARNING("warning ~p", [arg1]),

    [ self() ! Msg || Msg <- [c, d] ],

    ?LOG_ERROR(<<"error">>),
    ?LOG_CRITICAL(<<"critical123">>),
    ?LOG_ALERT(<<"alert">>),
    ?LOG_EMERGENCY(<<"emergency ~p">>, [arg1]),

    ?assertNoDelay(?assertLogEvent({_, "debug" ++ _}, _, ?MFA_META)),
    ?assertNoDelay(?assertLogEvent(_, error, ?MFA_META)),
    ?assertNoDelay(?assertLogEvent({string, "info" ++ _}, info, _)),
    ?assertNoDelay(?assertLogEvent({"warning" ++ _, _}, _, _)),

    UnmatchedLogEvents = test_logs:get_unmatched_log_events(),
    ct:log("UnmatchedLogEvents = ~p", [UnmatchedLogEvents]),
    ?assertEqual(4, length(UnmatchedLogEvents)),
    Levels = [ Level || #{level := Level} <- UnmatchedLogEvents ],
    ?assertEqual([notice, critical, alert, emergency], Levels).


gen_server_termination_test(_Config) ->
    test_logs:set_pid(),
    ?assertMatch({ok, Pid} when is_pid(Pid),
                 gen_server:start({local, dummy_gen_server},
                                  dummy_gen_server,
                                  no_args,
                                  [])),
    ?assertMatch(not_implemented, gen_server:call(dummy_gen_server, dummy_call)),
    UnmatchedLogEvents = test_logs:get_unmatched_log_events(),
    ct:log("UnmatchedLogEvents = ~p", [UnmatchedLogEvents]),
    ?assertNoDelay(
      ?assertLogEvent({report, #{
                                 label := {gen_server, terminate},
                                 name := dummy_gen_server,
                                 reason := {unexpected_call_request, dummy_call}
                                }},
                      error,
                      #{mfa := {gen_server, _, _}})),
    ?assertNoDelay(
      ?assertLogEvent({report, #{label := {proc_lib, crash}}},
                      error,
                      #{mfa := {proc_lib, crash_report, _}})).
