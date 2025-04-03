-module(dummy_gen_server).

-compile([export_all, nowarn_export_all]).


init(_) ->
    {ok, no_state}.


handle_call(Request, _From, State) ->
    {stop, {unexpected_call_request, Request}, not_implemented, State}.


handle_cast(Request, State) ->
    {stop, {unexpected_cast_request, Request}, State}.
