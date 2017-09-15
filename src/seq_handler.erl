-module(seq_handler).

%% Woody handler

-behaviour(woody_server_thrift_handler).
-behaviour(woody_event_handler).

-export([handle_function/4]).
-export([handle_event/4]).

%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()}.

handle_function('GetCurrent', [SequenceId], Context, _Opts) ->
    Value = seq_machine:get_current(SequenceId, Context),
    {ok, Value};

handle_function('GetNext', [SequenceId], Context, _Opts) ->
    Value = seq_machine:get_next(SequenceId, Context),
    {ok, Value}.

-spec handle_event(woody_event_handler:event(), woody:rpc_id(), woody_event_handler:event_meta(), woody:options()) ->
    ok.

handle_event(Event, RpcID, RawMeta, _) ->
    {Level, {Format, Args}, LogMeta} = woody_event_handler:format_event_and_meta(
        Event, RawMeta, RpcID, [event, service, function, type, metadata]),
    ok = lager:log(Level, [{pid, self()}, LogMeta], Format, Args).
