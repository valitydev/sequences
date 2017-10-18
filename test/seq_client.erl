-module(seq_client).

-export([new/0]).
-export([get_current/2]).
-export([get_next/2]).

-type client() :: woody_context:ctx().

-spec get_current(binary(), client()) -> integer().

get_current(SeqId, Client) ->
    call('GetCurrent', [SeqId], Client).

-spec get_next(binary(), client()) -> integer().

get_next(SeqId, Client) ->
    call('GetNext', [SeqId], Client).

-spec new() -> client().

new() ->
    woody_context:new().

call(Function, Args, Client) ->
    Call = {{seq_proto_sequences_thrift, 'Sequences'}, Function, Args},
    Opts = #{
        url => <<"http://sequences:8022/v1/sequences">>,
        event_handler => {scoper_woody_event_handler, #{log_scope => 'rpc.client'}}
    },
    case woody_client:call(Call, Opts, Client) of
        {ok, Response} ->
            Response;
        {exception, Exception} ->
            throw(Exception)
    end.
