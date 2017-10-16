-module(seq_handler).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()}.

handle_function(Func, Args, Context, Opts) ->
    scoper:scope(sequences,
        fun() -> handle_function_(Func, Args, Context, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()}.
handle_function_('GetCurrent', [SequenceId], Context, _Opts) ->
    scoper:add_meta(#{
        id => SequenceId
    }),
    Value = seq_machine:get_current(SequenceId, Context),
    {ok, Value};

handle_function_('GetNext', [SequenceId], Context, _Opts) ->
    scoper:add_meta(#{
        id => SequenceId
    }),
    Value = seq_machine:get_next(SequenceId, Context),
    {ok, Value}.
