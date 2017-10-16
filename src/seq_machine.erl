-module(seq_machine).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([get_current/2]).
-export([get_next/2]).

%% State processor

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-define(NS, <<"sequences">>).
-define(NIL, {nl, #msgpack_Nil{}}).
-define(INIT, 0).

-type id()          :: mg_proto_base_thrift:'ID'().
-type context()     :: woody_context:ctx().

%%

-spec get_next(id(), context()) ->
    integer().

get_next(Id, Context) ->
    {ok, AuxState} = call_automaton_with_lazy_start('Call', Id, [?NIL], Context),
    log_result(get_sequence_value(AuxState), "Sequence stepped").

-spec get_current(id(), context()) ->
    integer().

get_current(Id, Context) ->
    {ok, #'Machine'{aux_state = AuxState}} = call_automaton_with_lazy_start('GetMachine', Id, Context),
    log_result(get_sequence_value(AuxState), "Sequence fetched").

get_sequence_value(AuxState) ->
    unmarshal(AuxState).

log_result(Value, Message) ->
    ok = scoper:add_meta(#{value => Value}),
    _ = lager:info(Message),
    Value.

%%

ensure_started(Id, Context) ->
    case call_automaton('Start', [?NS, Id, ?NIL], Context) of
        {ok, _} ->
            ok;
        {exception, #'MachineAlreadyExists'{}} ->
            ok
    end.

call_automaton(Function, Id, Args, Context) ->
    Descriptor = construct_descriptor({id, Id}),
    call_automaton(Function, [Descriptor|Args], Context).

call_automaton(Function, Args, Context) ->
    Request = {{mg_proto_state_processing_thrift, 'Automaton'}, Function, Args},
    {ok, URL} = application:get_env(sequences, automaton_service_url),
    Opts = #{
        url           => URL,
        event_handler => {scoper_woody_event_handler, #{log_scope => 'rpc.client'}}
    },
    woody_client:call(Request, Opts, Context).

call_automaton_with_lazy_start(Function, Id, Context) ->
    call_automaton_with_lazy_start(Function, Id, [], Context).

call_automaton_with_lazy_start(Function, Id, Args, Context) ->
    case call_automaton(Function, Id, Args, Context) of
        {ok, _} = Ok ->
            Ok;
        {exception, #'MachineNotFound'{}} ->
            ok = ensure_started(Id, Context),
            call_automaton(Function, Id, Args, Context)
    end.

construct_descriptor(Ref) ->
    #'MachineDescriptor'{
        ns = ?NS,
        ref = Ref,
        range = #'HistoryRange'{}
    }.

%%

-type func() :: 'ProcessSignal' | 'ProcessCall'.

-spec handle_function(func(), woody:args(), context(), woody:options()) ->
    {ok, term()}.

handle_function(Func, Args, Context, Opts) ->
    scoper:scope(machine,
        fun() -> handle_function_(Func, Args, Context, Opts) end
    ).

-spec handle_function_(func(), woody:args(), context(), woody:options()) ->
    {ok, term()}.

handle_function_('ProcessSignal', [Args], _Context, _Opts) ->
    #'SignalArgs'{signal = {init, _}, machine = #'Machine'{id = ID}} = Args,
    scoper:add_meta(#{
        namespace => sequences,
        id => ID,
        activity => signal,
        signal => init
    }),
    {ok, #'SignalResult'{
        change = construct_change(init()),
        action = #'ComplexAction'{}
    }};

handle_function_('ProcessCall', [Args], _Context, _Opts) ->
    #'CallArgs'{machine = #'Machine'{id = ID, aux_state = CurrentAuxState}} = Args,
    scoper:add_meta(#{
        namespace => sequences,
        id => ID,
        activity => call
    }),
    NextAuxState = process_call(CurrentAuxState),
    {ok, #'CallResult'{
        change = construct_change(NextAuxState),
        action = #'ComplexAction'{},
        response = NextAuxState
    }}.

construct_change(State) ->
    #'MachineStateChange'{
        events = [],
        aux_state = State
    }.

init() ->
    marshal(?INIT).

process_call(CurrentValue) ->
    NextValue = unmarshal(CurrentValue) + 1,
    marshal(NextValue).

%% Marshalling

marshal(Int) when is_integer(Int) ->
    {arr, [{i, 1}, {i, Int}]}.

unmarshal({arr, [{i, 1},  {i, Int}]}) ->
    Int.
