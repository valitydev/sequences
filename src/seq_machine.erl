-module(seq_machine).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([get_current/2]).
-export([get_next/2]).

%% State processor

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-define(NS, <<"sequences">>).
-define(NIL, {nl, #mg_msgpack_Nil{}}).
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
    {ok, Machine} = call_automaton_with_lazy_start('GetMachine', Id, Context),
    #mg_stateproc_Machine{aux_state = EncodedAuxState} = Machine,
    AuxState = unmarshal_state(EncodedAuxState),
    log_result(get_sequence_value(AuxState), "Sequence fetched").

get_sequence_value(AuxState) ->
    unmarshal(AuxState).

log_result(Value, Message) ->
    ok = scoper:add_meta(#{value => Value}),
    _ = logger:info(Message),
    Value.

%%

ensure_started(Id, Context) ->
    case call_automaton('Start', [?NS, Id, ?NIL], Context) of
        {ok, _} ->
            ok;
        {exception, #mg_stateproc_MachineAlreadyExists{}} ->
            ok
    end.

call_automaton(Function, Id, Args, Context) ->
    Descriptor = construct_descriptor({id, Id}),
    call_automaton(Function, [Descriptor|Args], Context).

call_automaton(Function, Args, Context) ->
    Request = {{mg_proto_state_processing_thrift, 'Automaton'}, Function, Args},
    Opts = make_woody_options(automaton),
    woody_client:call(Request, Opts, Context).

call_automaton_with_lazy_start(Function, Id, Context) ->
    call_automaton_with_lazy_start(Function, Id, [], Context).

call_automaton_with_lazy_start(Function, Id, Args, Context) ->
    case call_automaton(Function, Id, Args, Context) of
        {ok, _} = Ok ->
            Ok;
        {exception, #mg_stateproc_MachineNotFound{}} ->
            ok = ensure_started(Id, Context),
            call_automaton(Function, Id, Args, Context)
    end.

construct_descriptor(Ref) ->
    #mg_stateproc_MachineDescriptor{
        ns = ?NS,
        ref = Ref,
        range = #mg_stateproc_HistoryRange{}
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
    #mg_stateproc_SignalArgs{signal = {init, _}, machine = #mg_stateproc_Machine{id = ID}} = Args,
    _ = scoper:add_meta(#{
        namespace => sequences,
        id => ID,
        activity => signal,
        signal => init
    }),
    {ok, #mg_stateproc_SignalResult{
        change = construct_change(init()),
        action = #mg_stateproc_ComplexAction{}
    }};

handle_function_('ProcessCall', [Args], _Context, _Opts) ->
    #mg_stateproc_CallArgs{machine = #mg_stateproc_Machine{id = ID, aux_state = EncodedAuxState}} = Args,
    _ = scoper:add_meta(#{
        namespace => sequences,
        id => ID,
        activity => call
    }),
    CurrentAuxState = unmarshal_state(EncodedAuxState),
    NextAuxState = process_call(CurrentAuxState),
    {ok, #mg_stateproc_CallResult{
        change = construct_change(NextAuxState),
        action = #mg_stateproc_ComplexAction{},
        response = NextAuxState
    }};

handle_function_('ProcessRepair', [Args], _Context, _Opts) ->
    #mg_stateproc_RepairArgs{machine = #mg_stateproc_Machine{id = ID}} = Args,
    _ = scoper:add_meta(#{
        namespace => sequences,
        id => ID,
        activity => repair
    }),
    throw(#mg_stateproc_RepairFailed{reason = {str, <<"No repair support">>}}).

construct_change(State) ->
    #mg_stateproc_MachineStateChange{
        events = [],
        aux_state = marshal_state(State)
    }.

init() ->
    marshal(?INIT).

process_call(CurrentValue) ->
    NextValue = unmarshal(CurrentValue) + 1,
    marshal(NextValue).

-spec make_woody_options(atom()) ->
    woody_client:options().
make_woody_options(Service) ->
    Services = application:get_env(sequences, services, #{}),
    #{Service := ServiceOptions} = Services,
    ServiceOptions#{event_handler => scoper_woody_event_handler}.

%% Marshalling

marshal_state(State) ->
    #mg_stateproc_Content{
        format_version = undefined,
        data = State
    }.

unmarshal_state(#mg_stateproc_Content{format_version = undefined, data = State}) ->
    State.

marshal(Int) when is_integer(Int) ->
    {arr, [{i, 1}, {i, Int}]}.

unmarshal({arr, [{i, 1},  {i, Int}]}) ->
    Int.
