-module(sequences).
-behaviour(supervisor).
-behaviour(application).

%% API
-export([start/0]).
-export([stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Application callbacks
-export([start/2]).
-export([stop/1]).


%% API

-spec start() ->
    {ok, _}.
start() ->
    application:ensure_all_started(?MODULE).

-spec stop() ->
    ok.
stop() ->
    application:stop(?MODULE).

%% Supervisor callbacks

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, Ip} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    ChildSpec = woody_server:child_spec(
        ?MODULE,
        #{
            ip            => Ip,
            port          => genlib_app:env(?MODULE, port, 8022),
            net_opts      => genlib_app:env(?MODULE, net_opts, []),
            event_handler => {scoper_woody_event_handler, #{log_scope => 'rpc.server'}},
            handlers      => [
                get_handler_spec(sequences),
                get_handler_spec(state_processor)
            ]
        }
    ),
    {ok, {
        #{strategy => one_for_all, intensity => 6, period => 30},
        [ChildSpec]
    }}.

get_handler_spec(sequences) ->
    {"/v1/sequences", {
        {seq_proto_sequences_thrift, 'Sequences'},
        seq_handler
    }};

get_handler_spec(state_processor) ->
    {"/v1/stateproc", {
        {mg_proto_state_processing_thrift, 'Processor'},
        seq_machine
    }}.

%% Application callbacks

-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
