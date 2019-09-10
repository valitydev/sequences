-module(sequences_tests_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_current/1]).
-export([get_next/1]).

%% tests descriptions

-type config() :: [{atom(), term()}].

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-spec all() -> [atom()].
all() ->
    [
        get_current,
        get_next
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    Apps = genlib_app:start_application_with(scoper, [
        {storage, scoper_storage_logger}
    ]) ++ genlib_app:start_application_with(sequences, [
        {services, #{
            automaton => #{
                url =>"http://machinegun:8022/v1/automaton"
            }
        }}
    ]),
    [{suite_apps, Apps} | C].

-spec end_per_suite(config()) -> term().
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?config(suite_apps, C)).

%% tests

-spec init_per_testcase(atom(), config()) -> config().

init_per_testcase(_Name, C) ->
    Client = seq_client:new(),
    [{client, Client} | C].

-spec end_per_testcase(atom(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

-spec get_current(term()) -> term().
get_current(C) ->
    Client = proplists:get_value(client, C),
    SeqId = get_sequence_id(),
    0 = seq_client:get_current(SeqId, Client),
    0 = seq_client:get_current(SeqId, Client),
    _ = seq_client:get_next(SeqId, Client),
    1 = seq_client:get_current(SeqId, Client).

-spec get_next(term()) -> term().
get_next(C) ->
    Client = proplists:get_value(client, C),
    SeqId = get_sequence_id(),
    1 = seq_client:get_next(SeqId, Client),
    2 = seq_client:get_next(SeqId, Client).

%%

get_sequence_id() ->
    integer_to_binary(erlang:system_time(micro_seconds)).
