-module(rebar3_format_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([ {name, format}
                              , {module, ?MODULE}
                              , {bare, true}
                              , {deps, []}
                              , {example, "rebar3 format"}
                              , {opts, tqformat_cli:opts()}
                              , {short_desc, "Erlang code formatter"}
                              , {desc, "Erlang code formatter"}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
  ConfigOpts = rebar_state:get(State, tqformat, []),
  {ArgOpts, ExtraFiles} = case rebar_state:command_parsed_args(State) of
                            {ParsedArgOpts, []} -> {ParsedArgOpts, []};
                            {ParsedArgOpts, OptFiles} -> {ParsedArgOpts, [{files, OptFiles}]}
                          end,
  Opts = ConfigOpts ++ ExtraFiles ++ ArgOpts,
  tqformat_cli:do(Opts),
  {ok, State}.

format_error(E) -> io_lib:format("~p", [E]).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
