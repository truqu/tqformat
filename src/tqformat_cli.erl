-module(tqformat_cli).

%% API
-export([main/1, default_opts/0, do/1, opts/0]).

-type opts() :: #{ width := pos_integer()
                 , mode := overwrite | verify | {write, file:name_all()}
                 , verbose := boolean()
                 , files := [stdin | file:name_all()]
                 }.

-export_type([opts/0]).

%%==============================================================================================
%% API
%%==============================================================================================

-spec main(any()) -> any().
main([]) -> getopt:usage(opts(), "tqformat");
main(Argv) ->
  io:setopts([binary, {encoding, unicode}]),
  case getopt:parse(opts(), Argv) of
    {ok, {ArgOpts, []}} -> do(ArgOpts);
    {ok, {ArgOpts, ExtraFiles}} -> do([{files, ExtraFiles} | ArgOpts]);
    {error, Error} ->
      io:put_chars(standard_error, [getopt:format_error(opts(), Error), "\n"]),
      getopt:usage(opts(), "tqformat")
  end.

-spec do(proplists:proplist()) -> any().
do(Args) -> exec(parse_opts(Args)).

-spec opts() -> [tuple()].
opts() ->
  [ {width, $w, "width", {integer, 96}, "width of paper"}
  , {verify, undefined, "verify", undefined, "verify only (no write)"}
  , {verbose, undefined, "verbose", undefined, "verbose"}
  , {files, undefined, undefined, string, "files to format/verify"}
  ].

%%==============================================================================================
%% Internal functions -- Args
%%==============================================================================================

parse_opts(Opts) -> parse_opts(Opts, [], default_opts()).

parse_opts([{files, Files} | Rest], Acc, Opts) ->
  parse_opts(Rest, expand_files(Files, Acc), Opts);
parse_opts([{width, W} | Rest], Acc, Opts) -> parse_opts(Rest, Acc, Opts#{width => W});
parse_opts([verify | Rest], Acc, Opts) -> parse_opts(Rest, Acc, Opts#{mode => verify});
parse_opts([verbose | Rest], Acc, Opts) -> parse_opts(Rest, Acc, Opts#{verbose => true});
parse_opts([], [], _) -> error_out("Invalid options\n", 4);
parse_opts([], Acc, Opts) -> Opts#{files => Acc}.

-spec default_opts() -> opts().
default_opts() -> #{width => 96, mode => overwrite, verbose => false, files => []}.

-spec expand_files([string(), ...], Files) -> Files | no_return() when
    Files :: [stdin | file:name_all()].
expand_files(_, [stdin]) -> no_combine_stdin_with_files();
expand_files("-", _) -> [stdin];
expand_files(File, Acc) when is_integer(hd(File)) ->
  case filelib:is_regular(File) of
    true -> [File | Acc];
    false ->
      case filelib:wildcard(File) of
        [] ->
          io:format(standard_error, "no file matching '~s'~n", [File]),
          Acc;
        Files -> Files ++ Acc
      end
  end;
expand_files(Files, Acc) -> lists:foldl(fun expand_files/2, Acc, Files).

-spec no_combine_stdin_with_files() -> no_return().
no_combine_stdin_with_files() -> error_out("stdin cannot be combined with other files\n", 4).

-spec error_out(unicode:chardata(), pos_integer()) -> no_return().
error_out(Msg, Code) ->
  io:put_chars(standard_error, Msg),
  erlang:halt(Code).

%%==============================================================================================
%% Internal functions -- Execution
%%==============================================================================================

-spec exec(opts()) -> any().
exec(#{files := Files} = Opts) ->
  Me = self(),
  await([ spawn(fun () ->
                      Me ! {self(), format_file(File, Opts)},
                      ok
                end) || File <- Files
        ]).

await([]) -> ok;
await([Pid | Rest]) -> receive {Pid, _} -> await(Rest) end.

print_warning(E) -> io:format(standard_error, "~p~n", [E]).

-spec format_file(stdin | file:name_all(), opts()) -> ok | no_return().
format_file(File, Opts) ->
  Start = erlang:timestamp(),
  Res = tqformat:format_file(File, Opts),
  format_status(File, Start, erlang:timestamp(), Opts),
  case Res of
    {ok, Warnings} ->
      lists:foreach(fun print_warning/1, Warnings),
      ok;
    {error, E} ->
      print_warning(E),
      erlang:halt(4)
  end.

format_status(_, _, _, #{verbose := false}) -> ok;
format_status(File, Start, End, #{verbose := true}) ->
  Duration = float_to_list(timer:now_diff(End, Start) / 1000000, [{decimals, 5}]),
  erlang:display({done, File, Duration}).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
