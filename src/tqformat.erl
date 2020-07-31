-module(tqformat).

-type error_info() :: {file:name_all(), erl_anno:location(), module(), Reason :: any()}.

%% API
-export([main/1, init/1, format_file/2, format_string/1]).
-export([format/2]).

%%==============================================================================================
%% API
%%==============================================================================================

-spec main(any()) -> any().
main(Args) -> tqformat_cli:main(Args).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) -> rebar3_format_prv:init(State).

-spec format_file(file:name_all(), file:name_all() | tqformat_cli:opts()) -> Result when
    Result :: {ok, [error_info()]} | {error, error_info()}.
format_file(Filename, OutFilename) when is_list(OutFilename) ->
  format_file( Filename
             , #{width => 96, mode => {write, OutFilename}, verbose => false, files => []}
             );
format_file(Filename, #{width := Width, mode := Mode}) ->
  case format(Filename, Width) of
    {ok, Res, Warnings} ->
      case handle_result(Res, Filename, Mode) of
        ok -> {ok, Warnings};
        {error, _} = E -> E
      end;
    {error, _} = E -> E
  end.

-spec format_string(string()) -> {ok, string(), [error_info()]} | {error, error_info()}.
format_string(String) ->
  case read_string_nodes(String) of
    {ok, Forms, Warnings} ->
      Result = fold_docs(Forms, 96),
      {ok, unicode:characters_to_list(Result), Warnings};
    {error, _} = Err -> Err
  end.

%%==============================================================================================
%% Internal functions
%%==============================================================================================

-spec format(file:name_all(), pos_integer()) ->
              {ok, binary(), [error_info()]} | {error, error_info()}.
format(Filename, Width) ->
  case read_nodes(Filename) of
    {ok, Forms, Warnings} ->
      Result = fold_docs(Forms, Width),
      {ok, unicode:characters_to_binary(Result), Warnings};
    {error, _} = Err -> Err
  end.

handle_result(Res, stdin, _) -> io:put_chars(Res);
handle_result(Res, Filename, overwrite) -> file:write_file(Filename, Res);
handle_result(Res, _, {write, Filename}) -> file:write_file(Filename, Res);
handle_result(Expected, Filename, verify) ->
  {ok, Original} = file:read_file(Filename),
  case Original == Expected of
    true -> ok;
    false -> {error, {not_same, Filename}}
  end.

fold_docs([], _) -> "";
fold_docs([F], Width) -> format_node(F, Width);
fold_docs([X, Y | Rest], Width) ->
  case
    {characterize(X), characterize(Y), erlfmt_scan:get_anno(pre_comments, element(2, Y), [])}
  of
    {spec, other, _} -> [format_node(X, Width), fold_docs([Y | Rest], Width)];
    {T, T, []} when T =/= other -> [format_node(X, Width), fold_docs([Y | Rest], Width)];
    {_, _, _} -> [format_node(X, Width), $\n, fold_docs([Y | Rest], Width)]
  end.

-spec read_nodes(file:name_all()) -> {ok, AbsForms, Warnings} | {error, Error} when
    AbsForms :: [erlfmt_parse:abstract_form()],
    Warnings :: [error_info()],
    Error :: error_info().
read_nodes(stdin) -> read_nodes(erlfmt_scan:io_node(standard_io), stdin, [], []);
read_nodes(Filename) ->
  try file_read_nodes(Filename) catch {error, Error} -> {error, Error} end.

-spec read_string_nodes(string()) -> {ok, AbsForms, Warnings} | {error, Error} when
    AbsForms :: [erlfmt_parse:abstract_form()],
    Warnings :: [error_info()],
    Error :: error_info().
read_string_nodes(Data) ->
  try read_nodes(erlfmt_scan:string_node(Data), "nofile", [], [])
  catch {error, Error} -> {error, Error}
  end.

-spec format_node(erlfmt_parse:abstract_form(), pos_integer()) -> string().
format_node({raw_string, _Anno, String}, _) -> [String, $\n];
format_node(Node, Width) -> tqpp:print(Width, tqlayout:layout(Node)).

-spec characterize(erlfmt_parse:abstract_form()) -> atom().
characterize({attribute, _, {atom, _, RawName}, _}) -> RawName;
characterize(_) -> other.

file_read_nodes(Filename) ->
  case file:open(Filename, [read, {encoding, utf8}]) of
    {ok, File} ->
      try read_nodes(erlfmt_scan:io_node(File), Filename, [], []) after file:close(File) end;
    {error, Reason} -> throw({error, {Filename, 0, file, Reason}})
  end.

read_nodes({ok, Tokens, Comments, Cont}, Filename, Acc, Warnings0) ->
  {Node, Warnings} = parse_nodes(Tokens, Comments, Filename, Cont, Warnings0),
  read_nodes(erlfmt_scan:continue(Cont), Filename, [Node | Acc], Warnings);
read_nodes({eof, _}, _, Acc, Warnings) -> {ok, lists:reverse(Acc), lists:reverse(Warnings)};
read_nodes({error, {ErrLoc, Mod, Reason}, _}, Filename, _, _) ->
  throw({error, {Filename, ErrLoc, Mod, Reason}}).

parse_nodes([], _, _, Cont, Warnings) -> {node_string(Cont), Warnings};
parse_nodes([{shebang, Meta, String}], [], _, _, Warnings) ->
  {{raw_string, Meta, String}, Warnings};
parse_nodes(Tokens, Comments, Filename, Cont, Warnings) ->
  case erlfmt_parse:parse_node(Tokens) of
    {ok, Node} -> {erlfmt_recomment:recomment(Node, Comments), Warnings};
    {error, {ErrLoc, Mod, Reason}} ->
      Warning = {Filename, ErrLoc, Mod, Reason},
      {node_string(Cont), [Warning | Warnings]}
  end.

node_string(Cont) ->
  {String, Anno} = erlfmt_scan:last_node_string(Cont),
  {raw_string, Anno, string:trim(String, both, "\n")}.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
