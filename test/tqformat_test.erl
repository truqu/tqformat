-module(tqformat_test).

-include_lib("eunit/include/eunit.hrl").

format_file_test_() ->
  Files =
    filelib:fold_files("./test/data", "\.orig$", false, fun (X, Acc) -> [X | Acc] end, []),
  {inparallel, test_format_files(Files)}.

test_format_files([]) -> [];
test_format_files([F | Files]) ->
  [{F, fun () -> test_format_file(F) end} | test_format_files(Files)].

test_format_file(Filename) ->
  Dir = filename:dirname(Filename),
  Basename = filename:basename(Filename, ".orig"),
  Out = filename:join([Dir, filename:flatten([Basename, ".result"])]),
  ExpectedF = filename:join([Dir, filename:flatten([Basename, ".formatted"])]),
  {ok, _} = tqformat:format_file(Filename, Out),
  {ok, Expected} = file:read_file(ExpectedF),
  {ok, Actual} = file:read_file(Out),
  verify_format(Expected, Actual),
  file:delete(Out).

format_docs_test_() ->
  Files = ["FORMAT.md", "README.md"],
  {inparallel, test_format_md_files(Files)}.

test_format_md_files([]) -> [];
test_format_md_files([F | Files]) ->
  [{F, test_format_md_file(F)} | test_format_md_files(Files)].

test_format_md_file(Filename) ->
  {ok, Content} = file:read_file(Filename),
  {_, Codeblocks} = lists:foldr( fun collect_code/2
                               , {text, []}
                               , string:split(binary_to_list(Content), "```", all)
                               ),
  {inparallel, test_format_md_blocks(Filename, Codeblocks)}.

test_format_md_blocks(_, []) -> [];
test_format_md_blocks(Filename, [B | Blocks]) ->
  [{Filename, fun () -> test_format_md_block(B) end} | test_format_md_blocks(Filename, Blocks)].

test_format_md_block(Original) ->
  {ok, Formatted, []} = tqformat:format_string(Original),
  verify_format(Original, Formatted).

collect_code(_, {text, Acc}) -> {code, Acc};
collect_code(S, {code, Acc}) ->
  [First, Code] = string:split(S, "\n"),
  Acc0 = case First of
           "erlang formatted" -> [Code | Acc];
           _ -> Acc
         end,
  {text, Acc0}.

verify_format(Expected, Actual) when is_list(Expected) ->
  verify_format(unicode:characters_to_binary(Expected), Actual);
verify_format(Expected, Actual) when is_list(Actual) ->
  verify_format(Expected, unicode:characters_to_binary(Actual));
verify_format(Expected, Actual) ->
  Diff = tdiff:format_diff_lines(tdiff:diff_binaries(Expected, Actual)),
  case Diff of
    [] -> ok;
    _ ->
      io:format("~s", [Diff]),
      throw({diff, "nonempty"})
  end.
