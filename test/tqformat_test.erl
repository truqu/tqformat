-module(tqformat_test).

-include_lib("eunit/include/eunit.hrl").

format_file_test_() ->
  Files =
    filelib:fold_files("./test/data", "\.orig$", false, fun (X, Acc) -> [X | Acc] end, []),
  test_format_files(Files).

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
  Diff = tdiff:format_diff_lines(tdiff:diff_binaries(Expected, Actual)),
  case Diff of
    [] -> file:delete(Out);
    _ ->
      io:format("~s", [Diff]),
      throw({diff, "nonempty"})
  end.
