# TQFormat

> Applying the TruQu formatting preferences to our Erlang-ish code

TQFormat is a rebar3 plugin for applying our formatting preferences to our code.

It is highly opinionated and very much unconfigurable. The only configuration
option concerns the page-width.

It is roughly based on Jean-Philippe Bernardy's [A Pretty But Not Greedy
Printer](https://dl.acm.org/doi/pdf/10.1145/3110250), and uses
[erlfmt](https://github.com/WhatsApp/erlfmt/) for parsing Erlang-ish code into
an AST. That AST is then fed to the formatter, which turns the AST into a
document-tree. The document-tree, finally, is evaluated given the paper-width,
which produces a set of printable lines.

## Examples

Our formatting style is largely based on how Emacs formats comma-first Erlang
code. Hence, a map like this:

```erlang
#{this => is, a => record, with => many, keys => and, values => and, wont => fit, on => a, single => line}.
```

will result in the following:

```erlang
#{ this => is
 , a => record
 , with => many
 , keys => and
 , values => and
 , wont => fit
 , on => a
 , single => line
 }.
```

We inline singular clauses in `case` (etc) expressions, though multiple
expressions will always be laid out as an indented block.

Therefore, this code:

```erlang
case Foo of bar -> do(something), ok; baz -> help end.
```

Will be formatted like so:

```erlang
case Foo of
  bar ->
    do(something),
    ok;
  baz -> help
end.
```

## Usage

This is a rebar3 plugin, so you'll want to add it to the plugins section of the
configuration.

Configuration can be provided through `rebar.config` or through command-line
parameters.

## License

TQFormat is released under the Apache 2.0 license, as found in the `LICENSE`
file.
