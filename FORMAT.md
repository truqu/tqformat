# The TQFormat format/styleguide

TQFormat is highly opinionated, and barely configurable. The only real decision
granted to the user, is width of the page. Given this width and an AST, TQFormat
produces a pretty printed layout.

This layout is based on a number of rules regarding the acceptable ways of
formatting construct. These rules are generally based on the emacs pretty
printer when writing code in a comma-first style. There are some places,
however, where we diverge from what emacs does - for consistency and
readability.

Given those rules, TQFormat tries to find the output that
satisfies those rules, and takes up the least vertical and horizontal space,
either fitting the page or with the minimal amount of overflow if something
simply cannot be validly formatted to fit within the page.

The layouter tries to construct the layout with the least height, shortest last
line (preferring documents where, if something were to be added, it would happen
with the least amount of indentation), and finally, shortest maximal width. The
combination of these factors produces very pretty documents.

This document represents (some of) the rules applied to figure out acceptable
formats for all constructs.

## The module level

A module is considered to exist of a number of forms, which are either
attributes (`-foo(..).`) or function declarations (`foo(..) -> [..].`).

Function declarations are always followed by an extra newline.

```erlang formatted
foo(bar) -> baz.

hello() -> world.
```

Between two attributes of the same "type" - for example, two `-include(..).`
attributes - an extra newline will only appear if the second attribute is
preceded by a comment.

```erlang formatted
-include("file1.hrl").
-include("file2.hrl").

%% This is preceded by a comment, so receives some breathing space
-include("file3.hrl").
```

Between attributes of different "types" - for example, a `behaviour` and an
`include` attributes - a newline is always added. This can be used to group
similar attributes.

```erlang formatted
-behaviour(behaviour1).
-behaviour(behaviour2).

-include("file1.hrl").
```

`spec` attributes are assumed to directly precede the function declaration they
specify, and are "attached" to the declaration.

```erlang formatted
-spec id(X) -> X.
id(X) -> X.
```

Multiline comments preceding any attribute or function declaration are always
separated by a newline from the following attribute or function.

```erlang formatted
%% Multiline comment
%% before an attribute

-export([foo/0]).

%% Multiline comment
%% before a function declaration.

foo() -> bar.
```

## Attribute formatting

Attributes come in three flavours: parameterless attributes (like `-endif.`),
regular attributes with one or more parameters (like `-export(..).` or
`-define(..).`) and attributes that deal with types (`-type`, `-opaque`, `-spec`
and `-callback`).

For each of these categories, different rules apply.

### Parameterless attributes

A parameterless attribute is always simply rendered as `-`, the name of the
attribute, and finally, a `.`.

### Regular attributes with parameters

A regular attribute looks a lot like a regular function call, prefixed with `-`
and suffixed with `.`. In fact, the same formatting mechanism as used for
function calls is used for these types of attributes.

Without going into the specifics, this results in attributes like these:

```erlang formatted
-module(foo).

-export([fun1/1, fun1/2, fun2/1, fun2/3]).

-record( rec_type
       , { field1 :: Type1
         , %% Comment on a record field1
           field2 :: Type2
         }
       ).

-export([ more_fun/1
        , %% Comment
          more_fun/2
        ]).
```

### Attributes that deal with types

The commonality between all of these is that they are not exactly rendered as a
`call`. However, these attributes themselves still fall into two separate
groups: `type` and `opaque` on the one hand, and `spec` and `callback` on the
other hand.

The former group declares names given to types, the latter specifies the types
of a function.

More specifically, only the latter group supports guards.

Guards in `spec` and `callback` clauses are separated from the function head
using the keyword `when`. After that, the guards themselves follow. If there is
only a single guard, it may end up inlined with the head. If there are multiple,
they will always appear on separate lines. The `when` keyword may either appear
in the function head, with the guards indented by 4 spaces relative to the
beginning of the head, or on the next line, indented by 10 spaces, with the
first guard appearing on the same line. The choice made here depends on which
layout produces the least wide "last" line in each section.

```erlang formatted
-type my_type() :: #{an => inline_map}
                 | {a, tuple, with, multiple, elements}
                 | #some_record_type{}
                 | boolean().

-spec my_simple_fun(X) -> integer() when X :: atom().

%% With two guards, things go onto a line of their own.
-spec my_twoguard_fun(X) -> integer() when
    X :: atom(),
    X :: integer().

%% If `when` no longer fits, it moves onto the next line.
-spec my_very_long_function_name(With :: atom(), A :: atom(), Lot :: atom()) -> Of :: extra()
          when T :: T.

%% Multiple clauses are aligned.
-spec two_clauses(integer(), integer()) -> integer();
                 (float(), float()) -> float().
```

The clause heads do follow regular "call" formatting - see the "Generic
container formatting" chapter for more info.

## Function declaration formatting

> TODO.

## Generic "block" formatting

By "block", we generally mean a nonempty list of expressions that are separated
by commas, prefixed by some sort of "head" and optionally suffixed by some sort
of "footer".

The most straightforward (albeit not most common) example of such a generic
"block"-like construct, is the `begin ... end` block.

In such blocks, we differentiate between blocks with a single, single-line
expression, and blocks whose body takes up multiple lines. In case the body
takes up a single line, we attempt to fit both the opening delimited, body _and_
closing delimiter on a single line. If that is not possible, we switch to the
same algorithm used for multi-line body blocks: the body is indented 2 spaces
(relative to the start of the opening delimiter).

```erlang formatted
fn() -> begin foo:bar() end.

fn1() ->
  begin
    TwoExpressions,
    take(2, lines)
  end.
```

> TODO: Describe other kinds of blocks

## Generic container formatting

A "generic container" is a set of comma-separated expressions delimited by some
characters. These generally fall into two groups: flat lists of expressions
(such as lists, function calls, binaries and tuples) and lists of associations
(such as maps and records).

For the most part, both groups behave the same, except when dealing with a
single "entry" which spans multiple lines.

Generic containers that contains associations take no special care in cases
where they contain a single value, whereas with flat expression lists, the
delimiters in such cases are always "stuck" to the contained value.

Containers always have either all the contained values and delimiters on a
single line, or vertically laid out with the comma first and aligned to the last
character of the opening delimiter. Likewise, the first character of the closing
delimiter is aligned to the last character of the opening delimiter.

```erlang formatted
%% A simple tuple with items on a single line.
%% Note that no extra space surrounds the contained values.
f1() -> {foo, bar, baz}.

%% When a flat list contains a single multiline item, the delimiters are stuck
%% to the contained values
f2() ->
  [#{ foofoofoofoofoofoofoofoofoofoo => barbarbarbarbarbarbarbarbarbarbarbar
    , foofoofoofoofoofoofoofoofoofo_ => barbarbarbarbarbarbarbarbarbarbarbar
    }].

%% Comments in Erlang always span the rest of the line, so they force breaks.
%% Note that function call arguments are also laid out as a generic container.
f3() ->
  some_mod:some_fn( FirstArg
                  , %% Comment before the second argument
                    SecondArg
                  ).

%% Containers of association lists provide some extra space surrounding single
%% multiline expressions.
f4() ->
  #{ my_key => [ val1
               , %% comment to force a break
                 val2
               ]
   }.
```
