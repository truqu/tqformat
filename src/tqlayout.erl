-module(tqlayout).

%% API
-export([layout/1]).

%%==============================================================================================
%% API
%%==============================================================================================

-spec layout(erlfmt_parse:abstract_form()) -> tqpp:document().
layout({function, Meta, Clauses}) ->
  add_comments(true, Meta, tqpp:cat(layout_clauses(Clauses), tqpp:t(".")));
layout({attribute, Meta, Name, []}) ->
  add_comments(true, Meta, tqpp:cat([tqpp:t("-"), layout_expr(Name), tqpp:t(".")]));
layout({attribute, Meta, {atom, _, RawName} = Name, [Value]}) when
    RawName =:= opaque;
    RawName =:= type ->
  FName = layout_expr(Name),
  FValue = layout_expr(Value),
  Doc = tqpp:cat([tqpp:t("-"), FName, tqpp:t(" "), FValue, tqpp:t(".")]),
  add_comments(true, Meta, Doc);
layout({attribute, Meta, {atom, _, RawName}, [Value]}) when
    RawName =:= spec;
    RawName =:= callback ->
  AttrNameS = atom_to_list(RawName),
  AttrName = tqpp:cat([tqpp:t("-"), tqpp:t(AttrNameS), tqpp:t(" ")]),
  {spec, _, FName, Clauses} = Value,
  Doc = layout_spec(string:length(AttrNameS) + 2, AttrName, FName, Clauses),
  add_comments(true, Meta, Doc);
layout({attribute, Meta, Name, Values}) ->
  Doc = tqpp:cat([tqpp:t("-"), layout_expr(Name), call(Values), tqpp:t(".")]),
  add_comments(true, Meta, Doc);
layout(Expr) ->
  Meta = element(2, Expr),
  Doc = do_layout_expr(Expr),
  case maps:get(dot, Meta, false) of
    true -> add_comments(false, Meta, tqpp:cat(Doc, tqpp:t(".")));
    false -> add_comments(false, Meta, Doc)
  end.

%%==============================================================================================
%% Dealing with expressions
%%==============================================================================================

-spec layout_expr(erlfmt_parse:abstract_expr()) -> tqpp:document().
layout_expr(Expr) when is_tuple(Expr) ->
  Meta = element(2, Expr),
  Doc = do_layout_expr(Expr),
  add_comments(false, Meta, maybe_wrap_in_parens(Meta, Doc));
layout_expr(Expr) -> do_layout_expr(Expr).

do_layout_expr({string, Meta, _}) -> tqpp:t(erlfmt_scan:get_anno(text, Meta));
do_layout_expr({char, #{text := "$ "}, $\s}) -> tqpp:t("$\\s");
do_layout_expr({Atomic, Meta, _Value}) when
    Atomic =:= integer;
    Atomic =:= float;
    Atomic =:= char;
    Atomic =:= atom;
    Atomic =:= var ->
  tqpp:t(erlfmt_scan:get_anno(text, Meta));
do_layout_expr({remote, _, Left, Right}) ->
  tqpp:cat([layout_expr(Left), tqpp:t(":"), layout_expr(Right)]);
do_layout_expr({concat, _, Values}) -> layout_concat(Values);
do_layout_expr({clause, _, Head, Guards, Body}) -> layout_clause(Head, Guards, Body);
do_layout_expr({call, _, Name, Args}) -> tqpp:cat(layout_expr(Name), call(Args));
do_layout_expr({list, _, Values}) -> container(Values, tqpp:t("["), tqpp:t("]"));
do_layout_expr({tuple, _, Values}) -> container(Values, tqpp:t("{"), tqpp:t("}"));
do_layout_expr({op, _Meta, Op, Expr}) -> layout_unary_op(Op, Expr);
do_layout_expr({op, _Meta, Op, Left, Right}) -> layout_binary_op(Op, Left, Right);
do_layout_expr({'case', _, Expr, Clauses}) ->
  Prefix = surround_block(tqpp:t("case"), layout_expr(Expr), tqpp:t("of")),
  surround_block(Prefix, layout_clauses(Clauses), tqpp:t("end"));
do_layout_expr({cons, _, Head, Tail}) ->
  HeadD = layout_expr(Head),
  TailD = tqpp:hsep([tqpp:t("|"), layout_expr(Tail)]),
  tqpp:sep([HeadD, TailD]);
do_layout_expr({'fun', _, Expr}) -> layout_fun_expr(Expr);
do_layout_expr({args, _, Values}) -> call(Values);
do_layout_expr({guard_or, _, Guards}) -> layout_guard(tqpp:t(";"), Guards);
do_layout_expr({guard_and, _, Guards}) -> layout_guard(tqpp:t(","), Guards);
do_layout_expr({record_field, _, Key}) -> layout_expr(Key);
do_layout_expr({record_field, _, Key, Value}) -> layout_field("=", Key, Value);
do_layout_expr({record_field, _, Expr, Name, Key}) ->
  tqpp:cat([layout_expr(Expr), tqpp:t("#"), layout_expr(Name), tqpp:t("."), layout_expr(Key)]);
do_layout_expr({record, _, Name, Values}) ->
  tqpp:cat([tqpp:t("#"), layout_expr(Name), container_o(Values, tqpp:t("{"), tqpp:t("}"))]);
do_layout_expr({record, _, Expr, Name, Values}) ->
  tqpp:cat([ layout_expr(Expr)
           , tqpp:t("#")
           , layout_expr(Name)
           , container_o(Values, tqpp:t("{"), tqpp:t("}"))
           ]);
do_layout_expr({bin, _, Values}) ->
  tqpp:cat([tqpp:t("<"), container_o(Values, tqpp:t("<"), tqpp:t(">>"))]);
do_layout_expr({'...', _}) -> tqpp:t("...");
do_layout_expr({'try', _, Exprs, OfClauses, CatchClauses, After}) ->
  layout_try_expr(Exprs, OfClauses, CatchClauses, After);
do_layout_expr({'catch', _, Exprs}) ->
  Exprs1 = lists:map(fun layout_expr/1, Exprs),
  tqpp:concat_with(fun (A, B) -> tqpp:cat([A, tqpp:t(":"), B]) end, Exprs1);
do_layout_expr({'receive', _, Clauses}) ->
  surround_block(tqpp:t("receive"), layout_clauses(Clauses), tqpp:t("end"));
do_layout_expr({'receive', _, [], AfterE, AfterB}) ->
  After = layout_receive_expr("receive after", AfterE, AfterB),
  End = tqpp:t("end"),
  tqpp:sep([After, End]);
do_layout_expr({'receive', _, Clauses, AfterE, AfterB}) ->
  After = layout_receive_expr("after", AfterE, AfterB),
  Head = before_block(tqpp:t("receive"), layout_clauses(Clauses)),
  End = tqpp:t("end"),
  tqpp:sep([Head, After, End]);
do_layout_expr({'if', _, Clauses}) ->
  surround_block(tqpp:t("if"), layout_clauses(Clauses), tqpp:t("end"));
do_layout_expr({map, _, Values}) ->
  tqpp:cat(tqpp:t("#"), container_o(Values, tqpp:t("{"), tqpp:t("}")));
do_layout_expr({map, _, Expr, Values}) ->
  tqpp:cat([layout_expr(Expr), tqpp:t("#"), container_o(Values, tqpp:t("{"), tqpp:t("}"))]);
do_layout_expr({map_field_assoc, _, Key, Value}) -> layout_field("=>", Key, Value);
do_layout_expr({map_field_exact, _, Key, Value}) -> layout_field(":=", Key, Value);
do_layout_expr({block, _, Exprs}) ->
  surround_block(tqpp:t("begin"), layout_block(Exprs), tqpp:t("end"));
do_layout_expr({macro_call, _, Name, none}) -> tqpp:cat(tqpp:t("?"), layout_expr(Name));
do_layout_expr({macro_call, _, Name, Args}) ->
  tqpp:cat([tqpp:t("?"), layout_expr(Name), call(Args)]);
do_layout_expr({bin_element, _, Expr, Size, Types}) -> layout_bin_element(Expr, Size, Types);
do_layout_expr({bin_size, _, Left, Right}) ->
  tqpp:cat([layout_expr(Left), tqpp:t("*"), layout_expr(Right)]);
do_layout_expr({lc, _, Expr, Exprs}) -> layout_l_comprehension(Expr, Exprs);
do_layout_expr({bc, _, Expr, Exprs}) -> layout_b_comprehension(Expr, Exprs);
do_layout_expr({generate, _, Left, Right}) -> layout_op_top("<-", Left, Right);
do_layout_expr({b_generate, _, Left, Right}) -> layout_op_top("<=", Left, Right);
do_layout_expr({comment, _, _} = E) -> layout_comment(E);
do_layout_expr(Expr) -> throw({unsupported, Expr}).

layout_concat(Values) -> tqpp:vcat(lists:map(fun layout_expr/1, Values)).

%%==============================================================================================
%% Dealing with various common structures
%%==============================================================================================

layout_l_comprehension(Expr, Exprs) ->
  layout_comprehension(Expr, Exprs, tqpp:t("["), tqpp:t("]")).

layout_b_comprehension(Expr, Exprs) ->
  tqpp:cat([tqpp:t("<"), layout_comprehension(Expr, Exprs, tqpp:t("<"), tqpp:t(">>"))]).

layout_comprehension(Expr, Exprs, Before, After) ->
  ExprD = layout_expr(Expr),
  ExprsD = comprehension_items(Exprs),
  Vert =
    tqpp:alt( tqpp:vcat(tqpp:cat([Before, tqpp:t(" "), ExprD, tqpp:t(" "), ExprsD]), After)
            , tqpp:vcat([tqpp:cat([Before, tqpp:t(" "), ExprD]), tqpp:indent(2, ExprsD), After])
            ),
  tqpp:flat_alt(tqpp:cat([Before, ExprD, tqpp:t(" "), ExprsD, After]), Vert).

comprehension_items([X | Xs]) ->
  tqpp:cat( tqpp:t("|")
          , tqpp:grouping( tqpp:empty()
                         , [{0, tqpp:cat(tqpp:t("| "), layout_expr(X))}
                            | [{0, tqpp:cat(tqpp:t(", "), layout_expr(D))} || D <- Xs]]
                         )
          ).

layout_receive_expr(Keyword, AfterE, AfterB) ->
  E = layout_expr(AfterE),
  B = layout_block(AfterB),
  Head = surround_block(tqpp:t(Keyword), E, tqpp:t("->")),
  before_block(Head, B).

layout_field(Op, Key, Value) ->
  KeyD = tqpp:hsep([layout_expr(Key), tqpp:t(Op)]),
  ValueD = layout_expr(Value),
  tqpp:alt(tqpp:grouping(tqpp:t(" "), [{0, KeyD}, {2, ValueD}]), tqpp:hsep([KeyD, ValueD])).

layout_fun_expr({function, _, Name, Arity}) ->
  tqpp:cat([tqpp:t("fun "), layout_expr(Name), tqpp:t("/"), layout_expr(Arity)]);
layout_fun_expr({function, _, Mod, Name, Arity}) ->
  tqpp:cat([ tqpp:t("fun ")
           , layout_expr(Mod)
           , tqpp:t(":")
           , layout_expr(Name)
           , tqpp:t("/")
           , layout_expr(Arity)
           ]);
layout_fun_expr({clauses, _, Clauses}) ->
  ClausesD = layout_clauses(Clauses),
  tqpp:sep([tqpp:hsep([tqpp:t("fun"), ClausesD]), tqpp:t("end")]);
layout_fun_expr(type) -> tqpp:t("fun ()");
layout_fun_expr({type, _, Args, Result}) ->
  ArgsD = call(Args),
  ResultD = layout_expr(Result),
  tqpp:cat([tqpp:t("fun ("), ArgsD, tqpp:t(" -> "), ResultD, tqpp:t(")")]);
layout_fun_expr(Expr) -> throw({unsupported, Expr}).

layout_try_expr(Exprs, OfClauses, CatchClauses, AfterE) ->
  Head = layout_try(Exprs, OfClauses),
  Catch = layout_catch(CatchClauses),
  After = layout_after(AfterE),
  Clauses = Catch ++ After,
  tqpp:sep([Head | Clauses] ++ [tqpp:t("end")]).

layout_try(Exprs, []) -> before_block(tqpp:t("try"), layout_block(Exprs));
layout_try(Exprs, OfClauses) ->
  Body = layout_block(Exprs),
  before_block(surround_block(tqpp:t("try"), Body, tqpp:t("of")), layout_clauses(OfClauses)).

layout_catch([]) -> [];
layout_catch(Clauses) -> [before_block(tqpp:t("catch"), layout_clauses(Clauses))].

layout_after([]) -> [];
layout_after(Clauses) -> [before_block(tqpp:t("after"), layout_clauses(Clauses))].

-spec call([any()]) -> tqpp:document().
call(Args) -> container(Args, tqpp:t("("), tqpp:t(")")).

container([{comment, _, _}] = Args, L, R) -> container_o(Args, L, R);
container([Arg], L, R) -> tqpp:cat([L, layout_expr(Arg), R]);
container(Xs, L, R) -> container_o(Xs, L, R).

container_o([], L, R) -> tqpp:cat(L, R);
container_o([X], L, R) ->
  {ForceVert, X0} = layout_first_container_expr(X),
  case ForceVert of
    true -> tqpp:vcat(tqpp:cat(L, X0), R);
    false -> tqpp:grouping(tqpp:empty(), [{fa, tqpp:cat(L, X0), tqpp:hsep([L, X0])}, {0, R}])
  end;
container_o([X | Xs], L, R) ->
  {ForceVert0, X0} = layout_first_container_expr(X),
  Items = lists:map(fun layout_other_container_expr/1, Xs),
  case ForceVert0 orelse lists:any(element(1, _), Items) of
    true ->
      TheItems = lists:map(fun ({_, {_, E}}) -> E end, Items),
      tqpp:vcat([tqpp:hsep([L, X0])] ++ TheItems ++ [R]);
    false ->
      TheItems = lists:map(fun ({_, E}) -> E end, Items),
      tqpp:grouping( tqpp:empty()
                   , [{fa, tqpp:cat(L, X0), tqpp:hsep([L, X0])} | TheItems] ++ [{0, R}]
                   )
  end.

layout_first_container_expr({comment, _, _} = E) -> {true, layout_comment(E)};
layout_first_container_expr(E) -> {false, layout_expr(E)}.

layout_other_container_expr({comment, _, _} = E) ->
  {true, {0, tqpp:cat(tqpp:t("  "), layout_comment(E))}};
layout_other_container_expr(E) -> {false, {0, tqpp:cat(tqpp:t(", "), layout_expr(E))}}.

surround_block(Prefix, Entries, Suffix) ->
  tqpp:grouping(tqpp:t(" "), [{0, Prefix}, {2, Entries}, {0, Suffix}]).

before_block(Prefix, Entries) -> tqpp:grouping(tqpp:t(" "), [{0, Prefix}, {2, Entries}]).

layout_unary_op(Op, Expr) when
    Op =:= 'not';
    Op =:= 'bnot';
    Op =:= 'catch' ->
  tqpp:hsep([tqpp:t(atom_to_list(Op)), layout_expr(Expr)]);
layout_unary_op(Op, Expr) -> tqpp:cat(tqpp:t(atom_to_list(Op)), layout_expr(Expr)).

layout_binary_op(Op, Left, Right) when
    Op =:= '=';
    Op =:= '::' ->
  layout_op_top(atom_to_list(Op), Left, Right);
layout_binary_op('/', {atom, _, _} = Left, {integer, _, _} = Right) ->
  tqpp:cat([layout_expr(Left), tqpp:t("/"), layout_expr(Right)]);
layout_binary_op('|', Left, Right) -> layout_union([layout_expr(Left)], Right);
layout_binary_op('..', Left, Right) ->
  tqpp:cat([layout_expr(Left), tqpp:t(".."), layout_expr(Right)]);
layout_binary_op(Op, Left, Right) ->
  OpD = tqpp:t(atom_to_list(Op)),
  LeftD = layout_expr(Left),
  RightD = layout_expr(Right),
  Tail = tqpp:hsep([OpD, RightD]),
  tqpp:grouping(tqpp:t(" "), [{0, LeftD}, {2, Tail}]).

layout_op_top(Op, Left, Right) -> layout_field(Op, Left, Right).

layout_union(Acc, {op, Meta, '|', Left, Right}) ->
  layout_union([add_comments(false, Meta, layout_expr(Left)) | Acc], Right);
layout_union(Acc, Right) ->
  [X | Xs] = lists:reverse([layout_expr(Right) | Acc]),
  Xs1 = lists:map(tqpp:cat(tqpp:t("| "), _), Xs),
  tqpp:grouping(tqpp:t(" "), [{0, X} | [{-2, X0} || X0 <- Xs1]]).

layout_bin_element(Expr, Size, Types) ->
  tqpp:cat([layout_expr(Expr), layout_bin_size(Size), layout_bin_types(Types)]).

layout_bin_size(default) -> tqpp:empty();
layout_bin_size(Size) -> tqpp:cat(tqpp:t(":"), layout_expr(Size)).

layout_bin_types(default) -> tqpp:empty();
layout_bin_types(Types) ->
  TypesD = lists:map(fun layout_expr/1, Types),
  Joined = tqpp:concat_with(fun (L, R) -> tqpp:cat([L, tqpp:t("-"), R]) end, TypesD),
  tqpp:cat(tqpp:t("/"), Joined).

%%==============================================================================================
%% Dealing with clauses
%%==============================================================================================

-spec layout_clauses([erlfmt_parse:abstract_form(), ...]) -> tqpp:document().
layout_clauses(Clauses) -> layout_vertical_with_sep(Clauses, tqpp:t(";")).

layout_clause(empty, Guards, Body) -> layout_clause(Guards, empty, Body);
layout_clause(Head, empty, Body) ->
  HeadD = layout_expr(Head),
  BodyD = layout_block(Body),
  Prelude = tqpp:hsep([HeadD, tqpp:t("->")]),
  tqpp:grouping(tqpp:t(" "), [{0, Prelude}, {2, BodyD}]);
layout_clause(Head, Guards, Body) ->
  HeadD = tqpp:hsep([layout_expr(Head), tqpp:t("when")]),
  GuardD = tqpp:hsep([layout_expr(Guards), tqpp:t("->")]),
  BodyD = layout_block(Body),
  GHead = tqpp:grouping(tqpp:t(" "), [{0, HeadD}, {4, GuardD}]),
  tqpp:grouping(tqpp:t(" "), [{0, GHead}, {2, BodyD}]).

layout_spec(_, AttrN, FName, [Clause]) ->
  tqpp:cat(layout_spec_clause(tqpp:cat(AttrN, layout_expr(FName)), Clause), tqpp:t("."));
layout_spec(AttrL, AttrN, {atom, Meta, _} = FName, [Clause | Clauses]) ->
  Prelude = tqpp:cat(AttrN, layout_expr(FName)),
  First = layout_spec_clause(Prelude, Clause),
  Spacing = tqpp:indent(AttrL + string:length(erlfmt_scan:get_anno(text, Meta)), tqpp:empty()),
  Rest = lists:map(layout_spec_clause(Spacing, _), Clauses),
  tqpp:cat(vertical_with_sep([First | Rest], tqpp:t(";")), tqpp:t(".")).

layout_spec_clause(Prelude, {spec_clause, _, Head, Body, empty}) ->
  FHeadD = tqpp:cat(layout_expr(Head), tqpp:t(" ->")),
  HeadD = tqpp:cat(Prelude, FHeadD),
  BodyD = layout_block(Body),
  tqpp:alt( tqpp:hsep([HeadD, BodyD])
          , tqpp:cat(Prelude, tqpp:vcat(FHeadD, tqpp:indent(2, BodyD)))
          );
layout_spec_clause(Prelude, {spec_clause, _, Head, Body, Guards}) ->
  HeadD = tqpp:cat([Prelude, layout_expr(Head), tqpp:t(" ->")]),
  BodyD = layout_block(Body),
  GuardD = layout_expr(Guards),
  tqpp:flat_alt( tqpp:hsep([HeadD, BodyD, tqpp:t("when"), GuardD])
               , tqpp:alt( tqpp:vcat( tqpp:hsep([HeadD, BodyD, tqpp:t("when")])
                                    , tqpp:indent(4, GuardD)
                                    )
                         , tqpp:vcat( tqpp:hsep([HeadD, BodyD])
                                    , tqpp:indent(10, tqpp:hsep([tqpp:t("when"), GuardD]))
                                    )
                         )
               ).

layout_block(Exprs) -> layout_vertical_with_sep(Exprs, tqpp:t(",")).

vertical_with_sep(Docs, Sep) ->
  tqpp:concat_with(fun (X, Y) -> tqpp:vcat([tqpp:cat(X, Sep), Y]) end, Docs).

layout_vertical_with_sep(Exprs, Sep) ->
  vertical_with_sep([layout_expr(Expr) || Expr <- Exprs], Sep).

layout_guard(Sep, Guards) -> layout_vertical_with_sep(Guards, Sep).

%%==============================================================================================
%% Utils
%%==============================================================================================

maybe_wrap_in_parens(Meta, Doc) ->
  case erlfmt_scan:get_anno(parens, Meta, false) of
    true -> tqpp:cat([tqpp:t("("), Doc, tqpp:t(")")]);
    false -> Doc
  end.

%%==============================================================================================
%% Deal with comments
%%==============================================================================================

add_comments(AddSpace, Meta, Doc) ->
  Pre = erlfmt_scan:get_anno(pre_comments, Meta, []),
  Start = erlfmt_scan:get_line(Meta),
  RStart = erlfmt_scan:get_inner_line(Meta),
  Separated = AddSpace andalso (RStart - Start) - length(Pre) > 1,
  Post = erlfmt_scan:get_anno(post_comments, Meta, []),
  add_post_comments(Post, add_pre_comments(Pre, Separated, Doc)).

add_pre_comments([], _, Doc) -> Doc;
add_pre_comments(Comments, true, Doc) ->
  tqpp:vcat([stack_comments(Comments), tqpp:empty(), Doc]);
add_pre_comments(Comments, false, Doc) -> tqpp:vcat(stack_comments(Comments), Doc).

add_post_comments([], Doc) -> Doc;
add_post_comments(Comments, Doc) -> tqpp:hsep([Doc, stack_comments(Comments)]).

stack_comments(Comments) -> tqpp:vcat(lists:map(fun layout_comment/1, Comments)).

layout_comment({comment, _Meta, Lines}) -> tqpp:vcat(lists:map(fun tqpp:t/1, Lines)).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
