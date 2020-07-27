-module(tqpp).

%% Primitives
-export([t/1, empty/0]).

%% Combinators
-export([ indent/2
        , concat_with/2
        , cat/1
        , cat/2
        , vcat/1
        , vcat/2
        , sep/1
        , hsep/1
        , alt/2
        , flat_alt/2
        , grouping/2
        ]).

%% Rendering
-export([print/2]).

%% Types
-export_types([document/0]).

-type document() ::
        {s, non_neg_integer(), unicode:chardata()}
      | {cat, document(), document()}
      | {flush, document()}
      | {indent, integer(), document()}
      | {flat_alt, document(), document()}
      | {alt, document(), document()}
      | {grouping, document(), [{integer(), document()} | {fa, document(), document()}]}.

%%==============================================================================================
%% Primitives
%%==============================================================================================

-spec t(string()) -> document().
t(T) -> {s, string:length(T), T}.

-spec empty() -> document().
empty() -> t([]).

%%==============================================================================================
%% Combinators
%%==============================================================================================

-spec cat(Left :: document(), Right :: document()) -> document().
cat({s, LS, LC}, {s, RS, RC}) -> {s, LS + RS, [LC, RC]};
cat(L, R) -> {cat, L, R}.

-spec vcat(Left :: document(), Right :: document()) -> document().
vcat(Left, Right) -> cat(flush(Left), Right).

-spec concat_with(fun ((D, D) -> D), [D, ...]) -> D when D :: document().
concat_with(F, Xs) -> foldr1(F, Xs).

-spec cat([document(), ...]) -> document().
cat(Xs) -> concat_with(fun cat/2, Xs).

-spec vcat([document(), ...]) -> document().
vcat(Xs) -> concat_with(fun vcat/2, Xs).

-spec hsep([document(), ...]) -> document().
hsep(Docs) -> concat_with(fun (L, R) -> cat([L, t(" "), R]) end, Docs).

-spec sep([document(), ...]) -> document().
sep(Docs) -> grouping(t(" "), [{0, Doc} || Doc <- Docs]).

-spec alt(document(), document()) -> document().
alt(Left, Right) -> {alt, Left, Right}.

-spec indent(integer(), document()) -> document().
indent(0, Doc) -> Doc;
indent(I, {indent, I2, D}) -> indent(I + I2, D);
indent(I, Doc) -> {indent, I, Doc}.

-spec flat_alt(document(), document()) -> document().
flat_alt(Left, Right) ->
  case flattened(Left) of
    no_flat -> Right;
    Flat -> {flat_alt, Flat, Right}
  end.

-spec grouping(Sep :: document(), Docs :: [GDoc, ...]) -> document() when
    GDoc :: {Ident :: integer(), document()}
          | {fa, Horizontal :: document(), Vertical :: document()}.
grouping(_, [{_, Doc}]) -> Doc;
grouping(Sep, Docs) -> {grouping, Sep, Docs}.

%%==============================================================================================
%% Internal combinators
%%==============================================================================================

-spec flush(document()) -> document().
flush({flush, _} = D) -> D;
flush(D) -> {flush, D}.

-spec flattened(document()) -> no_flat | document().
flattened({s, _, _} = D) -> D;
flattened({cat, L, R}) ->
  case flattened(L) of
    no_flat -> no_flat;
    FlatL ->
      case flattened(R) of
        no_flat -> no_flat;
        FlatR -> cat(FlatL, FlatR)
      end
  end;
flattened({flush, _}) -> no_flat;
flattened({indent, I, D}) ->
  case flattened(D) of
    no_flat -> no_flat;
    FlatD -> {indent, I, FlatD}
  end;
flattened({flat_alt, L, R}) ->
  case flattened(R) of
    no_flat -> L;
    FlatR -> {flat_alt, L, FlatR}
  end;
flattened({alt, L, R}) ->
  case {flattened(L), flattened(R)} of
    {no_flat, no_flat} -> no_flat;
    {no_flat, FlatR} -> FlatR;
    {FlatL, no_flat} -> FlatL;
    {FlatL, FlatR} -> {alt, FlatL, FlatR}
  end;
flattened({grouping, Sep, Docs}) ->
  flattened(concat_with( fun (X, Y) -> cat([X, Sep, Y]) end
                       , lists:map( fun ({_, X}) -> X;
                                        ({fa, X, _}) -> X
                                    end
                                  , Docs
                                  )
                       )).

%%==============================================================================================
%% Rendering
%%==============================================================================================

-record(line, {content = [] :: unicode:chardata(), indent = 0 :: integer()}).

-spec print(Paper :: pos_integer(), document()) -> iodata().
print(Paper, Document) ->
  [{_, Lines} | _] = best_layouts([discard_invalid(Paper, interpret(Paper, Document))]),
  lists:map(fun print_line/1, Lines).

print_line(L) -> [spacing(L#line.indent), L#line.content, $\n].

-spec spacing(non_neg_integer()) -> iodata().
spacing(0) -> [];
spacing(2) -> "  ";
spacing(4) -> "    ";
spacing(8) -> "        ";
spacing(N) -> spacing_help(N, []).

-spec spacing_help(non_neg_integer(), iodata()) -> iodata().
spacing_help(0, Acc) -> Acc;
spacing_help(N, Acc) when N >= 8 -> spacing_help(N - 8, ["        " | Acc]);
spacing_help(N, Acc) when N >= 4 -> spacing_help(N - 4, ["    " | Acc]);
spacing_help(N, Acc) -> spacing_help(N - 1, [$\s | Acc]).

%%==============================================================================================
%% Internal rendering types
%%==============================================================================================

-record( m
       , { height :: non_neg_integer()
         , last_width :: non_neg_integer()
         , max_width :: non_neg_integer()
         }
       ).

-type lines() :: [#line{}].
-type layout() :: {#m{}, [lines()]}.

%%==============================================================================================
%% Interpretation
%%==============================================================================================

-spec interpret(pos_integer(), document()) -> [layout(), ...].
interpret(_, {s, Length, Content}) ->
  [{#m{height = 0, last_width = Length, max_width = Length}, [#line{content = Content}]}];
interpret(W, {cat, LDoc, RDoc}) ->
  [Ls, Rs] = pmap(interpret(W, _), [LDoc, RDoc]),
  Candidates = [concat_l(L, R) || L <- Ls, R <- Rs],
  best_layouts([discard_invalid(W, Candidates)]);
interpret(W, {flush, Doc}) -> [flush_l(L) || L <- interpret(W, Doc)];
interpret(W, {indent, I, Doc}) ->
  Layouts = best_layouts([discard_invalid(W - I, interpret(W - I, Doc))]),
  [indent_l(L, I) || L <- Layouts];
interpret(W, {flat_alt, LDoc, RDoc}) ->
  [Ls, Rs] = pmap(interpret(W, _), [LDoc, RDoc]),
  best_layouts([discard_invalid(W, Ls), discard_invalid(W, Rs)]);
interpret(W, {alt, LDoc, RDoc}) ->
  [Ls, Rs] = pmap(interpret(W, _), [LDoc, RDoc]),
  best_layouts([discard_invalid(W, Ls), discard_invalid(W, Rs)]);
interpret(W, {grouping, SepDoc, IDocs}) ->
  [Sep] = interpret(W, SepDoc),
  ILayouts = pmap( fun ({I, Doc}) -> {I, interpret(W, Doc)};
                       ({fa, FDoc, VDoc}) -> {fa, interpret(W, FDoc), interpret(W, VDoc)}
                   end
                 , IDocs
                 ),
  HCatElements = pmap( fun ({_, Layouts}) -> only_single_line(Layouts);
                           ({fa, Layouts, _}) -> only_single_line(Layouts)
                       end
                     , ILayouts
                     ),
  VCatElements = pmap( fun ({0, Layouts}) -> Layouts;
                           ({I, Layouts}) -> lists:map(indent_l(_, I), Layouts);
                           ({fa, _, Layouts}) -> Layouts
                       end
                     , ILayouts
                     ),
  Horizontal = discard_invalid( W
                              , foldl1( fun (Xs, Ys) ->
                                              [ concat_l(XSep, Y)
                                                || X <- Xs, Y <- Ys, XSep <- [concat_l(X, Sep)]
                                              ]
                                        end
                                      , HCatElements
                                      )
                              ),
  Vertical =
    foldl1( fun (Xs, Ys) ->
                  best_layouts([[concat_l(FX, Y) || X <- Xs, Y <- Ys, FX <- [flush_l(X)]]])
            end
          , VCatElements
          ),
  best_layouts([Horizontal, Vertical]).

-spec only_single_line([layout()]) -> [layout()].
only_single_line(Layouts) -> lists:takewhile(fun ({#m{height = H}, _}) -> H == 0 end, Layouts).

%%
%% Concat helpers
%%

-spec concat_l(layout(), layout()) -> layout().
concat_l({#m{height = 0, max_width = 0}, _}, R) -> R;
concat_l(L, {#m{height = 0, max_width = 0}, _}) -> L;
concat_l({#m{height = 0} = LM, [L]}, {#m{height = 0} = LR, [R]}) ->
  {concat_m(LM, LR), [concat_line(L, R)]};
concat_l({#m{last_width = 0} = LM, LC}, {RM, RC}) ->
  {concat_m(LM, RM), lists:append(lists:droplast(LC), RC)};
concat_l({LM, LC}, {RM, RC}) ->
  Indent = LM#m.last_width,
  {Lefts, LastLeft} = pop_last(LC),
  [FirstRight | Rights] = RC,
  { concat_m(LM, RM)
  , lists:append( Lefts
                , [concat_line(LastLeft, FirstRight)
                   | lists:map(fun (L) -> L#line{indent = L#line.indent + Indent} end, Rights)]
                )
  }.

-spec concat_m(#m{}, #m{}) -> #m{}.
concat_m(#m{} = L, #m{} = R) ->
  #m{ height = L#m.height + R#m.height
    , last_width = L#m.last_width + R#m.last_width
    , max_width = max(L#m.max_width, (L#m.last_width + R#m.max_width))
    }.

-spec concat_line(#line{}, #line{}) -> #line{}.
concat_line(L = #line{}, R = #line{}) ->
  #line{indent = L#line.indent + R#line.indent, content = [L#line.content, R#line.content]}.

%%
%% Flush helpers
%%

-spec flush_m(#m{}) -> #m{}.
flush_m(A) -> A#m{last_width = 0, height = A#m.height + 1}.

-spec flush_l(layout()) -> layout().
flush_l({M, C}) -> {flush_m(M), lists:append(C, [#line{}])}.

%%
%% Indent helpers
%%

-spec indent_l(layout(), integer()) -> layout().
indent_l({M, C}, I) ->
  { M#m{max_width = M#m.max_width + I, last_width = M#m.last_width + I}
  , lists:map(fun (L) -> L#line{indent = L#line.indent + I} end, C)
  }.

%%
%% Layout optimizers
%%

-spec dominates(layout(), layout()) -> boolean().
dominates({{_, LH, LW, LMW}, _}, {{_, RH, RW, RMW}, _}) ->
  LH =< RH andalso LW =< RW andalso LMW =< RMW.

-spec discard_invalid(pos_integer(), [layout(), ...]) -> [layout(), ...].
discard_invalid(_, []) -> [];
discard_invalid(W, Layouts) when is_list(Layouts) ->
  case lists:filter(fits(W, _), Layouts) of
    [] -> [narrowest(Layouts)];
    Valids -> Valids
  end.

-spec fits(pos_integer(), layout()) -> boolean().
fits(W, {#m{max_width = MW}, _}) -> W >= MW.

-spec narrowest([layout(), ...]) -> layout().
narrowest([{LM, _} = L, {RM, _} = R | Rest]) ->
  case LM#m.max_width =< RM#m.max_width of
    true -> narrowest([L | Rest]);
    false -> narrowest([R | Rest])
  end;
narrowest([L]) -> L.

-spec best_layouts([[layout(), ...]]) -> [layout(), ...].
best_layouts(Layouts) -> pareto(mergeAll(Layouts), []).

-spec pareto([layout()], [layout()]) -> [layout(), ...].
pareto([], Acc) -> lists:reverse(Acc);
pareto([X | Xs], []) -> pareto(Xs, [X]);
pareto([X | Xs], Acc) ->
  case lists:any(dominates(_, X), Acc) of
    true -> pareto(Xs, Acc);
    false -> pareto(Xs, [X | Acc])
  end.

-spec mergeAll([[layout()]]) -> [layout()].
mergeAll(Xs) -> lists:foldl(fun merge/2, [], Xs).

-spec merge([layout()], [layout()]) -> [layout()].
merge([], Xs) -> Xs;
merge([X | Xs], []) -> merge(Xs, [X]);
merge([X | Xs], [X | _] = Ys) -> merge(Xs, Ys);
merge([{MX, _} = X | Xs] = AXs, [{MY, _} = Y | Ys] = AYs) ->
  case MX =< MY of
    true -> [X | merge(Xs, AYs)];
    false -> [Y | merge(AXs, Ys)]
  end.

%%
%% Generic helpers
%%

-spec foldr1(fun ((X, X) -> X), [X, ...]) -> X.
foldr1(F, Xs) ->
  lists:foldr( fun (X, null) -> X;
                   (X, Y) -> F(X, Y)
               end
             , null
             , Xs
             ).

-spec foldl1(fun ((X, X) -> X), [X, ...]) -> X.
foldl1(_, [X]) -> X;
foldl1(F, [X, Y | Xs]) -> foldl1(F, [F(X, Y) | Xs]).

-spec pop_last([X, ...]) -> {[X], X}.
pop_last(Xs) ->
  lists:foldr( fun (X, null) -> {[], X};
                   (X, {Acc, L}) -> {[X | Acc], L}
               end
             , null
             , Xs
             ).

pmap(_, []) -> [];
pmap(F, [X]) -> [F(X)];
pmap(F, Es) ->
  Parent = self(),
  collect([spawn(fun () -> Parent ! {self(), F(E)} end) || E <- Es]).

collect([]) -> [];
collect([Pid | Next]) -> receive {Pid, Res} -> [Res | collect(Next)] end.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
