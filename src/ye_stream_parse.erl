-module(ye_stream_parse).
-export([bin/2]).

-include("yeml.hrl").

-record(pstate, {level = 0, expect_new_block = false, line = 1, cbs}).

-define(CALLBACK(NAME, ARGS, STATE), 
    (if 
        ((STATE#pstate.cbs)#ye_cb_state.NAME =:= undefined) -> STATE;
        true -> STATE#pstate{cbs =
                check_cbs(apply((STATE#pstate.cbs)#ye_cb_state.NAME,
                        [STATE#pstate.cbs | ARGS]))}
    end)).
-define(IS_WHITESPACE(C), C =:= $\ ; C=:= $\n; C =:= $\t).
-define(IS_NB_WHITESPACE(C), C =:= $\ ; C =:= $\t).
-define(IS_NOT_WHITESPACE(C), C =/= $\ , C =/= $\t, C=/= $\n).

-spec bin(binary(), #pstate{}) -> any().
bin(Binary, State) ->
    try 
        root(Binary, #pstate{cbs = State})
    catch 
        error:function_clause ->
            [{Module, Function, [Bin, StackState]}|_] = erlang:get_stacktrace(),
            {Near, _} = consume_until_newline(Bin),
            {parse_error, {{line, StackState#pstate.line}, {near, Near},
                    {module, Module}, {function, Function}}}
    end.

root(<<>>, State) ->
    State#pstate.cbs;
root(Bin, State) ->
    {Rest, NextState} = update_state(parse_root(Bin, State)),
    root(Rest, NextState).

parse_root(<<"---">>, State) ->
    X = ?CALLBACK(directives_end, [], State),
    {<<>>, ?CALLBACK(doc_begin, [], X)};
parse_root(<<"---", C, Rest/binary>>, State) when ?IS_WHITESPACE(C) ->
    X = ?CALLBACK(directives_end, [], State),
    {<<C, Rest/binary>>, ?CALLBACK(doc_begin, [], X)};
parse_root(<<"...", C, Rest/binary>>, State) when ?IS_WHITESPACE(C) ->
    {<<C, Rest/binary>>, ?CALLBACK(doc_end, [], State)};
parse_root(Bin, State) ->
    generic(Bin, State).

generic(Bin, #pstate{level = PrevLevel, expect_new_block = NewBlock} = Prev) ->
    {CurrentLevel, Rest} = strip_indentation(Bin),
    State = Prev#pstate{level = CurrentLevel, expect_new_block = false},
    if
        (CurrentLevel < PrevLevel) and not NewBlock ->
            {Bin, State};
        ((CurrentLevel > PrevLevel) and NewBlock) or
                ((CurrentLevel =:= PrevLevel) and not NewBlock) ->
            {Rest2, NextState} = update_state(parse_generic(Rest, State)),
            generic(Rest2, NextState);
        true ->
            throw({parse_error, State#pstate.line, Bin, ?MODULE, generic})

    end;
generic(<<>>, State) ->
    {<<>>, State}.

parse_generic(<<$#, CommentAndRest/binary>>, State) ->
    {Comment, Rest} = consume_until_newline(CommentAndRest),
    {Rest, ?CALLBACK(comment, [Comment], State)};
parse_generic(<<$-, C, Rest/binary>>, State) when ?IS_NB_WHITESPACE(C) ->
    % TODO: we could get a linebreak (after x whitespace?) after - as well...
    % DEAL WITH IT!
    {Rest2, NextState} = generic(Rest, ?CALLBACK(seq_begin, [], State)),
    {Rest2, ?CALLBACK(seq_end, [], NextState)};
parse_generic(<<C, Tokens/binary>>, State) when ?IS_NOT_WHITESPACE(C) ->
    case parse_scalar(<<C, Tokens/binary>>) of
        {mapping, Key, Rest} ->
            NextState = State#pstate{expect_new_block = true},
            generic(Rest, ?CALLBACK(mapping_begin, [Key], NextState));
        {mapping, Key, Scalar, Rest} ->
            X = ?CALLBACK(mapping_begin, [Key], State),
            {Rest, ?CALLBACK(scalar, [Scalar], X)};
        {plain, Scalar, Rest} ->
            {Rest, ?CALLBACK(scalar, [Scalar], State)}
    end.

parse_scalar(Bin) ->
    parse_scalar(Bin, <<>>).
parse_scalar(<<$:, Rest/binary>>, Acc) ->
    % FIXME we need to check if rest includes a simple plain scalar and then
    % return it...
    {mapping, Acc, Rest};
parse_scalar(<<C, Rest/binary>>, Acc) when ?IS_WHITESPACE(C) ->
    {plain, Acc, Rest}.

%%% Helper functions
consume_until_newline(Bin) ->
    consume_until_newline(Bin, <<>>).
consume_until_newline(<<>>, Acc) ->
    {Acc, <<>>};
consume_until_newline(<<$\n, Bin/binary>>, Acc) ->
    {Acc, <<$\n, Bin>>};
consume_until_newline(<<C, Bin/binary>>, Acc) ->
    consume_until_newline(Bin, <<Acc/binary, C>>).

update_state({<<$\n, Rest>>, State}) ->
    update_state({Rest, State#pstate{line = State#pstate.line + 1}});
update_state({<<C, Rest>>, State}) when ?IS_WHITESPACE(C) ->
    strip_empty_lines(Rest, State);
update_state(State) ->
    State.

strip_empty_lines(Bin, State) ->
    strip_empty_lines(Bin, State, Bin).
strip_empty_lines(<<C, Rest/binary>>, State, Bin) when ?IS_NB_WHITESPACE(C) ->
    strip_empty_lines(Rest, State, Bin);
strip_empty_lines(<<$\n, Rest/binary>>, State, _) ->
    strip_empty_lines(Rest, State#pstate{line = State#pstate.line + 1}, Rest);
strip_empty_lines(_, State, Bin) ->
    {Bin, State}.

strip_indentation(<<$\ , Rest>>) ->
    strip_indentation(<<$\ , Rest>>, 0).
strip_indentation(<<$\ , Rest>>, Level) ->
    strip_indentation(Rest, Level + 1);
strip_indentation(Rest, Level) ->
    {Level, Rest}.

check_cbs(#ye_cb_state{} = S) ->
    S;
check_cbs(S) ->
    erlang:error({bad_return, S}). 
