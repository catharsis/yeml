-module(ye_stream_parse).
-export([bin/2]).

-include("yeml.hrl").

-record(pstate, {directives = false, line = 1, cbs}).

-define(CALLBACK(NAME, ARGS, STATE), 
    (if 
        ((STATE#pstate.cbs)#ye_cb_state.NAME =:= undefined) -> STATE;
        true -> STATE#pstate{cbs =
                check_cbs(apply((STATE#pstate.cbs)#ye_cb_state.NAME,
                        [STATE#pstate.cbs | ARGS]))}
    end)).
-define(IS_WHITESPACE(C), C =:= $\ ; C=:= $\n; C =:= $\t).

-spec bin(binary(), #pstate{}) -> any().
bin(Binary, State) ->
    try 
        parse(Binary, #pstate{cbs = State})
    catch 
        error:function_clause ->
            [{Module, Function, [Bin, StackState]}|_] = erlang:get_stacktrace(),
            {Near, _} = consume_until_newline(Bin),
            {parse_error, {{line, StackState#pstate.line}, {near, Near},
                    {module, Module}, {function, Function}}}
    end.


parse(<<"---", C, Rest/binary>>, State) when ?IS_WHITESPACE(C) ->
    X = ?CALLBACK(directives_end, [], State),
    UpdState = update_line(X,C),
    parse(Rest, ?CALLBACK(doc_begin, [], UpdState));
parse(<<"...", C, Rest/binary>>, State) when ?IS_WHITESPACE(C) ->
    UpdState = update_line(State, C),
    parse(Rest, ?CALLBACK(doc_end, [], UpdState));
parse(<<$#, CommentAndRest/binary>>, State) ->
    {Comment, Rest} = consume_until_newline(CommentAndRest),
    UpdState = update_line(State, $\n),
    parse(Rest, ?CALLBACK(comment, [Comment], UpdState));
parse(<<$\n, Rest/binary>>, State) ->
    UpdState = update_line(State, $\n),
    parse(Rest, UpdState);
parse(<<>>, State) ->
    State#pstate.cbs.

consume_until_newline(Bin) ->
    consume_until_newline(Bin, <<>>).
consume_until_newline(<<>>, Acc) ->
    {Acc, <<>>};
consume_until_newline(<<$\n, Bin/binary>>, Acc) ->
    {Acc, Bin};
consume_until_newline(<<C, Bin/binary>>, Acc) ->
    consume_until_newline(Bin, <<Acc/binary, C>>).

update_line(State, $\n) ->
    State#pstate{line = State#pstate.line + 1 };
update_line(State, _) ->
    State.

check_cbs(#ye_cb_state{} = S) ->
    S;
check_cbs(S) ->
    erlang:error({bad_return, S}). 
