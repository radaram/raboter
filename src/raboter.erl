-module(raboter).
-behaviour(gen_server).

-export([start_link/0, 
  send_message/2]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {url, update_id}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_message(ChatID, Text) ->
  set_command(set_command_url(), "chat_id=" ++ integer_to_list(ChatID) ++ "&text=" ++ Text).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gen server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
  io:format("---Start bot---~n"),
  inets:start(),
  ssl:start(),
  self() ! flush,
  {ok, #state{url = get_command_url(), update_id = 0}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(flush, State) ->
  Response = parse_response(get_command(State#state.url ++ integer_to_list(State#state.update_id + 1))),
  {JsonObj} = jiffy:decode(Response),
  Result = proplists:get_value(<<"result">>, JsonObj, []),
  case Result of
	[{[{<<"update_id">>, NewUpdateId} | _]}] -> 
      timer:send_after(3000, self(), flush);
	[] -> 
	  timer:send_after(3000, self(), command_check),
	  NewUpdateId = State#state.update_id
  end,
  {noreply, State#state{update_id = NewUpdateId}};

handle_info(command_check, State) ->
  Response = parse_response(get_command(State#state.url ++ integer_to_list(State#state.update_id + 1))),
  {JsonObj} = jiffy:decode(Response),
  Result = proplists:get_value(<<"result">>, JsonObj, []),
  case Result of
	[{[{<<"update_id">>, NewUpdateId}, {<<"message">>, {Message}} |_]}] -> 
      {From} = proplists:get_value(<<"from">>, Message),
	  ChatID = proplists:get_value(<<"id">>, From),
      Command = binary_to_list(proplists:get_value(<<"text">>, Message)),
	  TargetModule = application:get_env(target),
	  case TargetModule of
	    undefined ->
		  run_command(ChatID, Command);
		_ ->
	      erlang:apply(TargetModule, run_command, [ChatID, Command])
	  end;
	[] -> 
	  NewUpdateId = State#state.update_id
  end,
  timer:send_after(3000, self(), command_check),
  {noreply, State#state{update_id = NewUpdateId}};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ssl:stop(),
  inets:stop().

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_command(Url) ->
  request(get, {Url, []}).

set_command(Url, Data) ->
  Response = request(post, {Url, [], "application/x-www-form-urlencoded", Data}),
  {ok, {{"HTTP/1.1",ReturnCode, State}, _Head, _Body}} = Response,
  io:format("~w / ~w~n", [ReturnCode, State]).

request(Method, Body) ->
  httpc:request(Method, Body, [{ssl,[{verify,0}]}], []).

parse_response({ok, { _, _, Body}}) ->
  Body.

get_token() ->
  {ok, Directory} = file:get_cwd(),
  {ok, Data} = file:read_file(Directory ++ "/token.tok"),
  binary_to_list(Data).

get_base_url() ->
  "https://api.telegram.org/bot" ++ get_token().

get_command_url() ->
  get_base_url() ++ "/getUpdates?offset=".

set_command_url() ->
  get_base_url() ++ "/sendMessage".

run_command(ChatID, Text) ->
  send_message(ChatID, "Message is being ignored -> Message: <<" ++ Text ++ ">> check if the environment variable is set correctly").
