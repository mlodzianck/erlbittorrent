%%%-------------------------------------------------------------------
%%% @author maciejtokarski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. mar 2016 00:14
%%%-------------------------------------------------------------------
-module(peer_client_connection).
-author("maciejtokarski").

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, hashinfo, reminder = <<>>}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link(IpAddr, Port, HashInfo, PeerId) ->
  gen_server:start_link(?MODULE, {IpAddr, Port, HashInfo, PeerId}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init({IpAddr, Port, HashInfo, PeerId}) ->
  {ok, Socket} = gen_tcp:connect(IpAddr, Port, [{active, once}, {mode, binary}], 1000),
  gen_tcp:send(Socket, <<19:8, <<"BitTorrent protocol">>/bitstring, 0:64, HashInfo/binary, (list_to_binary(PeerId))/bitstring>>),
  {ok, #state{socket = Socket, hashinfo = HashInfo}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).


handle_info({tcp, _, Bin}, State = #state{reminder = Reminder}) ->
  {ok, Msgs, NewReminder} = bt_message_coder:decode_message(<<Reminder/binary, Bin/binary>>),
  [handle_bt_message(Msg,State) || Msg <-Msgs],
  inet:setopts(State#state.socket, [{active, once}]),
  {noreply, State#state{reminder = NewReminder}};
handle_info(Info, State) ->
  error_logger:info_msg("Got info ~p~n", [Info]),
  inet:setopts(State#state.socket, [{active, once}]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_bt_message(#{type := bitfield},#state{socket = Socket}) ->
  error_logger:info_msg("Got bitfield, sending interested~n",[]),
  gen_tcp:send(Socket, bt_message_coder:encode_message(#{type => interested}));
handle_bt_message(#{type := unchoke},#state{socket = Socket}) ->
  error_logger:info_msg("Got unchoke, sending request~n",[]),
  gen_tcp:send(Socket, bt_message_coder:encode_message(#{type => request,index => 0 , start => 0, length => 16384}));
handle_bt_message(Msg,_) ->
  error_logger:info_msg("Other message ~p~n",[Msg]).