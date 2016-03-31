%%%-------------------------------------------------------------------
%%% @author maciejtokarski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. mar 2016 20:34
%%%-------------------------------------------------------------------
-module(t).
-author("maciejtokarski").
-compile(export_all).

t() ->
  ibrowse:start(),
  {ok, File} = file:read_file("t.t"),
  Map = bencoder:bendecode(File),
  Info = maps:get(<<"info">>, Map),
%%  error_logger:info_msg("~p~n",[Map]),
  InforStr = bencoder:bencode(Info),
  Hash = crypto:hash(sha, InforStr),
  EncHash = ibrowse_lib:url_encode(binary_to_list(Hash)),
  PeerId = "-EB0001-vN)Lmjw_X7*q",
  AnnounceUrl = binary_to_list(maps:get(<<"announce">>,Map)),
  FullUrl = AnnounceUrl++"?info_hash="++EncHash++"&peer_id="++PeerId++"&compact=1",
  {ok,"200",_,Payload} = ibrowse:send_req(FullUrl, [], get),
  ResponseMap = bencoder:bendecode(list_to_binary(Payload)),
  error_logger:info_msg("~p~n",[]),
  Peers = maps:get(<<"peers">>,ResponseMap),
  PeersDecoded = decode_peers(Peers),
  error_logger:info_msg("Peers ~p~n",[PeersDecoded]),
  [{IpAddr,Port}|_] = PeersDecoded,
  peer_client_connection:start_link(IpAddr,Port,Hash,PeerId).


decode_peers(Bin) ->decode_peers(Bin,[]).
decode_peers(<<A:8,B:8,C:8,D:8,Port:16,Rest/binary>>,Acc) ->
  decode_peers(Rest,Acc++[{{A,B,C,D},Port}]);
decode_peers(<<>>,Acc) ->Acc.
