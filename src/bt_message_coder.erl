%%%-------------------------------------------------------------------
%%% @author maciek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. mar 2016 08:37
%%%-------------------------------------------------------------------
-module(bt_message_coder).
-author("maciek").

-compile(export_all).
decode_message(Bin) -> decode_message(Bin, []).

decode_message(Bin, Acc) ->
  case do_decode_message(Bin) of
    {ok, Ret, <<>>} -> {ok, Acc ++ [Ret], <<>>};
    {ok, Ret, Rest} -> decode_message(Rest, Acc ++ [Ret]); %%not optimal, should use tail recursion....
    {fail, message_too_short, Msg} -> {ok, Acc, Msg}
  end.



do_decode_message(<<19:8, "BitTorrent protocol", RSVRD:64, InfoHash:160, PeerId:160, Rest/binary>>) ->
  Ret = #{type=>handshake, rsvd=>RSVRD, info_hash=>InfoHash, peer_id=>PeerId},
  {ok, Ret, Rest};
do_decode_message(<<1:32, 0:8, Rest/binary>>) ->
  Ret = #{type=>choke},
  {ok, Ret, Rest};
do_decode_message(<<1:32, 1:8, Rest/binary>>) ->
  Ret = #{type=>unchoke},
  {ok, Ret, Rest};
do_decode_message(<<1:32, 2:8, Rest/binary>>) ->
  Ret = #{type=>interested},
  {ok, Ret, Rest};
do_decode_message(<<1:32, 3:8, Rest/binary>>) ->
  Ret = #{type=>not_interested},
  {ok, Ret, Rest};
do_decode_message(<<5:32, 4:8, PieceIdx:32, Rest/binary>>) ->
  Ret = #{type=>have, piece_idx=>PieceIdx},
  {ok, Ret, Rest};
do_decode_message(<<Len:32, 5:8, Rest/binary>> = Msg) ->
  case (byte_size(Rest)) >= (Len - 1) of
    true -> {BF, NewRest} = erlang:split_binary(Rest, Len - 1),
      Ret = #{type=>bitfield, bitfield=>BF},
      {ok, Ret, NewRest};
    false -> {fail, message_too_short, Msg}
  end;
do_decode_message(<<13:32, 6:8, Index:32, Begin:32, Length:32, Rest/binary>>) ->
  Ret = #{type=>request, index=>Index, start=>Begin, length=>Length},
  {ok, Ret, Rest};
do_decode_message(<<Len:32, 7:8, Index:32, Begin:32, Rest/binary>> = Msg) ->
  case (byte_size(Rest)) >= (Len - 9) of
    true -> {Payload, NewRest} = erlang:split_binary(Rest, Len - 9),
      Ret = #{type=>piece, index => Index, start=>Begin, payload =>Payload},
      {ok, Ret, NewRest};
    false -> {fail, message_too_short, Msg}
  end;
do_decode_message(<<0,0,0,0,0,0,0,0,Rest/binary>>) -> {ok, #{type=>keep_alive}, Rest};
do_decode_message(<<0,0,0,0,Rest/binary>>) -> {ok, #{type=>keep_alive}, Rest};
do_decode_message(<<Len:32/integer-unsigned-big, MsgId:8/integer-unsigned-big, Payload/binary>> = Msg) ->
  case (byte_size(Payload)) >= (Len - 1) of
    true -> {TrimedPayload, NewRest} = erlang:split_binary(Payload, Len - 1),
      Ret = #{type=>other, id => MsgId, payload=>TrimedPayload},
      {ok, Ret, NewRest};
    false -> {fail, message_too_short, Msg}
  end.



encode_message(#{type := interested}) -> <<1:32,2:8>>;
encode_message(#{type := request, index := Index , start := Start, length := Len}) -> <<13:32,6:8,Index:32,Start:32,Len:32>>;
encode_message(#{type := not_interested}) -> <<1:32,3:8>>.