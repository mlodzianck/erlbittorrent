-module(bencoder).
-author("maciek").
%% API
-export([bendecode/1]).
-compile(export_all).
bendecode(Bin) ->bendecode(Bin,[]).

bendecode(<<>>,Acc) -> Acc;

bendecode(<<$i:8,Rest/binary>>,Acc) ->
  {Int,NewRest} = bendecode_integer(Rest),
  bendecode(NewRest,Acc++Int);
bendecode(<<$l:8,Rest/binary>>,Acc) ->
  {List,NewRest} = bendecode_list(Rest),
  bendecode(NewRest,Acc++[List]);
bendecode(<<$d:8,Rest/binary>>,Acc) ->
  {Dict,NewRest} = bendecode_dictionary(Rest),
  bendecode(NewRest,Acc++Dict);
bendecode(<<Rest/binary>>,Acc) ->
  {Str,NewRest } = bendecode_string(Rest),
  bendecode(NewRest,Acc ++ Str).

bendecode_string(Bin) ->bendecode_string(Bin,[]).

bendecode_string(<<$::8,Rest/binary>>,LenAcc) ->
  StrLen = list_to_integer(LenAcc),
  BitLen = StrLen * 8,
  <<Str:BitLen/bitstring,NewRest/binary>>  =  Rest,
  {Str,NewRest};
bendecode_string(<<L:8,Rest/binary>>,LenAcc) ->
  bendecode_string(Rest,LenAcc++[L]).

bendecode_integer(Rest) ->bendecode_integer(Rest,[]).
bendecode_integer(<<$-:8,Rest/binary>>,[]) ->bendecode_integer(Rest,[$-]);
bendecode_integer(<<$e:8,Rest/binary>>,IntAcc) -> {list_to_integer(IntAcc),Rest};
bendecode_integer(<<Num:8,Rest/binary>>,IntAcc) -> bendecode_integer(Rest,IntAcc++[Num]).


bendecode_list(Bin) -> bendecode_list(Bin,[]).

bendecode_list(<<$e:8,Rest/binary>>,ListAcc) ->  {ListAcc,Rest};

bendecode_list(<<$i:8,Rest/binary>>,ListAcc) ->
  {Elem,NewRest} = bendecode_integer(Rest),
  bendecode_list(NewRest,ListAcc++[Elem]);
bendecode_list(<<$l:8,Rest/binary>>,ListAcc) ->
  {Elem,NewRest} = bendecode_list(Rest),
  bendecode_list(NewRest,ListAcc++[Elem]);
bendecode_list(<<$d:8,Rest/binary>>,ListAcc) ->
  {Elem,NewRest} = bendecode_dictionary(Rest),
  bendecode_list(NewRest,ListAcc++[Elem]);
bendecode_list(Bin,ListAcc) ->
  {Elem,NewRest} = bendecode_string(Bin),
  bendecode_list(NewRest,ListAcc++[Elem]).

bendecode_dictionary(Bin) -> bendecode_dictionary(Bin,#{}).
bendecode_dictionary(Bin,#{}) -> bendecode_dictionary_key(Bin,#{}).
bendecode_dictionary_key(<<$e:8,Rest/binary>>,MapAcc) -> {MapAcc,Rest};

bendecode_dictionary_key(Bin,MapAcc) ->
  {Key,RestAfterKey} = bendecode_string(Bin),
  {Value,RestAfterValue} = bendecode_dictionary_value(RestAfterKey),
  NewMapAcc = MapAcc#{Key => Value},
  bendecode_dictionary_key(RestAfterValue,NewMapAcc).

bendecode_dictionary_value(<<$i:8,Rest/binary>>) ->
  bendecode_integer(Rest);
bendecode_dictionary_value(<<$l:8,Rest/binary>>) ->
  bendecode_list(Rest);
bendecode_dictionary_value(<<$d:8,Rest/binary>>) ->
  bendecode_dictionary(Rest);
bendecode_dictionary_value(Bin) ->
  bendecode_string(Bin).

is_proplist([]) -> true;
is_proplist([{K,_}|L]) when is_atom(K) -> is_proplist(L);
is_proplist(_) -> false.

bencode(L) when is_binary(L) -> bencoder:bencode_string(L);
bencode(M) when is_map(M) -> bencode_map(M);
bencode(I) when is_integer(I) -> bencode_integer(I);
bencode(L) when is_list(L) -> bencode_list(L).

bencode_string(L) ->
  Len = integer_to_binary(byte_size(L)),

  <<Len/binary,$::8,L/bitstring>>.
bencode_integer(I) ->
  Ib = integer_to_binary(I),
  <<$i:8,Ib/binary,$e:8>>.
bencode_list(L) ->
  Payload = << <<(bencode(X))/binary>> || X <- L >>,
  <<$l:8,Payload/binary,$e:8>>.
bencode_map(M) ->
  Payload = << <<(
      begin
        << (bencode_string(Key))/binary,(bencode(maps:get(Key,M)))/binary>>
      end
  )/binary>> || Key <- maps:keys(M) >>,
  <<$d:8,Payload/binary,$e:8>>.

%% M = bencoder:bendecode(B).
%% Info = maps:get(<<"info">>,M).
%% InforStr = bencoder:bencode(Info)
%% string:to_upper(lists:flatten([[integer_to_list(N, 16) || <<N:4>> <= crypto:hash(sha, InforStr)]])).
