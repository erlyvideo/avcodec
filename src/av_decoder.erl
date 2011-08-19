-module(av_decoder).
-include("log.hrl").
-export([init/2, decode/2, async_decode/2, info/1]).

-define(CMD_INIT, 1).
-define(CMD_INFO, 2).

init(CodecName, Options) ->
  case erl_ddll:load_driver(code:lib_dir(avcodec,ebin), av_decoder_drv) of
  	ok -> ok;
  	{error, already_loaded} -> ok;
  	{error, permanent} -> ok;
  	{error, Error} -> error_logger:error_msg("Can't load AV Decoder: ~p~n", [erl_ddll:format_error(Error)])
  end,
  Decoder = open_port({spawn, av_decoder_drv}, [binary]),
  DecoderConfig = proplists:get_value(decoder_config, Options),
%  CodecBin = list_to_binary(lists:flatten(io_lib:format("~.4s", [CodecName]))),
  CodecBin = atom_to_binary(CodecName,latin1),
  SizeCodecBin = size(CodecBin),
  ?D(SizeCodecBin),
  <<"ok">> = port_control(Decoder, ?CMD_INIT, <<CodecBin:SizeCodecBin/binary, DecoderConfig/binary>>),
  {ok, Decoder}.


async_decode({ok,Decoder}, Frame) when is_binary(Frame) ->
  port_command(Decoder, Frame);
async_decode(Decoder, Frame) when is_binary(Frame) ->
  port_command(Decoder, Frame).

decode(Decoder, Frame) ->
  async_decode(Decoder, Frame),
  receive
    {yuv, _Decoder, YUV} -> YUV;
    {yuv, _Decoder} -> ok
  after
    1000 -> erlang:error(av_timeout)
  end.

info(undefined) ->
  ok;
info({ok,Decoder}) ->
 info(Decoder);
info(Decoder) ->
  <<Width:32, Height:32, TotalTime:32>> = port_control(Decoder, ?CMD_INFO, <<>>),
  [{width,Width},{height,Height},{time,TotalTime}].
  
