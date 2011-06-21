-module(av_decoder).

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
  CodecName = h264,
  DecoderConfig = proplists:get_value(decoder_config, Options),
  <<"ok">> = port_control(Decoder, ?CMD_INIT, DecoderConfig),
  {ok, Decoder}.


async_decode(Decoder, Frame) when is_binary(Frame) ->
  port_command(Decoder, Frame).

decode(Decoder, Frame) ->
  async_decode(Decoder, Frame),
  receive
    {yuv, Decoder, YUV} -> {ok, YUV};
    {yuv, Decoder} -> ok
  after
    1000 -> erlang:error(av_timeout)
  end.


info(Decoder) ->
  <<Width:32, Height:32, TotalTime:32>> = port_control(Decoder, ?CMD_INFO, <<>>),
  [{width,Width},{height,Height},{time,TotalTime}].
  