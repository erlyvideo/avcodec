-module(av_decoder).

-export([init/2, decode/2]).

-define(CMD_INIT, 1).

init(CodecName, Options) ->
  case erl_ddll:load_driver(code:lib_dir(avcodec,ebin), av_decoder_drv) of
  	ok -> ok;
  	{error, already_loaded} -> ok;
  	{error, Error} -> error_logger:error_msg("Can't load AV Decoder: ~p~n", [erl_ddll:format_error(Error)])
  end,
  Decoder = open_port({spawn, av_decoder_drv}, [binary]),
  CodecName = h264,
  DecoderConfig = proplists:get_value(decoder_config, Options),
  <<"ok">> = port_control(Decoder, ?CMD_INIT, DecoderConfig),
  {ok, Decoder}.
  

decode(Decoder, Frame) ->
  port_command(Decoder, Frame),
  receive
    {yuv, Decoder, YUV} -> {ok, YUV};
    {yuv, Decoder} -> ok
  after
    1000 -> erlang:error(av_timeout)
  end.
