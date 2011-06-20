-module(av_codec).
-include_lib("erlmedia/include/video_frame.hrl").

-define(D(X), io:format("~p~n", [X])).

-export([test/2, loop_frames/1]).

test(Host, Name) ->
  {ok, _Media} = media_provider:play(Host, Name, [{stream_id,1}]),
  timer:send_after(5000, stop),
  loop_frames(undefined).


loop_frames(Decoder) ->
  receive
    #video_frame{codec = h264, flavor = config, body = Config} when Decoder == undefined ->
      {ok, Decoder1} = av_decoder:init(h264, [{decoder_config, Config}]),
      ?MODULE:loop_frames(Decoder1);
    #video_frame{codec = h264, flavor = config} ->
      ?MODULE:loop_frames(Decoder);
    #video_frame{codec = h264, body = H264} when Decoder == undefined ->
      ?MODULE:loop_frames(Decoder);
    #video_frame{codec = h264, body = H264} ->
      case av_decoder:decode(Decoder, H264) of
        ok -> ?D("empty output");
        {ok, YUV} -> ?D({"YUV output:",size(YUV)})
      end,
      ?MODULE:loop_frames(Decoder);
    stop ->
      ?D(stop);
    Else ->
      ?D(Else),
      ?MODULE:loop_frames(Decoder)
  after
    5000 -> ?D(timeout)
  end.
      