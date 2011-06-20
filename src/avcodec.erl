-module(avcodec).
-include_lib("erlmedia/include/video_frame.hrl").

-define(D(X), io:format("~p~n", [X])).

-export([test/2, loop_frames/0]).
-export([start/0, stop/0]).

start() ->
  application:start(avcodec).

stop() ->
  application:stop(avcodec).

test(Host, Name) ->
  start(),
  avcodec_sup:start_yuv_stream(Host, Name, self()),
  timer:send_after(5000, stop),
  loop_frames().


loop_frames() ->
  receive
    {yuv, _YuvStream, YUV} ->
      ?D({"YUV output:",size(YUV)}),
      ?MODULE:loop_frames();
    stop ->
      ?D(stop);
    Else ->
      ?D(Else),
      ?MODULE:loop_frames()
  after
    5000 -> ?D(timeout)
  end.
      