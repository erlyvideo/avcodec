-module(yuv_stream).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("erlmedia/include/video_frame.hrl").
-include("log.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(yuv, {
  host,
  name,
  media,
  decoder,
  consumer
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Host, Name, Consumer) ->
  gen_server:start_link(?MODULE, [Host, Name, Consumer], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Host, Name, Consumer]) ->
  erlang:monitor(process, Consumer),
  {ok, Media} = media_provider:play(Host, Name, [{stream_id, 1}]),
  {ok, #yuv{host = Host, name = Name, consumer = Consumer, media = Media}}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(#video_frame{codec = h264, flavor = config, body = Config}, #yuv{decoder = undefined} = State) ->
  {ok, Decoder1} = av_decoder:init(h264, [{decoder_config, Config}]),
  {noreply, State#yuv{decoder = Decoder1}};

handle_info(#video_frame{codec = h264, flavor = config}, #yuv{} = State) ->
  {noreply, State};

handle_info(#video_frame{codec = h264}, #yuv{decoder = undefined} = State) ->
  {noreply, State};

handle_info(#video_frame{codec = h264, body = H264}, #yuv{decoder = Decoder} = State) ->
  av_decoder:async_decode(Decoder, H264),
  {noreply, State};

handle_info({yuv, Decoder, YUV}, #yuv{decoder = Decoder, consumer = Consumer} = State) ->
  Consumer ! {yuv, self(), YUV},
  {noreply, State};

handle_info({yuv, Decoder}, #yuv{decoder = Decoder} = State) ->
  {noreply, State};

handle_info({'DOWN', _Ref, process, _Consumer, _Reason}, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

