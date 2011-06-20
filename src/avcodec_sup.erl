
-module(avcodec_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([start_yuv_stream/3]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_yuv_stream(Host, Name, Consumer) ->
  supervisor:start_child(yuv_stream_sup, [Host, Name, Consumer]).
  

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([yuv_stream]) ->
  {ok, {{simple_one_for_one, 1, 100}, [
    {   undefined,                               % Id       = internal id
    {yuv_stream,start_link,[]},                  % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
    }
  ]}};


init([]) ->
  Supervisors = [
  {   yuv_stream_sup,
      {supervisor,start_link,[{local, yuv_stream_sup}, ?MODULE, [yuv_stream]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
  }
  ],
    {ok, { {one_for_one, 5, 10}, Supervisors} }.

