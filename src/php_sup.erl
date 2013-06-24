%%%-------------------------------------------------------------------
%%% File    : php_sup.erl
%%% Author  : Andy Skelton <andy@automattic.com>
%%% Purpose : Supervisor for php_app
%%% Created : 15 Jan 2009 by Andy Skelton <andy@automattic.com>
%%% License : GPLv2
%%%-------------------------------------------------------------------
-module(php_sup).

-behaviour(supervisor).

-import(php, [get_opt/3]).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
start_link() ->
	start_link([]).
start_link(Args) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init(Args) ->
	Procs = case get_opt(procs, Args, default) of
		ProcsArg when is_integer(ProcsArg) ->
			ProcsArg;
		_ ->
			erlang:system_info(logical_processors)
	end,
	Opts = get_opt(opts, Args, []),
	Interface = {php, {php, start_link, []},
		permanent, 2000, worker, [php]},
	Decoder = {php_decode, {php_decode, start_link, []},
		permanent, 2000, worker, [php_decode]},
	Servers = [{Proc, {php_eval, start_link, [Opts, Proc]},
		permanent, 2000, worker, [php_eval]}
		|| P <- lists:seq(1, Procs), Proc <- [get_proc_name(phpeval, P)]],
	{ok, {{one_for_one, 1, 1}, [Interface, Decoder] ++ Servers}}.

%%====================================================================
%% Internal functions
%%====================================================================

get_proc_name(Name, Num) when is_integer(Num) ->
	list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Num)).
