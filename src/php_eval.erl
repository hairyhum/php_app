%%%-------------------------------------------------------------------
%%% File    : php_eval.erl
%%% Author  : Andy Skelton <andy@automattic.com>
%%% Purpose : A server for running PHP code.
%%% Created : 15 Jan 2009 by Andy Skelton <andy@automattic.com>
%%% License : GPLv2
%%%-------------------------------------------------------------------
-module(php_eval).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(php_util,[get_opt/3]).

%% port handler in PHP
-define(PHPLOOP, "ini_set('track_errors',true);do{ob_start();
  @$len = array_pop(unpack('N',fread(STDIN,4)));@trigger_error('');
  @$_C_=fread(STDIN,$len);@trigger_error('');
  while(strlen($_C_) < $len){
    $_C_.=fread(STDIN, $len - strlen($_C_));
  } 
  $len = null;
  if(eval('return true;if(0){'.$_C_.'}')){$_R_=serialize(eval($_C_));}else{$_R_='E;';}$_R_.=serialize($php_errormsg);$_R_.=serialize(ob_get_clean());fwrite(STDOUT,pack('N',strlen($_R_)).$_R_);}while(!empty($_C_));exit;").

-record(state, {
	  port,
	  opts,
	  pid,
	  require = []
	 }).

%%====================================================================
%% API
%%====================================================================
start_link(Args, Proc) ->
    gen_server:start_link({local, Proc}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    process_flag(trap_exit,true),
    State = start_php(#state{opts=Opts}),
    {ok, State}.

handle_call(get_state, _, State) ->
    {reply, State, State};
handle_call({eval, Code, Timeout, MaxMem}, From, #state{opts=Opts}=OrigState) ->
    State = guarantee_php(OrigState),
    Exec  = exec_php(State#state.port, Code, Timeout),
    Limit = if
		MaxMem =:= undefined -> get_opt(maxmem, Opts, infinity);
		true -> MaxMem
	    end,
    Restart = case {element(1, Exec), Limit} of
		  {exit, _} ->
		      true;
		  {ok, infinity} ->
		      false;
		  {ok, _} ->
		      case get_mem(State#state.pid) of
			  Mem when not is_integer(Mem); Mem > Limit ->
			      true;
			  _ ->
			      false
		      end
	      end,
    Reply = case {element(1, Exec), Restart} of
		{exit, _} ->
		    Exec;
		{ok, true} ->
		    erlang:append_element(Exec, break);
		{ok, false} ->
		    erlang:append_element(Exec, continue)
	    end,
    gen_server:reply(From, Reply),
    NewState = case Restart of
		   true ->
		       restart_php(State);
		   false ->
		       State
	       end,
    {noreply, NewState};
handle_call(get_mem, _, OrigState) ->
    State = guarantee_php(OrigState),
    Mem = get_mem(State#state.pid),
    {reply, Mem, State};
handle_call(restart_php, _, State) ->
    {reply, ok, restart_php(State)};
handle_call({require, Require}, _From, OrigState) ->
    NewState = restart_php(OrigState#state{require=Require}),
    case NewState#state.pid of
	Pid when is_integer(Pid) ->
	    {reply, ok, NewState};
	_ ->
	    {reply, error, restart_php(OrigState)}
    end;
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', OldPort, _Why}, #state{port=OldPort}=State) ->
    {noreply, restart_php(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    stop_php(State#state.port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

start_php(#state{opts=Opts, require=Require}=State) ->
    Php  = get_opt(php,  Opts, "php"),
    Init = get_opt(init, Opts, []),
    Dir  = get_opt(dir,  Opts, []),
    Envs = get_opt(envs, Opts, []),
    Command = Php ++ " -r '" ++ escape(Init) ++ ";" ++ escape(?PHPLOOP) ++ "'",
    PortOpts = [{packet,4},exit_status] ++
	case Dir of
	    [] -> [];
	    _  -> [{cd, Dir}]
	end ++
	case Envs of
	    [] -> [];
	    _  -> [{env, Envs}]
	end,
    Port = open_port({spawn, Command}, PortOpts),
    require(Port, Require),
    Pid  = get_pid(Port),
    State#state{port=Port,pid=Pid}.

require(_, []) ->
    ok;
require(Port, [{_, {code, Code}} | Require]) ->
    exec_php(Port, Code, 5000),
    require(Port, Require).

stop_php(Port) ->
    case erlang:port_info(Port) of
	undefined ->
	    ok;
	_ ->
	    exec_php(Port, "exit(0);", 0),
	    ok
    end.

restart_php(State) ->
    stop_php(State#state.port),
    start_php(State).

guarantee_php(State) ->
    case get_pid(State#state.port) of
	Pid when is_integer(Pid) -> State#state{pid=Pid};
	_ -> guarantee_php(restart_php(State))
    end.

get_pid(Port) ->
    case exec_php(Port, "return getmypid();", 5000) of
	{_,_,Pid,_} -> Pid;
	_ -> undefined
    end.

get_mem(Pid) ->
    case is_integer(Pid) of
	true ->
	    Mem = string:strip(string:strip(os:cmd("ps h -o rss "++integer_to_list(Pid))),right,10),
	    case length(Mem) of
		0 -> undefined;
		_ -> list_to_integer(Mem)
	    end;
	false ->
	    undefined
    end.

%% @spec (list()) -> list()
%% @doc Replaces ' with '\'' for use in bash command arguments. Since
%%      it is impossible to escape a single-quote in a single-quoted
%%      argument, we must break out of the quotes before escaping it.
escape(Str) ->
    escape(Str, []).

escape([], Acc) ->
    lists:reverse(Acc);
escape([H|T], Acc) ->
    case H =:= 39 of  % 39 is single-quote
	true ->
	    escape(T, [39,39,92,39|Acc]);  % 92 is backslash
	false ->
	    escape(T, [H|Acc])
    end.

exec_php(Port, Code, Timeout) ->
    Port ! {self(), {command, unicode:characters_to_binary(Code)}},
    receive
	{Port, {exit_status, Status}} -> {exit, Status};
	{Port, {data, Data}}          -> 
           {Return, Rest} = php_util:unserialize(Data),
					 {Error, Rest2} = php_util:unserialize(Rest),
					 {Output, _End} = php_util:unserialize(Rest2),
					 case Return of
					     error ->
						 {parse_error, Error};
					     _ ->
						 {ok, Output, Return, Error}
					 end
    after
	Timeout ->
	    exit(Port, kill),
	    {exit, timeout}
    end.
