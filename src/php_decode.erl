-module(php_decode).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-export([json_decoder/0, decode_data/1]).

-include("php_app.hrl").

-record(state, {
	decoder :: fun()
}).

decode_data(Data) ->
	gen_server:call(?MODULE, {decode_data, Data}).

json_decoder() ->
	jsonx:decoder([{php_result, record_info(fields, php_result)}]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Opts) ->
	State = #state{decoder = json_decoder()},
	{ok, State}.

handle_call({decode_data, Data}, _, #state{decoder = Decoder} = State) ->
	{reply, Decoder(Data), State}.

handle_cast(_, State) ->
	{noreply, State}.
handle_info(_, State) ->
	{noreply, State}.
terminate(_Reason, _State) ->
	ok.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
