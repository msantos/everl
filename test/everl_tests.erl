%% Copyright (c) 2011, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%% 
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%% 
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(everl_tests).

-compile(export_all).

-include_lib("everl.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(EV_RDWR, ?EV_READ bor ?EV_WRITE).

-define(NUM_CLIENTS, 50).
-define(NUM_REQ, 200).


% Start up an echo server on a random port and NUM_CLIENTS clients.
% Each client will make NUM_REQ requests.
client_server_test() ->
    ok = start(?NUM_CLIENTS).

start(N) ->
    Port = crypto:rand_uniform(1024, 16#FFFF),

    Self = self(),
    spawn(fun() -> server(Port, Self) end),

    receive ok -> ok end,

    spawn(fun() -> client(Port, N, Self) end),

    wait(N).

wait(0) ->
    ok;
wait(N) ->
    receive
        ok ->
            error_logger:info_report([{clients, N}]),
            wait(N-1);
        Err ->
            Err
    after
        10000 ->
            {failed, N}
    end.

client(_Port, 0, _Pid) ->
    ok;
client(Port, N, Pid) ->
    spawn(fun() -> client_1(Port, Pid) end),
    client(Port, N-1, Pid).

client_1(Port, Pid) ->
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [
            binary,
            {packet,0},
            {active, false}
        ]),

    Data = list_to_binary(lists:seq(97,126)),

    ok = client_2(Socket, Data, ?NUM_CLIENTS, Pid),

    Pid ! ok.

client_2(_Socket, _Data, 0, _Pid) ->
    ok;
client_2(Socket, Data, N, Pid) ->
    ok = gen_tcp:send(Socket, Data),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            client_2(Socket, Data, N-1, Pid);
        Error ->
            {client_error, Error}
    end.


server(Port, Pid) ->
    {ok, Socket} = procket:open(Port, [
            {protocol, tcp},
            {type, stream},
            {family, inet}
        ]),
    ok = procket:listen(Socket),
    io:format("listening on ~p~n", [Port]),
    {ok, Watcher} = everl:create(Socket, ?EV_READ),
    Pid ! ok,
    listen(Socket, Watcher).

listen(Socket, Watcher) ->
    ok = everl:arm(Watcher),
    receive
        {everl_watcher, Socket, ?EV_READ} ->
            {ok, Socket1, <<Family:16/native, Port:16,
                IP1,IP2,IP3,IP4, _/binary>>} = procket:accept(Socket, 16),

            io:format("Connect from op=~p, family=~p, ip=~p, port=~p~n",
                [?EV_READ, Family, {IP1,IP2,IP3,IP4}, Port]),

            spawn(fun() -> accept(Socket1) end),
            listen(Socket, Watcher);

        Error ->
            Error
    end.

accept(Socket) ->
    {ok, Watcher} = everl:create(Socket, ?EV_RDWR),
    accept(Socket, Watcher, <<>>).
accept(Socket, Watcher, Buf) ->
    ok = everl:arm(Watcher),
    receive
        {everl_watcher, Socket, ?EV_RDWR} ->
            Res = procket:read(Socket, 1024),
            case Res of
                {ok, <<>>} ->
                    procket:close(Socket);
                {ok, Buf1} ->
                    Data = list_to_binary([Buf, Buf1]),
                    ok = procket:write(Socket, Data),
                    accept(Socket, Watcher, <<>>);
                {error, eagain} ->
                    accept(Socket, Watcher, <<>>)
            end;

        {everl_watcher, Socket, ?EV_WRITE} ->
            case Buf of
                <<>> -> ok;
                _ ->
                    procket:write(Socket, Buf)
            end,
            accept(Socket, Watcher, <<>>);

        {everl_watcher, Socket, ?EV_READ} ->
            Res = procket:read(Socket, 1024),
            case Res of
                {ok, <<>>} ->
                    procket:close(Socket);
                {ok, Buf1} ->
                    Data = list_to_binary([Buf, Buf1]),
                    accept(Socket, Watcher, Data);
                {error, eagain} ->
                    accept(Socket, Watcher, Buf)
            end;

        Error ->
            error_logger:error_report([{server, Error}])
    end.
