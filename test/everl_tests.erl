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

-define(NUM_CLIENTS, 100).
-define(NUM_REQ, 200).


% Start up an echo server on a random port and NUM_CLIENTS clients.
% Each client will make NUM_REQ requests.
client_server_test() ->
    ok = start(?NUM_CLIENTS).

start(N) ->
    Port = crypto:rand_uniform(1024, 16#FFFF),

    Self = self(),
    spawn_link(fun() -> server(Port, Self) end),

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
        1000 ->
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
    {ok, Socket} =  gen_tcp:listen(Port, [
            binary,
            {packet, 0},
            {active, false},
            {backlog, 128}
        ]),

    {ok, FD} = inet:getfd(Socket),

    error_logger:info_report([
            {server, listening},
            {port, Port}
        ]),

    {ok, Watcher} = everl:create(FD, ?EV_READ),
    Pid ! ok,
    listen(Socket, Watcher).

listen(Socket, Watcher) ->
    {ok, FD} = inet:getfd(Socket),
    ok = everl:arm(Watcher),
    receive
        {everl_watcher, FD, ?EV_READ} ->
            {ok, Socket1} = gen_tcp:accept(Socket),
            {ok, {Address, Port}} = inet:peername(Socket1),

            error_logger:info_report([
                    {server, connect},
                    {address, inet_parse:ntoa(Address)},
                    {port, Port}
                ]),

            spawn(fun() -> accept(Socket1) end),
            listen(Socket, Watcher);

        Error ->
            Error
    end.

accept(Socket) ->
    {ok, FD} = inet:getfd(Socket),
    {ok, Watcher} = everl:create(FD, ?EV_RDWR),
    accept(Socket, Watcher, <<>>).
accept(Socket, Watcher, Buf) ->
    {ok, FD} = inet:getfd(Socket),
    ok = everl:arm(Watcher),
    receive
        {everl_watcher, FD, ?EV_RDWR} ->
            Res = gen_tcp:recv(Socket, 0),
            case Res of
                {ok, Buf1} ->
                    ok = gen_tcp:send(Socket, list_to_binary([Buf, Buf1])),
                    accept(Socket, Watcher, <<>>);
                {error, closed} ->
                    ok
            end;

        {everl_watcher, FD, ?EV_WRITE} ->
            case Buf of
                <<>> -> ok;
                _ ->
                    gen_tcp:send(Socket, Buf)
            end,
            accept(Socket, Watcher, <<>>);

        {everl_watcher, FD, ?EV_READ} ->
            Res = gen_tcp:recv(Socket, 0),
            case Res of
                {ok, Buf1} ->
                    accept(Socket, Watcher, list_to_binary([Buf, Buf1]));
                {error, closed} ->
                    ok
            end;

        Error ->
            error_logger:error_report([{server, Error}])
    end.
