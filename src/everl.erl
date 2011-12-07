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
-module(everl).
-include("everl.hrl").

-export([
        create/2,
        arm/1,
        disarm/1,

        flag/1
    ]).
-export([init/0]).

-on_load(on_load/0).


init() ->
    on_load().

on_load() ->
    erlang:load_nif(progname(), []).


watcher_create(_,_) ->
    erlang:error(not_implemented).

watcher_arm(_) ->
    erlang:error(not_implemented).

watcher_disarm(_) ->
    erlang:error(not_implemented).

create(FD, Flags) when is_atom(Flags); is_list(Flags) ->
    Flag = flag(Flags),
    create(FD, Flag);
create(FD, Flags) when is_integer(Flags) ->
    watcher_create(FD, Flags).

arm(Handle) ->
    watcher_arm(Handle).

disarm(Handle) ->
    watcher_disarm(Handle).

flag(read) -> ?EV_READ;
flag(write) -> ?EV_WRITE;
flag(?EV_READ) -> read;
flag(?EV_WRITE) -> write;

flag(Flags) when is_list(Flags) ->
    lists:foldl(fun(X,F) -> X bor F end, 0, Flags).

progname() ->
    filename:join([
        filename:dirname(code:which(?MODULE)),
        "..",
        "priv",
        ?MODULE
    ]).
