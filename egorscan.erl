
-module(egorscan).

-export([ start/0, htget/1, list/1 ]).





%% @doc Quick scanner for Egor.
%% Usage:
%%
%% Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:8:8] [async-threads:0] [kernel-poll:false]
%% Eshell V5.9.1  (abort with ^G)
%% 
%% 1> c("egorscan.erl").
%% {ok,egorscan}
%%
%% 2> egorscan:start().
%% ok
%%
%% 3> egorscan:list([ "http://google.com", "http://yahoo.com" ]).
%% [{"http://google.com",
%%  {200,
%%   "<!doctype html><html itemscope=\"\" itemtype=\"http://schema.org...






start() ->

    inets:start(),
    crypto:start(),
    ssl:start().





htget(Thing) ->

    case httpc:request(Thing) of
        
        {ok,{{_,RCode,_},_,Ret}} -> {RCode, Ret};
        Other                    -> {error, Other}

    end.





list(L) ->

    [ { Target, htget(Target) } || Target <- L ].