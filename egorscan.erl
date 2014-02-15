
-module(egorscan).

-export([ start/0, htget/1, get_alexa_list/0, list/1, full_list/0 ]).





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
%%
%% 4> egor_scan:get_alexa_list().
%% ["http://google.com","http://facebook.com",
%%  "http://youtube.com","http://yahoo.com","http://baidu.com",
%%  "http://wikipedia.org","http://qq.com","http://taobao.com",
%%  "http://live.com","http://linkedin.com",
%%  "http://sina.com.cn","http://twitter.com",
%%  "http://amazon.com","http://hao123.com",
%%  "http://google.co.in","http://blogspot.com",
%%  "http://weibo.com","http://163.com","http://wordpress.com",
%%  "http://yahoo.co.jp","http://360.cn","http://tmall.com",
%%  "http://bing.com","http://yandex.ru","http://vk.com"]
%%
%% 4> egorscan:full_list().
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




get_alexa_list() ->

    { 200, Content }     = htget("http://www.alexa.com/topsites"),
    { match, MatchList } = re:run(Content, "\<a href=\"/siteinfo/.*\"\>", [global, {capture,[0],list}]),
    [ "http://" ++ lists:reverse(string:substr(lists:reverse(string:substr(Match, 20)), 3)) || [Match] <- MatchList ].





list(L) ->

    [ { Target, htget(Target) } || Target <- L ].





full_list() ->

    egorscan:list(egorscan:get_alexa_list()).