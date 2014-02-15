egorscan
========

A quick web scanner I wrote real quick-like for Egor Homakov.

Kind of a piece of crap.




Usage
-----

```erlang
Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:8:8] [async-threads:0] [kernel-poll:false]
Eshell V5.9.1  (abort with ^G)

1> c("egorscan.erl").
{ok,egorscan}

2> egorscan:start().
ok

3> egorscan:list([ "http://google.com", "http://yahoo.com" ]).
[{"http://google.com",
 {200,
  "<!doctype html><html itemscope=\"\" itemtype=\"http://schema.org..."

4> egor_scan:get_alexa_list().
["http://google.com","http://facebook.com",
 "http://youtube.com","http://yahoo.com","http://baidu.com",
 "http://wikipedia.org","http://qq.com","http://taobao.com",
 "http://live.com","http://linkedin.com",
 "http://sina.com.cn","http://twitter.com",
 "http://amazon.com","http://hao123.com",
 "http://google.co.in","http://blogspot.com",
 "http://weibo.com","http://163.com","http://wordpress.com",
 "http://yahoo.co.jp","http://360.cn","http://tmall.com",
 "http://bing.com","http://yandex.ru","http://vk.com"]

5> egorscan:full_list().
[{"http://google.com",
 {200,
  "<!doctype html><html itemscope=\"\" itemtype=\"http://schema.org..."
```
