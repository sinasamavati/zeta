%% Copyright (c) 2014 Sina Samavati <sina.samv@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(zeta_SUITE).

-export([all/0]).
-export([format/1]).

all() ->
    [format].

format(_) ->
    Fmt1 = "~h ~l ~u ~t \"~r\" ~s ~B",
    L1 = "127.0.0.1 - some_user [27/May/2014:19:28:56 +0450] \"GET /some-url HTTP/1.1\" 200 23",
    {ok, [{host, <<"127.0.0.1">>},
          {user, <<"some_user">>},
          {datetime, {{2014, 5, 27}, {19, 28, 56}}},
          {timezone, <<"+0450">>},
          {method, <<"GET">>},
          {url, <<"/some-url">>},
          {version, <<"HTTP/1.1">>},
          {status, 200},
          {content_length, 23}]} = zeta:format(Fmt1, L1),

    Fmt2 = "~h ~l ~u ~t \"~r\" ~s ~B \"~{referer}\" \"~{user-agent}\"",
    L2 = "127.0.0.1 - - [27/May/2014:19:28:31 +0450] \"GET / HTTP/1.1\" 200 5 \"-\" \"curl/7.33.0\"",
    {ok, [{host, <<"127.0.0.1">>},
          {user, <<"-">>},
          {datetime, {{2014, 5, 27}, {19, 28, 31}}},
          {timezone, <<"+0450">>},
          {method, <<"GET">>},
          {url, <<"/">>},
          {version, <<"HTTP/1.1">>},
          {status, 200},
          {content_length, 5},
          {<<"referer">>, <<"-">>},
          {<<"user-agent">>, <<"curl/7.33.0">>}]} = zeta:format(Fmt2, L2).
