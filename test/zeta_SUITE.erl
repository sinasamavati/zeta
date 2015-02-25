%% Copyright (c) 2014-2015 Sina Samavati <sina.samv@gmail.com>
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
-export([parse/1]).
-export([parse_file/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [parse, parse_file].

parse(_) ->
    %% -------------------------------------------------------------------------
    Fmt1 = "~h ~l ~u ~t \"~r\" ~s ~B",
    L1 = "127.0.0.1 - some_user [27/May/2014:19:28:56 +0450] \"GET /some-url HTTP/1.1\" 200 23",
    {ok, [{host, {127, 0, 0, 1}},
          {user, <<"some_user">>},
          {datetime, {{2014, 5, 27}, {19, 28, 56}}},
          {timezone, <<"+0450">>},
          {method, <<"GET">>},
          {url, <<"/some-url">>},
          {version, <<"HTTP/1.1">>},
          {status, 200},
          {content_length, 23}]} = zeta:parse(Fmt1, L1, auto),

    %% -------------------------------------------------------------------------
    Fmt2 = "~h ~l ~u ~t \"~r\" ~s ~B \"~{referer}\" \"~{user-agent}\"",
    L2 = "127.0.0.1 - - [27/May/2014:19:28:31 +0450] \"GET / HTTP/1.1\" 200 5 \"-\" \"curl/7.33.0\"",
    {ok, [{host, {127, 0, 0, 1}},
          {user, <<"-">>},
          {datetime, {{2014, 5, 27}, {19, 28, 31}}},
          {timezone, <<"+0450">>},
          {method, <<"GET">>},
          {url, <<"/">>},
          {version, <<"HTTP/1.1">>},
          {status, 200},
          {content_length, 5},
          {<<"referer">>, <<"-">>},
          {<<"user-agent">>, <<"curl/7.33.0">>}]} = zeta:parse(Fmt2, L2, auto),

    %% -------------------------------------------------------------------------
    Fmt3 = "~h ~l ~u ~t \"~r\" ~s ~B",
    L3 = "10.0.1.2 - - [22/Jun/214:08:13:29 +0200] \"GET /resource?p=2 HTTP/1.1\" 200 905",
    {ok, [{host, <<"10.0.1.2">>},
          {user, <<"-">>},
          {datetime, <<"22/Jun/214:08:13:29">>},
          {timezone, <<"+0200">>},
          {method, <<"GET">>},
          {url, <<"/resource?p=2">>},
          {version, <<"HTTP/1.1">>},
          {status, <<"200">>},
          {content_length, <<"905">>}]} = zeta:parse(Fmt3, L3, binary),

    %% -------------------------------------------------------------------------
    {ok, [{host, "127.0.0.1"},
          {user, "-"},
          {datetime, "27/May/2014:19:28:31"},
          {timezone, "+0450"},
          {method, "GET"},
          {url, "/"},
          {version, "HTTP/1.1"},
          {status, "200"},
          {content_length, "5"},
          {"referer", "-"},
          {"user-agent", "curl/7.33.0"}]} = zeta:parse(Fmt2, L2, list),

    %% -------------------------------------------------------------------------
    F1 = fun(method, _) -> tada;
            (url, _) -> "/";
            (header_name, X) -> X;
            (_, _) -> nil
         end,
    {ok, [{host, nil},
          {user, nil},
          {datetime, nil},
          {timezone, nil},
          {method, tada},
          {url, "/"},
          {version, nil},
          {status, nil},
          {content_length, nil},
          {<<"referer">>, nil},
          {<<"user-agent">>, nil}]} = zeta:parse(Fmt2, L2, F1),
    ok.

parse_file(Conf) ->
    Filename = filename:join(?config(data_dir, Conf), "access.log"),
    N = 3000,
    helper:generate_file(Filename, N),
    {ok, D, Fx} = zeta:parse_file(Filename),
    io:format("~p~n", [D]),
    Fz = fun({ok, D1, Fx1}, F, Acc) ->
                 true = lists:member({user, <<"-">>}, D1),
                 true = lists:member({version,<<"HTTP/1.1">>}, D1),
                 io:format("~p~n", [D1]),
                 F(Fx1(), F, [D1|Acc]);
            (eof, _, Acc) ->
                 N = length(Acc)
         end,
    Fz(Fx(), Fz, [D]),
    ok.
