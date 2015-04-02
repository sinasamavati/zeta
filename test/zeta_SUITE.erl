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
    {ok, D1} = zeta:parse(Fmt1, L1, auto),
    [{content_length, 23},
     {datetime, {{2014, 5, 27}, {19, 28, 56}}},
     {host, {127, 0, 0, 1}},
     {method, <<"GET">>},
     {status, 200},
     {timezone, <<"+0450">>},
     {url, <<"/some-url">>},
     {user, <<"some_user">>},
     {version, <<"HTTP/1.1">>}] = lists:sort(D1),

    %% -------------------------------------------------------------------------
    Fmt2 = "~h ~l ~u ~t \"~r\" ~s ~B \"~{referer}\" \"~{user-agent}\"",
    L2 = "127.0.0.1 - - [27/May/2014:19:28:31 +0450] \"GET / HTTP/1.1\" 200 5 \"-\" \"curl/7.33.0\"",
    {ok, D2} = zeta:parse(Fmt2, L2, auto),
    [{content_length, 5},
     {datetime, {{2014, 5, 27}, {19, 28, 31}}},
     {host, {127, 0, 0, 1}},
     {method, <<"GET">>},
     {status, 200},
     {timezone, <<"+0450">>},
     {url, <<"/">>},
     {user, <<"-">>},
     {version, <<"HTTP/1.1">>},
     {<<"referer">>, <<"-">>},
     {<<"user-agent">>, <<"curl/7.33.0">>}] = lists:sort(D2),


    %% -------------------------------------------------------------------------
    Fmt3 = "~h ~l ~u ~t \"~r\" ~s ~B",
    L3 = "10.0.1.2 - - [22/Jun/214:08:13:29 +0200] \"GET /resource?p=2 HTTP/1.1\" 200 905",
    {ok, D3} = zeta:parse(Fmt3, L3, binary),
    [{content_length, <<"905">>},
     {datetime, <<"22/Jun/214:08:13:29">>},
     {host, <<"10.0.1.2">>},
     {method, <<"GET">>},
     {status, <<"200">>},
     {timezone, <<"+0200">>},
     {url, <<"/resource?p=2">>},
     {user, <<"-">>},
     {version, <<"HTTP/1.1">>}] = lists:sort(D3),

    %% -------------------------------------------------------------------------
    {ok, D4} = zeta:parse(Fmt2, L2, list),
    [{content_length, "5"},
     {datetime, "27/May/2014:19:28:31"},
     {host, "127.0.0.1"},
     {method, "GET"},
     {status, "200"},
     {timezone, "+0450"},
     {url, "/"},
     {user, "-"},
     {version, "HTTP/1.1"},
     {"referer", "-"},
     {"user-agent", "curl/7.33.0"}] = lists:sort(D4),

    %% -------------------------------------------------------------------------
    F1 = fun(method, _) -> tada;
            (url, _) -> "/";
            (header_name, X) -> X;
            (_, _) -> nil
         end,
    {ok, D5} = zeta:parse(Fmt2, L2, F1),
    [{content_length, nil},
     {datetime, nil},
     {host, nil},
     {method, tada},
     {status, nil},
     {timezone, nil},
     {url, "/"},
     {user, nil},
     {version, nil},
     {<<"referer">>, nil},
     {<<"user-agent">>, nil}] = lists:sort(D5),
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
