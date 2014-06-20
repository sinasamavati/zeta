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

-module(zeta).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([format/2]).
-export([parse_file/1]).
-export([parse_file/2]).

%% -----------------------------------------------------------------------------
%% types
%% -----------------------------------------------------------------------------
-type fmt() :: string() | binary().
-type data() :: string() | binary().
-type log_data() :: [log_data()]
                  | {host, binary()}
                  | {user, binary()}
                  | {datetime, calendar:datetime()}
                  | {timezone, binary()}
                  | {method, binary()}
                  | {url, binary()}
                  | {version, binary()}
                  | {status, non_neg_integer()}
                  | {content_length, integer() | binary()}
                  | {binary(), binary()}.
-type filename() :: file:name_all().

-export_type([fmt/0]).
-export_type([data/0]).
-export_type([log_data/0]).
-export_type([filename/0]).

%% -----------------------------------------------------------------------------
%% default log format
%% -----------------------------------------------------------------------------
-define(LOG_FMT, <<"~h ~l ~u ~t \"~r\" ~s ~B \"~{referer}\" \"~{user-agent}\"">>).

-spec format(fmt(), data()) -> {ok, log_data()}.
format(Fmt, Data) ->
    format(to_bin(Fmt), to_bin(Data), []).

-spec parse_file(filename()) -> {ok, [log_data()]} | {error, any()}.
parse_file(Filename) ->
    parse_file(?LOG_FMT, Filename).

-spec parse_file(fmt(), filename()) -> {ok, [log_data()]} | {error, any()}.
parse_file(Fmt, Filename) ->
    case file:read_file(Filename) of
        {ok, <<>>} ->
            {ok, []};
        {ok, Content} ->
            Lines = re:split(Content, <<"\n">>, [trim]),
            Format = fun(E) ->
                             {ok, Data} = format(Fmt, E),
                             Data
                     end,
            {ok, [Format(L) || L <- Lines]};
        Else ->
            Else
    end.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
-spec to_bin(binary() | string()) -> binary().
to_bin(X) when is_binary(X) -> X;
to_bin(X) when is_list(X) -> list_to_binary(X).

-spec format(binary(), binary(), [term()]) -> {ok, log_data()}.
format(<<>>, <<>>, Acc) ->
    {ok, Acc};
format(<<$~, $h, Fmt/binary>>, Data, Acc) ->
    {V, Fmt1, Data1} = get_value(Fmt, Data),
    format(Fmt1, Data1, Acc ++ [{host, V}]);
format(<<$~, $l, Fmt/binary>>, <<$-, Data/binary>>, Acc) ->
    format(Fmt, Data, Acc);
format(<<$~, $u, Fmt/binary>>, Data, Acc) ->
    {V, Fmt1, Data1} = get_value(Fmt, Data),
    format(Fmt1, Data1, Acc ++ [{user, V}]);
format(<<$~, $t, Fmt/binary>>, Data, Acc) ->
    {V, Fmt1, Data1} = get_datetime(Fmt, Data),
    {Datetime, Timezone} = parse_datetime(V),
    format(Fmt1, Data1, Acc ++ [{datetime, Datetime}, {timezone, Timezone}]);
format(<<$~, $r, Fmt/binary>>, Data, Acc) ->
    {V, Fmt1, Data1} = get_value(Fmt, Data),
    [Method, URL, Version] = re:split(V, <<" ">>),
    Acc1 = Acc ++ [{method, Method}, {url, URL}, {version, Version}],
    format(Fmt1, Data1, Acc1);
format(<<$~, $s, Fmt/binary>>, Data, Acc) ->
    {V, Fmt1, Data1} = get_value(Fmt, Data),
    format(Fmt1, Data1, Acc ++ [{status, bin_to_int(V)}]);
format(<<$~, $b, Fmt/binary>>, Data, Acc) ->
    {V, Fmt1, Data1} = get_value(Fmt, Data),
    format(Fmt1, Data1, Acc ++ [{content_length, V}]);
format(<<$~, $B, Fmt/binary>>, Data, Acc) ->
    {V, Fmt1, Data1} = get_value(Fmt, Data),
    format(Fmt1, Data1, Acc ++ [{content_length, bin_to_int(V)}]);
format(<<$~, ${, Fmt/binary>>, Data, Acc) ->
    {Name, Fmt1} = get_name(Fmt, <<>>),
    {V, Fmt2, Data1} = get_value(Fmt1, Data),
    format(Fmt2, Data1, Acc ++ [{Name, V}]);
format(<<H, Fmt/binary>>, <<H, Rest/binary>>, Acc) ->
    format(Fmt, Rest, Acc).

-spec get_value(binary(), binary()) -> {binary(), binary(), binary()}.
get_value(Fmt, Data) ->
    get_value(Fmt, Data, <<>>).

-spec get_value(binary(), binary(), binary()) -> {binary(), binary(), binary()}.
get_value(<<>>, Data, Acc) ->
    {<<Acc/binary, Data/binary>>, <<>>, <<>>};
get_value(<<H, Fmt/binary>>, <<H, Data/binary>>, Acc) ->
    {Acc, Fmt, Data};
get_value(Fmt, <<H, Data/binary>>, Acc) ->
    get_value(Fmt, Data, <<Acc/binary, H>>).

-spec get_name(binary(), binary()) -> {binary(), binary()}.
get_name(<<$}, Fmt/binary>>, Acc) ->
    {Acc, Fmt};
get_name(<<H, Fmt/binary>>, Acc) ->
    get_name(Fmt, <<Acc/binary, H>>).

-spec get_datetime(binary(), binary()) -> {binary(), binary(), binary()}.
get_datetime(Fmt, Data) ->
    get_datetime(Fmt, Data, <<>>).

-spec get_datetime(binary(), binary(), binary()) ->
                          {binary(), binary(), binary()}.
get_datetime(<<H, Fmt/binary>>, <<$], H, Data/binary>>, Acc) ->
    {Acc, Fmt, Data};
get_datetime(Fmt, <<$[, Data/binary>>, Acc) ->
    get_datetime(Fmt, Data, Acc);
get_datetime(Fmt, <<H, Data/binary>>, Acc) ->
    get_datetime(Fmt, Data, <<Acc/binary, H>>).

-spec parse_datetime(binary()) -> {calendar:datetime(), binary()}.
parse_datetime(Datetime) ->
    {Day, Datetime1} = get_day(Datetime, <<>>),
    {Month, Datetime2} = get_month(Datetime1, <<>>),
    {Year, Datetime3} = get_year(Datetime2, <<>>),
    {Time, Timezone} = get_time(Datetime3, <<>>),
    {{{Year, Month, Day}, Time}, Timezone}.

-spec month(binary()) -> 1..12.
month(<<"Jan">>) -> 1;
month(<<"Feb">>) -> 2;
month(<<"Mar">>) -> 3;
month(<<"Apr">>) -> 4;
month(<<"May">>) -> 5;
month(<<"Jun">>) -> 6;
month(<<"Jul">>) -> 7;
month(<<"Aug">>) -> 8;
month(<<"Sep">>) -> 9;
month(<<"Oct">>) -> 10;
month(<<"Nov">>) -> 11;
month(<<"Dec">>) -> 12.

-spec get_year(binary(), binary()) -> {non_neg_integer(), binary()}.
get_year(<<$:, Datetime/binary>>, Acc) ->
    {bin_to_int(Acc), Datetime};
get_year(<<H, Datetime/binary>>, Acc) ->
    get_year(Datetime, <<Acc/binary, H>>).

-spec get_month(binary(), binary()) -> {1..12, binary()}.
get_month(<<$/, Datetime/binary>>, Acc) ->
    {month(Acc), Datetime};
get_month(<<H, Datetime/binary>>, Acc) ->
    get_month(Datetime, <<Acc/binary, H>>).

-spec get_day(binary(), binary()) -> {non_neg_integer(), binary()}.
get_day(<<$/, Datetime/binary>>, Acc) ->
    {bin_to_int(Acc), Datetime};
get_day(<<H, Datetime/binary>>, Acc) ->
    get_day(Datetime, <<Acc/binary, H>>).

-spec get_time(binary(), binary()) -> {calendar:time(), binary()}.
get_time(<<$ , Timezone/binary>>, Acc) ->
    [H, M, S] = re:split(Acc, <<":">>),
    {{bin_to_int(H), bin_to_int(M), bin_to_int(S)}, Timezone};
get_time(<<H, Datetime/binary>>, Acc) ->
    get_time(Datetime, <<Acc/binary, H>>).

%% -----------------------------------------------------------------------------
%% define bin_to_int/1
%% -----------------------------------------------------------------------------
-define(B2I1, bin_to_int(X) -> list_to_integer(binary_to_list(X))).
-define(B2I2, bin_to_int(X) -> binary_to_integer(X)).

-ifdef(_R14).
?B2I1.
-endif.

-ifdef(_R15).
?B2I1.
-endif.

-ifdef(_R16).
?B2I2.
-endif.

-ifdef(_17).
?B2I2.
-endif.
