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

-module(zeta).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([parse/2]).
-export([parse/3]).
-export([parse_file/1]).
-export([parse_file/2]).
-export([parse_file/3]).

%% -----------------------------------------------------------------------------
%% types
%% -----------------------------------------------------------------------------
-type fmt() :: string() | binary().
-type data() :: string() | binary().
-type return_type() :: binary | list | auto.
-type fun_application() :: fun((atom(), binary()) -> term()).
-type log_kv() :: {host, binary() | list() | term() | inet:ip_address()}
                | {user, binary() | list() | term()}
                | {datetime, binary() | list() | calendar:datetime() | term()}
                | {timezone, binary() | list() | term()}
                | {method, binary() | list() | term()}
                | {url, binary() | list() | term()}
                | {version, binary() | list() | term()}
                | {status, binary() | list() | integer() | term()}
                | {content_length, binary() | list() | integer() | term()}
                | {binary() | list() | term(), binary() | list() | term()}.
-type log_data() :: [log_kv()].
-type filename() :: file:name_all().
-type parser_fun() :: fun(() -> {ok, log_data(), parser_fun()} | eof).

-export_type([fmt/0]).
-export_type([data/0]).
-export_type([return_type/0]).
-export_type([fun_application/0]).
-export_type([log_data/0]).
-export_type([filename/0]).
-export_type([parser_fun/0]).

%% -----------------------------------------------------------------------------
%% default log format
%% -----------------------------------------------------------------------------
-define(LOG_FMT, <<"~h ~l ~u ~t \"~r\" ~s ~B \"~{referer}\" \"~{user-agent}\"">>).

%% -----------------------------------------------------------------------------
%% parse log data
%% -----------------------------------------------------------------------------
-spec parse(fmt(), data()) -> {ok, log_data()} | {error, any()}.
parse(Fmt, Data) ->
    parse(Fmt, Data, binary).

%% -----------------------------------------------------------------------------
%% parse log data and apply a function to each value
%% -----------------------------------------------------------------------------
-spec parse(fmt(), data(), return_type() | fun_application()) ->
                   {ok, log_data()} | {error, any()}.
parse(Fmt, Data, F) ->
    case get_fun_application(F) of
        {ok, F1} ->
            parse1(Fmt, Data, F1);
        Else ->
            Else
    end.

%% -----------------------------------------------------------------------------
%% parse a file in streaming fashion
%% -----------------------------------------------------------------------------
-spec parse_file(filename()) -> {ok, [log_data()]} | {error, any()}.
parse_file(Filename) ->
    parse_file(Filename, ?LOG_FMT).

-spec parse_file(filename(), fmt()) -> {ok, log_data(), parser_fun()} |
                                       {error, any(), parser_fun()} |
                                       {error, any()}.
parse_file(Filename, Fmt) ->
    parse_file(Filename, Fmt, binary).

-spec parse_file(filename(), fmt(), return_type() | fun_application()) ->
                        {ok, log_data(), parser_fun()} |
                        {error, any(), parser_fun()} |
                        {error, any()}.
parse_file(Filename, Fmt, F) ->
    case get_fun_application(F) of
        {ok, F1} ->
            case file:read_file(Filename) of
                {ok, <<>>} ->
                    {ok, [], fun() -> eof end};
                {ok, Content} ->
                    parse_stream(Fmt, Content, F1);
                Else ->
                    Else
            end;
        Else ->
            Else
    end.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
-spec parse1(fmt(), data(), return_type() | fun_application()) ->
                    {ok, log_data()} | {error, any()}.
parse1(Fmt, Data, F) ->
    case (catch parse(to_bin(Fmt), to_bin(Data), F, [])) of
        X = {ok, _} ->
            X;
        _ ->
            {error, bad_format}
    end.

-spec to_bin(binary() | string()) -> binary().
to_bin(X) when is_binary(X) -> X;
to_bin(X) when is_list(X) -> list_to_binary(X).

%% -----------------------------------------------------------------------------
%% get function application
%% -----------------------------------------------------------------------------
-spec get_fun_application(return_type()) -> {ok, fun_application()} |
                                            {error, badarg}.
get_fun_application(binary) ->
    {ok, fun(_, X) -> X end};
get_fun_application(list) ->
    {ok, fun(_, X) -> convert_to(list, X) end};
get_fun_application(auto) ->
    {ok, fun convert_to/2};
get_fun_application(F) when is_function(F) ->
    {ok, F};
get_fun_application(_) ->
    {error, badarg}.

%% -----------------------------------------------------------------------------
%% parse data and generate function for parsing data in a stream way
%% -----------------------------------------------------------------------------
-spec parse_stream(fmt(), binary(), return_type() | fun_application()) ->
                          {ok, log_data(), parser_fun()} |
                          {error, any(), parser_fun()} | eof.
parse_stream(_, <<>>, _) ->
    eof;
parse_stream(Fmt, Stream, F) ->
    {Data, Rest} = splitnl(Stream),
    case parse1(Fmt, Data, F) of
        {ok, X} ->
            {ok, X, fun() -> parse_stream(Fmt, Rest, F) end};
        {error, Reason} ->
            {error, Reason, fun() -> parse_stream(Fmt, Rest, F) end}
    end.

%% -----------------------------------------------------------------------------
%% split with new line
%% -----------------------------------------------------------------------------
-spec splitnl(binary()) -> {binary(), binary()}.
splitnl(Data) ->
    case binary:split(Data, <<"\n">>, [trim]) of
        [] -> {<<>>, <<>>};
        [H] -> {H, <<>>};
        [H, Rest] -> {H, Rest}
    end.

%% -----------------------------------------------------------------------------
%% log format parser
%% -----------------------------------------------------------------------------
-spec parse(binary(), binary(), fun_application(), [term()]) ->
                   {ok, log_data()}.
parse(<<>>, <<>>, _, Acc) ->
    {ok, Acc};
parse(<<"~h", Fmt/binary>>, Data, F, Acc) ->
    {V, Fmt1, Data1} = get_value(Fmt, Data),
    parse(Fmt1, Data1, F, Acc ++ [{host, F(host, V)}]);
parse(<<"~l", Fmt/binary>>, <<$-, Data/binary>>, F, Acc) ->
    parse(Fmt, Data, F, Acc);
parse(<<"~u", Fmt/binary>>, Data, F, Acc) ->
    {V, Fmt1, Data1} = get_value(Fmt, Data),
    parse(Fmt1, Data1, F, Acc ++ [{user, F(user, V)}]);
parse(<<"~t", Fmt/binary>>, Data, F, Acc) ->
    {V, Fmt1, Data1} = get_datetime(Fmt, Data),
    [Datetime, Timezone] = binary:split(V, <<" ">>),
    Acc1 = Acc ++ [{datetime, F(datetime, Datetime)},
                   {timezone, F(timezone, Timezone)}],
    parse(Fmt1, Data1, F, Acc1);
parse(<<"~r", Fmt/binary>>, Data, F, Acc) ->
    {V, Fmt1, Data1} = get_value(Fmt, Data),
    [Method, URL, Version] = re:split(V, <<" ">>),
    Acc1 = Acc ++ [{method, F(method, Method)},
                   {url, F(url, URL)},
                   {version, F(version, Version)}],
    parse(Fmt1, Data1, F, Acc1);
parse(<<"~s", Fmt/binary>>, Data, F, Acc) ->
    {<<S, T, A, V/binary>>, Fmt1, Data1} = get_value(Fmt, Data),
    Acc1 = Acc ++ [{status, F(status, <<S, T, A>>)}],
    parse(Fmt1, <<V/binary, Data1/binary>>, F, Acc1);
parse(<<"~b", Fmt/binary>>, Data, F, Acc) ->
    {V, Fmt1, Data1} = get_value(Fmt, Data),
    parse(Fmt1, Data1, F, Acc ++ [{content_length, F(content_length, V)}]);
parse(<<"~B", Fmt/binary>>, Data, F, Acc) ->
    {V, Fmt1, Data1} = get_value(Fmt, Data),
    parse(Fmt1, Data1, F, Acc ++ [{content_length, F(content_length, V)}]);
parse(<<"~{", Fmt/binary>>, Data, F, Acc) ->
    {Name, Fmt1} = get_name(Fmt, <<>>),
    {V, Fmt2, Data1} = get_value(Fmt1, Data),
    parse(Fmt2, Data1, F, Acc ++ [{F(header_name, Name), F(header, V)}]);
parse(<<H, Fmt/binary>>, <<H, Rest/binary>>, F, Acc) ->
    parse(Fmt, Rest, F, Acc).

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

-spec parse_datetime(binary()) -> calendar:datetime().
parse_datetime(Datetime) ->
    {Day, Datetime1} = get_v_til($/, Datetime, <<>>),
    {Month, Datetime2} = get_v_til($/, Datetime1, <<>>),
    {Year, Time} = get_v_til($:, Datetime2, <<>>),
    [H, M, S] = binary:split(Time, <<":">>, [global]),
    {{bin_to_int(Year), month(Month), bin_to_int(Day)},
     {bin_to_int(H), bin_to_int(M), bin_to_int(S)}}.

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

-spec get_v_til(non_neg_integer(), binary(), binary()) -> {binary(), binary()}.
get_v_til(X, <<X, Data/binary>>, Acc) ->
    {Acc, Data};
get_v_til(X, <<H, Data/binary>>, Acc) ->
    get_v_til(X, Data, <<Acc/binary, H>>).

%% -----------------------------------------------------------------------------
%% data type conversion to deal with return type
%% -----------------------------------------------------------------------------
-spec convert_to(atom(), term()) -> term().
convert_to(list, X) -> binary_to_list(X);
convert_to(integer, X) -> bin_to_int(X);
convert_to(host, X) ->
    case inet_parse:address(binary_to_list(X)) of
        {ok, Host} -> Host;
        _ -> X
    end;
convert_to(user, X) -> X;
convert_to(datetime, X) -> parse_datetime(X);
convert_to(timezone, X) -> X;
convert_to(method, X) -> X;
convert_to(url, X) -> X;
convert_to(version, X) -> X;
convert_to(status, X) -> bin_to_int(X);
convert_to(content_length, X) -> bin_to_int(X);
convert_to(header, X) -> X;
convert_to(header_name, X) -> X.

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
