%% NOTE: prevent having a look at the this source code.
%%
%% it's awful, but at least, it can generate some data for testing purposes.

-module(helper).
-export([generate_file/2]).

generate_file(Filename, NLines) ->
    filelib:ensure_dir(Filename),
    Data = gen_data(NLines, []),
    file:write_file(Filename, Data).

gen_data(0, Acc) ->
    Acc;
gen_data(N, Acc) ->
    gen_data(N-1, [list_to_binary(gen_line())|Acc]).

gen_line() ->
    lists:flatten(lists:concat([gen_ip(), " - ", "- ", "[", rand(1, 30), "/", gen_month(),
                                "/", rand(2000, 2014), ":", rand(0, 24), ":", rand(0, 59),
                                ":", rand(0, 59), " +0200] \"", gen_method(), " ", gen_url(),
                                " HTTP/1.1\" ", gen_status(), " ", gen_content_length(),
                                " \"-\" ", "\"", gen_user_agent(), "\"\n"])).

gen_ip() ->
    Lo = 0,
    Hi = 255,
    lists:concat([rand(Lo, Hi), ".", rand(Lo, Hi), ".",
                  rand(Lo, Hi), ".", rand(Lo, Hi)]).

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

gen_month() ->
    month(rand(1, 12)).

rand(Lo, Hi) ->
    crypto:rand_uniform(Lo, Hi).

gen_method() ->
    Methods = ["GET", "PUT", "POST", "DELETE", "HEAD", "PATCH"],
    lists:nth(rand(1, length(Methods)), Methods).

gen_url() ->
    "/" ++ gen_str(100).

gen_status() ->
    St = [200,201,202,203,204,205,206,300,301,302,303,304,305,306,307,400,401,
          402,403,404,405,406,407,408,409,410],
    lists:nth(rand(1, length(St)), St).

gen_content_length() ->
    rand(0, 10000).

gen_user_agent() ->
    gen_str(20).

chars() ->
    ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
     "o", "p", "q", "r", "s", "t", "u", "w", "x", "y", "z", "/", "?", "="].

gen_str(Hi) ->
    [lists:nth(rand(1, length(chars())), chars()) || _ <- lists:seq(1, rand(1, Hi))].
