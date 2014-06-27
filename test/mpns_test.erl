-module(mpns_test).
-compile([export_all, debug_info, warnings_as_errors]).

-include_lib("eunit/include/eunit.hrl").
-include("mpns.hrl").

-define(meck_lager(), begin
    meck:new(lager),
    meck:expect(lager, dispatch_log, fun(_Severity, _Metadata, _Format, _Args, _Size) ->
        ?debugFmt(_Format, _Args),
        ok
    end),
    meck:expect(lager, dispatch_log, fun(_Severity, _Module, _Function, _Line, _Pid, _Traces, _Format, _Args, _TruncSize) ->
        ?debugFmt(_Format, _Args),
        ok
    end)
end).

send_text_test() ->
    ?meck_lager(),
    mpns:start(undefined),
    Pid = self(),
    meck:new(httpc),
    meck:expect(httpc, request, fun(Method, Request, HttpOpts, Opts) ->
        Pid ! {Method, Request, HttpOpts, Opts},
        {ok, {{"HTTP/1.1", 200, "OK"}, [
            {"x-deviceconnectionstatus", "Connected"},
            {"x-notificationstatus", "Received"},
            {"x-subscriptionstatus", "Active"}
        ], []}}
    end),
    Tags = [
        mpns:tile_param(<<"Title">>, <<"MyGame!">>),
        mpns:tile_param(<<"Content">>, <<"don't forget to play again!">>)
    ],
    mpns:send_tile("whatever", ?CLASS_TILE_INMEDIATE, Tags),
    receive
        {post,{
            "whatever",
            [{"X-WindowsPhone-Target","token"},{"X-NotificationClass","1"}],
            "text/xml",
            "<?xml version='1.0' encoding='utf-8'?>"
            "<wp:Notification xmlns:wp='WPNotification'>"
                "<wp:Tile>"
                    "<wp:Title>MyGame!</wp:Title>"
                    "<wp:Content>don't forget to play again!</wp:Content>"
                "</wp:Tile>"
            "</wp:Notification>"
        }, [{timeout,5000}], []} -> ok;
        Any -> throw(Any)
    after 1000 ->
        throw("TIMEOUT!!!")
    end, 
    mpns:stop(),
    meck:unload(),
    ok.

expire_test() ->
    ?meck_lager(),
    mpns:start(mpns_test_module),
    Pid = self(),
    meck:new(httpc),
    meck:expect(httpc, request, fun(Method, Request, HttpOpts, Opts) ->
        Pid ! {Method, Request, HttpOpts, Opts},
        {ok, {{"HTTP/1.1", 412, "Whatever"}, [
            {"x-deviceconnectionstatus", "Disconnected"},
            {"x-notificationstatus", "Dropped"},
            {"x-subscriptionstatus", "Expired"}
        ], []}}
    end),
    meck:new(mpns_test_module),
    meck:expect(mpns_test_module, expired, fun("whatever") ->
        Pid ! ok
    end), 
    Tags = [
        mpns:tile_param(<<"Title">>, <<"MyGame!">>),
        mpns:tile_param(<<"Content">>, <<"don't forget to play again!">>)
    ],
    mpns:send_tile("whatever", ?CLASS_TILE_INMEDIATE, Tags),
    lists:foreach(fun(_) ->
        receive
            {post, _Request, [{timeout, 5000}], []} -> ok;
            ok -> ok;
            Any -> throw(Any)
        after 1000 ->
            throw("TIMEOUT!!!")
        end
    end, [1,2]),
    mpns:stop(),
    meck:unload(),
    ok.
