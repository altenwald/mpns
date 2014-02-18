-module(mpns_test).

-include("mpns_test.hrl").

-define(meck_lager(), begin
    meck:new(lager),
    meck:expect(lager, dispatch_log, fun(_Severity, _Metadata, _Format, _Args, _Size) ->
        %?debugFmt(_Format, _Args),
        ok
    end),
    meck:expect(lager, dispatch_log, fun(_Severity, _Module, _Function, _Line, _Pid, _Traces, _Format, _Args, _TruncSize) ->
        %?debugFmt(_Format, _Args),
        ok
    end)
end).

send_text_test() ->
    ?meck_lager(),
    mpns:start_link(undefined),
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
        {"Title", "MyGame!"},
        {"Content", "don't forget to play again!"}
    ],
    mpns:send("whatever", ?CLASS_TILE_INMEDIATE, Tags),
    mpns:send(Push, []),
    receive
        {post,{
            "whatever",
            [{"X-NotificationClass","1"}],
            "text/xml",
            "<?xml version='1.0' encoding='utf-8'?>"
            "<wp:Notification xmlns:wp='WPNotification'>"
              "<wp:Title>MyGame!</wp:Title>"
              "<wp:Content>don't forget to play again!</wp:Content>"
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
    mpns:start_link(undefined),
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
    meck:expect(mpns_test_module, expired, fun(<<"http://example.com/token">>) ->
        Pid ! ok
    end), 
    Tags = [
        {"Title", "MyGame!"},
        {"Content", "don't forget to play again!"}
    ],
    mpns:send("whatever", ?CLASS_TILE_INMEDIATE, Tags),
    receive
        {post, _Request, [{timeout, 5000}], []} -> ok;
        ok -> ok;
        Any -> throw(Any)
    after 1000 ->
        throw("TIMEOUT!!!")
    end, 
    mpns:stop(),
    meck:unload(),
    ok.
