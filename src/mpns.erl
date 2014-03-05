-module(mpns).
-behaviour(gen_server).

-include("mpns.hrl").

%% gen_server callbacks
-export([
    start_link/1, start/1, init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3, stop/0]).

%% API Methods
-export([
    send_toast/3, send_tile/3, send_notification/3, send_raw/3,
    tile_param/2, tile_param/3]).

-define(TIMEOUT_MPNS_ON_ERROR, 2000).

-define(XML_NOTIFY_BEGIN, 
    "<?xml version='1.0' encoding='utf-8'?>"
    "<wp:Notification xmlns:wp='WPNotification'>").

-define(XML_NOTIFY_END,
    "</wp:Notification>").

%% State
-record(state, {
    module :: atom()
}).

-spec start_link(Module::atom()) ->
    {ok, Pid::pid()} | {error, Reason::atom()}.

start_link(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Module], []).

-spec start(Module::atom()) ->
    {ok, Pid::pid()} | {error, Reason::atom()}.

start(Module) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Module], []).

-spec stop() -> ok.

stop() ->
    catch gen_server:call(?MODULE, stop).

-spec send_toast(
    BaseURL::string(), Class::string(), Params::[tile_param()]) -> ok.

send_toast(BaseURL, Class, Params) ->
    gen_server:cast(?MODULE, {send, ["toast", BaseURL, Class, toast(Params)]}).

-spec send_tile(
    BaseURL::string(), Class::string(), Params::[tile_param()]) -> ok.

send_tile(BaseURL, Class, Params) ->
    gen_server:cast(?MODULE, {send, ["token", BaseURL, Class, tile(Params)]}).

-spec send_notification(
    BaseURL::string(), Class::string(), Params::[tile_param()]) -> ok.

send_notification(BaseURL, Class, Params) ->
    gen_server:cast(?MODULE, {send, [undefined, BaseURL, Class, notification(Params)]}).

-spec send_raw(
    BaseURL::string(), Class::string(), Params::[tile_param()]) -> ok.

send_raw(BaseURL, Class, XML) ->
    gen_server:cast(?MODULE, {send, [undefined, BaseURL, Class, raw(XML)]}).

-spec tile_param(
    Name :: binary(), 
    Content :: binary()) -> tile_param().

tile_param(Name, Content) ->
    tile_param(Name, [], Content).

-spec tile_param(
    Name :: binary(), 
    Attrs :: [attribute()], 
    Content :: binary()) -> tile_param().

tile_param(Name, Attrs, Content) ->
    #tile_param{name=Name, attrs=Attrs, content=Content}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec init( Args :: [term()] ) -> 
    {ok, State :: #state{}}.

init([Module]) ->
    {ok, #state{module=Module}}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
-spec handle_info(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}}.

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
-spec handle_cast(Msg::any(), State::#state{}) ->
    {noreply, State::#state{}}.

handle_cast({send, Msg}, #state{module=Module}=State) ->
    spawn(fun() ->
        erlang:apply(fun send/5, [Module|Msg])
    end),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

-spec handle_call(Msg::any(), From::{pid(),_}, State::#state{}) ->
    {noreply, State::#state{}} |
    {stop, Reason::atom(), State::#state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, State};

handle_call({send, Msg}, From, #state{module=Module}=State) ->
    spawn(fun() ->
        Res = erlang:apply(fun send/5, [Module|Msg]),
        gen_server:reply(From, Res)
    end),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------

-spec terminate(Reason::any(), State::#state{}) -> ok.

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------

-spec code_change(OldVsn::string(), State::#state{}, Extra::any()) ->
    {ok, State::#state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec to_xml(tile_param()) -> string().

to_xml(#tile_param{name=Name, attrs=Attrs, content=Content}) ->
    "<wp:" ++ Name ++
    lists:foldl(fun({Var,Val}, Res) ->
        % TODO: sanitize Val
        Res ++ " " ++ binary_to_list(Var) ++ "='" ++ 
        binary_to_list(Val) ++ "'"
    end, "", Attrs) ++ ">" ++
    % TODO: sanitize Content
    binary_to_list(Content) ++ 
    "</wp:" ++ binary_to_list(Name) ++ ">".

-spec toast(Params::[tile_param()]) -> string().

toast(Params) ->
    notification("Toast", Params).

-spec tile(Params::[tile_param()]) -> string().

tile(Params) ->
    notification("Tile", Params).

-spec raw(XML::string()) -> string().

raw(XML) ->
    notification(undefined, XML).

-spec notification(Params::[tile_param()]) -> string().

notification(Params) ->
    notification(undefined, Params).

-type notification_type() :: string() | undefined.

-spec notification(
    Type::notification_type(), 
    Params::[tile_param()] | string()) -> string().

notification(Tag, Params) ->
    case Tag of
        undefined -> ?XML_NOTIFY_BEGIN;
        Tag -> ?XML_NOTIFY_BEGIN "<wp:" ++ Tag ++ ">"
    end ++
    case Params of
    [#tile_param{}|_] ->
        lists:foldl(fun(Param, Res) ->
            Res ++ to_xml(Param)
        end, "", Params);
    _ ->
        Params
    end ++
    case Tag of
        undefined -> ?XML_NOTIFY_END;
        Tag -> "</wp:" ++ Tag ++ ">" ?XML_NOTIFY_END
    end.


-spec send(
    Module::atom(), Type::string() | undefined, BaseURL::string(), 
    Class::string(), Params::[tile_param()]) -> ok.

send(Module, Type, BaseURL, Class, Content) ->
    InputHeaders = case Type of 
    undefined -> 
        [{"X-NotificationClass", Class}];
    _ -> [
        {"X-WindowsPhone-Target", Type},
        {"X-NotificationClass", Class}
    ]
    end,
    Response = httpc:request(post, 
        {BaseURL, InputHeaders, "text/xml", Content}, 
        [{timeout, 5000}], []),
    lager:debug("get response=~p~n", [Response]),
    case Response of
        {ok, {{_Version, 200, _Reason}, Headers, _BodyResp}} ->
            ConnStatus = proplists:get_value("x-deviceconnectionstatus", Headers),
            NotStatus = proplists:get_value("x-notificationstatus", Headers),
            SuscriptStatus = proplists:get_value("x-subscriptionstatus", Headers),
            lager:info(
                "Sent to ~p with Conn=~p; Notify=~p; Suscription=~p~n", 
                [BaseURL, ConnStatus, NotStatus, SuscriptStatus]
            );
        {ok, {{_Version, Code, _Reason}, Headers, _BodyResp}} ->
            ConnStatus = proplists:get_value("x-deviceconnectionstatus", Headers),
            NotStatus = proplists:get_value("x-notificationstatus", Headers),
            SuscriptStatus = proplists:get_value("x-subscriptionstatus", Headers),
            case {Code, ConnStatus,NotStatus,SuscriptStatus} of
                {_, _, "Dropped", "Expired"} ->
                    case Module of
                        undefined -> 
                            ok;
                        _ ->
                            lager:info("Expired: ~p:expired(~p).~n", 
                                [Module, BaseURL]), 
                            Module:expired(BaseURL)
                    end,
                    lager:warning("Expired token ~p~n", [BaseURL]);
                {_, _, "Dropped", _} ->
                    lager:warning("Dropped message due to ~s from ~p~n", [SuscriptStatus, BaseURL]);
                {503, _, _, _} ->
                    lager:warning("Microsoft Server Error (503) with URL ~p~n", [BaseURL]);
                _ ->
                    lager:error(
                        "Sent from ~p with Code=~p; Conn=~p; Notify=~p; Suscription=~p~n", 
                        [BaseURL, Code, ConnStatus, NotStatus, SuscriptStatus]
                    )
            end;
        {error, no_scheme} ->
            lager:error("invalid URL [~p]~n", [BaseURL]);
        {error, timeout} ->
            lager:warning("timeout in request to MPNS retrying in ~p seconds.~n", [?TIMEOUT_MPNS_ON_ERROR]),
            timer:sleep(?TIMEOUT_MPNS_ON_ERROR),
            send(Module, Type, BaseURL, Class, Content);
        {error, {failed_connect,_ConnectInfo}} ->
            lager:warning("cannot connect to MPNS, retrying in ~p seconds.~n", [?TIMEOUT_MPNS_ON_ERROR]),
            timer:sleep(?TIMEOUT_MPNS_ON_ERROR),
            send(Module, Type, BaseURL, Class, Content);
        Any ->
            lager:error("Fatal error: ~p~n", [Any])
    end,
    ok.
