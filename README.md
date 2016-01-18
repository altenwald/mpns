MPNS
====

[![Build Status](https://api.travis-ci.org/altenwald/mpns.png)](https://travis-ci.org/altenwald/mpns)

Microsoft Push Network Service is an application you can add to your project for give support to send push notifications to Windows Phone devices.

For send this code to the device:

```xml
<?xml version="1.0" encoding="utf-8"?>
<wp:Notification xmlns:wp="WPNotification" Version="2.0">
  <wp:Tile>
    <wp:SmallBackgroundImage Action="Clear">/small.jpg</wp:SmallBackgroundImage>
    <wp:WideBackgroundImage Action="Clear">/wide.jpg</wp:WideBackgroundImage>
    <wp:WideBackBackgroundImage Action="Clear">/back.jpg</wp:WideBackBackgroundImage>
    <wp:WideBackContent Action="Clear">/back_content.jpg</wp:WideBackContent>
    <wp:BackgroundImage Action="Clear">/background.jpg</wp:BackgroundImage>
    <wp:Count Action="Clear">5</wp:Count>
    <wp:Title Action="Clear">My App</wp:Title>
    <wp:BackBackgroundImage Action="Clear">/back_background.jpg</wp:BackBackgroundImage>
    <wp:BackTitle Action="Clear">My App (back)</wp:BackTitle>
    <wp:BackContent Action="Clear">back of medium Tile size content</wp:BackContent>
  </wp:Tile>
</wp:Notification>
```

The code to use:

```erlang
ActionClear = [clear],
Tags = [
    mpns:tile_param(<<"SmallBackgroundImage">>, ActionClear, <<"/small.jpg">>),
    mpns:tile_param(<<"WideBackgroundImage">>, ActionClear, <<"/wide.jpg">>),
    mpns:tile_param(<<"WideBackBackgroundImage">>, ActionClear, <<"/back.jpg">>),
    mpns:tile_param(<<"WideBackContent">>, ActionClear, <<"/back_content.jpg">>),
    mpns:tile_param(<<"BackgroundImage">>, ActionClear, <<"/background.jpg">>),
    mpns:tile_param(<<"Count">>, ActionClear, <<"5">>),
    mpns:tile_param(<<"Title">>, ActionClear, <<"My App">>),
    mpns:tile_param(<<"BackBackgroundImage">>, ActionClear, <<"/back_background.jpg">>),
    mpns:tile_param(<<"BackTitle">>, ActionClear, <<"My App (back)">>),
    mpns:tile_param(<<"BackContent">>, ActionClear, <<"back of medium Tile size content">>)
],
mpns:send_tile("http://...", ?CLASS_TILE_INMEDIATE, "2.0", undefined, Tags),
```

You can use `mpns:sync_send_tile/5` instead. This function returns the
information in this way:

```
{ok, Conn::string(), Notif::string(), Subs::string()} |
{error, atom()} |
{error, Code::string(), Conn::string(), Notif::string(), Subs::string()}.
```

The meaning of those params are:

* `Code` is a HTTP code and in the error should be something like 5xx or 4xx.
* `Notif` is a notification status. You can see it in [this table][https://msdn.microsoft.com/es-es/library/windows/apps/ff941100(v=vs.105).aspx#BKMK_PushNotificationServiceResponseCodes].
* `Conn` is a connection status. You can see it in [this table][https://msdn.microsoft.com/es-es/library/windows/apps/ff941100(v=vs.105).aspx#BKMK_PushNotificationServiceResponseCodes].
* `Subs` is a subscription status. You can see it in [this table][https://msdn.microsoft.com/es-es/library/windows/apps/ff941100(v=vs.105).aspx#BKMK_PushNotificationServiceResponseCodes].

More information don't hesitate to write to me or open an issue. Enjoy!