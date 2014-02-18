MPNS
====

Microsoft Push Network Service is an application you can add to your project for give support to send push notifications to Windows Phone devices.

For send this code to the device:

```xml
<?xml version="1.0" encoding="utf-8"?>
<wp:Notification xmlns:wp="WPNotification">
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
ActionClear = [{"Action", "Clear"}],
Tags = [
    mpns:tile_param("SmallBackgroundImage", ActionClear, "/small.jpg"),
    mpns:tile_param("WideBackgroundImage", ActionClear, "/wide.jpg"),
    mpns:tile_param("WideBackBackgroundImage", ActionClear, "/back.jpg"),
    mpns:tile_param("WideBackContent", ActionClear, "/back_content.jpg"),
    mpns:tile_param("BackgroundImage", ActionClear, "/background.jpg"),
    mpns:tile_param("Count", ActionClear, "5"),
    mpns:tile_param("Title", ActionClear, "My App"),
    mpns:tile_param("BackBackgroundImage", ActionClear, "/back_background.jpg"),
    mpns:tile_param("BackTitle", ActionClear, "My App (back)"),
    mpns:tile_param("BackContent", ActionClear, "back of medium Tile size content")
],
mpns:send("http://...", ?CLASS_TILE_INMEDIATE, Tags),
```
