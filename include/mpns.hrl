
% TOAST DELIVERY CLASSES
-define(CLASS_TOAST_INMEDIATE, "2").
-define(CLASS_TOAST_450S, "12").
-define(CLASS_TOAST_900S, "22").

% TILE DELIVERY CLASSES
-define(CLASS_TILE_INMEDIATE, "1").
-define(CLASS_TILE_450S, "11").
-define(CLASS_TILE_900S, "21").

-type variable() :: binary().
-type value() :: binary().
-type attribute() :: {variable(), value()}.

-record(tile_param, {
	name :: binary(),
	attrs = [] :: [attribute()],
	content :: binary()
}).

-type tile_param() :: #tile_param{}.
