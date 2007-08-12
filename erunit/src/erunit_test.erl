-module(erunit_test).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
	[{tests, 0}];
behaviour_info(_) ->
	undefined.