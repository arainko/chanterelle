### Test interactions
* hoisted with merged
* hoisted.local with named/tuple/merged
* error messages around .passthroughHoisted with Accumulating (not Accumulating & FailFast)
* interactions with .some when operating with `Mode[Option]`
* interactions with .leftValue and .rightValue with `Mode[Either]`
* add tests for hoisted.regional in combo with Accumulating (to see whether we're actually stamping the bottommost Wrapped node with Hoisted.Yes instead of Hoisted.Passthrough)
* add a test that confirm that Plan.updateTransitively goes through all possible nodes
