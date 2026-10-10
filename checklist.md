### Test interactions
* hoisted with merged (not whether it works with hoisting a .merged node, just that they don't interefere with each other - merged nodes are not supported for hoisting at all (for now))
* hoisted.local with named/tuple (merged is not supported on the Transformation level even tho it nostly works out fine on the Plan level - this is to be addressed later on)
* error messages around .passthroughHoisted with Accumulating (not Accumulating & FailFast)
* interactions with .some when operating with `Mode[Option]` - I'd expect this to work the same .element does
* interactions with .leftValue and .rightValue with `Mode[Either]` - .rightValue should work the same way, .leftValue ... not sure (make sure there's a good error message)
* add tests for hoisted.regional in combo with Accumulating (to see whether we're actually stamping the bottommost Wrapped node with Hoisted.Yes instead of Hoisted.Passthrough) - make sure there's a good error message
* add a test that confirm that Plan.updateTransitively goes through all possible node
* copy over Mode tests from ducktape
* add a test for Collection.Builder.fromAppendable (AND COME UP WITH A BETTER NAME?)
* add tests for catsInterop? Or just don't publish it yet (but I still want to be sure that all this stuff works for the likes of `dynosaur` or `decline`)
