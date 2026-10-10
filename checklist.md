### Test interactions
* hoisted.local with named/tuple (merged is not supported on the Transformation level even tho it nostly works out fine on the Plan level - this is to be addressed later on)
* error messages around .passthroughHoisted with Accumulating (not Accumulating & FailFast)
* interactions with .some when operating with `Mode[Option]` - I'd expect this to work the same .element does
* interactions with .leftValue and .rightValue with `Mode[Either]` - .rightValue should work the same way, .leftValue ... not sure (make sure there's a good error message)
* add a test that confirm that Plan.updateTransitively goes through all possible node
* copy over Mode tests from ducktape
* add a test for Collection.Builder.fromAppendable (AND COME UP WITH A BETTER NAME?)
* add tests for catsInterop? Or just don't publish it yet (but I still want to be sure that all this stuff works for the likes of `dynosaur` or `decline`)
