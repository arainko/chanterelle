MVP of the library:

  Support for:
    [X] named tuples
    [X] tuples
    [X] options
    [X] collections

  Modifiers:
    [X] single-element .add
    [X] single-element .compute
    [X] update
    [] remove (both named and positional tuples)
    field renames:
      [] local
      [] regional

  Other:
    [] sane errors
    [] optimized interpreter - turn non-modified branches into Leaves that just take the source value

Further ideas:
  * deep merging
  * turn a case class hierarchy into a named tuple/back to a case class
