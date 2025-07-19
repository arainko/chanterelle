# 🚧 WORK IN PROGRESS 🚧

## chanterelle

A Scala library that's all about named tuples.

### Feature checklist :

Support for:
* :heavy_check_mark: named tuples
* :heavy_check_mark: tuples
* :heavy_check_mark: options
* :heavy_check_mark: collections

Modifiers:
* :heavy_check_mark: single-element .put
* :heavy_check_mark: single-element .compute
* :heavy_check_mark: update
* :heavy_check_mark: remove (both named and positional tuples)
  
  field renames:
    * 🔜 local
    * 🔜 regional

Other:
  * 🔜 sane errors
  * :heavy_check_mark: optimized interpreter - turn non-modified branches into Leaves that just take the source value

Further ideas:
  * deep merging
  * turn a case class hierarchy into a named tuple/back to a case class
