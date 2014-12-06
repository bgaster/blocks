ABC or Another Blocks Clone
======

This is a very simple Haskell implementation of Blocks (aka Tetris)
that I'm using to develop some basic ideas in game play using
Haskell. The current implementation uses:

* OpenGL 3.1 (via vinyl-gl and GLUtil)
  * The shaders are trivial as all they do is single texture lookup
* A simply renderer loop in the IO monad

TODO
----

1. Vinyl-gl does not allow seperate vertexes and UVs, i.e. SOA.
  * Extend Vinyl-GL to do this.
2. Extend GLUtil to add text rendering support
3. Add renderer score board
4. Add some music
5. Add pause key
6. Add drop key
7. Add new game
8. Reright using FRP, most likely Netwire but we will see
  * I'm actually not convinced tht this is better than the current approach but I want to better understand this "more functional" approach.