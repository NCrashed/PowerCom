PowerCom
========

Serial port chat to train Haskell.

I've run into big problem with Gtk2Hs and Cloud Haskell. Gtk bindings are using IO () callbacks and Cloud Haskell 
is using Process monad to handle messages communication. Last easy workaround i found is to spawn new process from IO monad,
but it is so redundant... project is freezed temporarily.

Compilation
===========

First, install GtkHs from [this tutorial](http://www.haskell.org/haskellwiki/Gtk2Hs/Installation).

And finally:
```
cd src
cabal install
```
