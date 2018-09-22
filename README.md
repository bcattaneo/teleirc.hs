# teleirc.hs

Simple Telegram <-> IRC relay bot written in Haskell

Requirements
------
* haskell
* Modules:
    * http-conduit
    * unliftio

Installing modules
------
```
cabal install http-conduit unliftio
```

Running
------
```
ghci main.hs
```
And then just type `main`

Compiling
------
```
ghc --make main.hs
./main
```
(Or ./main.exe in Windows)