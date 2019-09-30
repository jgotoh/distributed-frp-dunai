# Distributed Systems Extensions for the Dunai FRP Library

## simple-example

The example needs [SDL2](https://www.libsdl.org/download-2.0.php) and [SDL2-gfx](http://www.ferzkopp.net/wordpress/2016/01/02/sdl_gfx-sdl2_gfx/) installed on the system.

A P2P session can be created or joined:

To create a session on localhost:
`cabal new-run exes -- --ip 127.0.0.1 --p 3000 --host`

To join:
`cabal new-run exes -- --ip 127.0.0.1 --p 3000`

## hlint

This project uses `hlint` to statically check for errors.

For more information see:
https://github.com/ndmitchell/hlint

## ghcid

Running `ghcid` then automatically checks source files for errors on changes.
`ghcid` can be modified in file `.ghcid`.

To modify GHCi startup used by ghcid, modify `.ghci`

For more information see:
https://github.com/ndmitchell/ghcid

**TODO** source code formatter

## Profiling

To visualize the profiling report, run `hp2ps -e8in -c simple-example.hp`

## network-transport-tcp

The library uses a modified version of the package `network-transport-tcp`. The modified version updated the `network` dependency to version 3 and removed the upper bounds of the `async` package to support newer versions of GHC.

However removing the upper bounds of `async` lead to a failed test (TestTCP.hs:testUnnecessaryConnect), so extra care should be taken to not reconnect to a socket already connected to.


