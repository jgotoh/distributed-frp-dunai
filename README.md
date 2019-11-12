# Distributed Systems Extensions for the Dunai FRP Library

## distributed-paddles

`distributed-paddles` is an example application inspired by PONG&trade; (Atari Interactive, Inc., 1972).

The application needs [SDL2](https://www.libsdl.org/download-2.0.php) and [SDL2-gfx](http://www.ferzkopp.net/wordpress/2016/01/02/sdl_gfx-sdl2_gfx/) installed on the system.

A session can be created or joined:

To create a session called `name` on localhost, port `3000`:

`cabal new-run distributed-paddles -- --host --ip 127.0.0.1 --p 3000 --name name`

To join with a nickname `A`, receiving messages on port `3001`:

`cabal new-run distributed-paddles -- --ip 127.0.0.1 --p 3001 --name name --s 127.0.0.1:3000:0 --nick A`

### Simple startup

To run a server with `n` hosts that join, run `run_test n`, e.g `run_test 2`. 

Running with argument 0 only starts up a server.

When the server started successfully, press any key to start the clients.

To exit the session, press again any key.

### Profiling

To automatically run a server with two hosts that join using profiling, run `run_profiling`.

## hlint

This project uses `hlint` to statically check for code improvements.

For more information see:
https://github.com/ndmitchell/hlint

## ghcid

Running `ghcid` then automatically checks source files for errors on changes.
`ghcid` can be modified in file `.ghcid`.

To modify GHCi startup used by ghcid, modify `.ghci`

For more information see:
https://github.com/ndmitchell/ghcid

## Formatting

All Haskell source files are formatted via [Brittany](https://github.com/lspitzner/brittany/).

## network-transport-tcp

The library uses a modified version of the package `network-transport-tcp`. The modified version updated the `network` dependency to version 3 and removed the upper bounds of the `async` package to support newer versions of GHC.

However removing the upper bounds of `async` lead to a failed test (TestTCP.hs:testUnnecessaryConnect), so extra care should be taken to not reconnect to a socket already connected to.


