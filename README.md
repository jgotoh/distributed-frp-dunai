# Distributed Systems Extensions for the Dunai FRP Library

This library provides a client/ server infrastructure by using [Cloud Haskell](https://haskell-distributed.github.io/) for distributed FRP applications implemented with [Dunai/ BearRiver](https://github.com/ivanperez-keera/dunai).

Its features are demonstrated in an exemplary application called `distributed-paddles`.
`distributed-paddles` needs [SDL2](https://www.libsdl.org/download-2.0.php)(package [libsdl2-dev](https://packages.ubuntu.com/bionic/libdevel/libsdl2-dev) in Ubuntu, [SDL2-devel](https://pkgs.org/download/SDL2-devel) in rpm) and [SDL2_gfx](http://www.ferzkopp.net/wordpress/2016/01/02/sdl_gfx-sdl2_gfx/)(package [libsdl2-gfx-dev](https://packages.ubuntu.com/bionic/libdevel/libsdl2-gfx-dev) in Ubuntu, [SDL2_gfx-devel](https://pkgs.org/download/SDL2_gfx-devel) in rpm) installed on the system.

Install them with your favorite package manager, e.g `apt install libsdl2-dev; apt install libsdl2-gfx-dev`.

To build/install the project with cabal `--allow-newer` needs to be used:

`cabal build --allow-newer`

`cabal install --allow-newer`

Installation and build process was tested with Cabal 3.2.0.0, GHC 8.2.2 and GHC 8.8.3.

Modules of the client/server architecture:

- `Network.Server` exports functions necessary to create a server application. Servers generate state (which will be sent to clients) and are the central authority of an application in this project. They process commands received by clients.

- `Network.Client` exports functions to create a client application. Clients display states and generate commands from user input, which will be sent to a server.

- `Network.Common` contains types and functions used by both servers and clients

There is also support for several mechanisms to synchronise the state between running instances of an application:

- Time Warp Synchronisation is supported for Servers, for a version of reactimate which uses time warp, see module `FRP.BearRiver.Network.TimeWarp`. A reactimate version for clients in this context is in module `FRP.BearRiver.Network.Reactimate`.

- Dead Reckoning and Client Side Prediction is supported for Clients, see modules `Data.MonadicStreamFunction.Prediction` and `FRP.BearRiver.Network.Prediction`

Documentation of the project via Haddock is provided in directory `doc`.
For example usage of the modules, see `distributed-paddles` and the unit tests.

## distributed-paddles

`distributed-paddles` is an example application inspired by PONG&trade; (Atari Interactive, Inc., 1972).

Command line arguments are used to decide whether to host a session as a
server or join a session as a client. In addition, command line arguments should
be used to decide which consistency maintenance mechanisms to use.

Command line arguments for servers:
- `--host` flag to initiate server startup
- `--ip IP` on which IP to host the server (e.g localhost)
- `--p PORT` port of server
- `--name NAME` name of the session hosted
- `--d LENGTH` length of the playable round in seconds
- `--t` flag to turn on time warp synchronisation

To create a session called `name` on localhost, port `3000`, maximum length of the game 30 seconds, using time warp, use:

`cabal new-run distributed-paddles -- --host --ip 127.0.0.1 --p 3000 --name name --d 30 --t`

When the server started correctly, it will print something like this: `Server starts at: 127.0.0.1:3000:0 ... `.
`127.0.0.1:3000:0` is the address of the server which has to be used by clients to connect.

Command line arguments for clients:
- `--ip IP` on which ip to start the server
- `--p PORT` port of client
- `--nick NICK` name of client
- `--name NAME` name of session to join
- `--s ADDRESS` address of the server, in this format: `IP:PORT:0`, see above
- `--csp` turn on client side prediction (default is off)
- `--drmFirst` turn on dead reckoning (default is off)

To join with a nickname `A`, receiving messages on port `3001`, using client side prediction and dead reckoning:

`cabal new-run distributed-paddles -- --ip 127.0.0.1 --p 3001 --name name --s 127.0.0.1:3000:0 --nick A --csp --drmFirst`

Paddles can be moved up and down by pressing arrow keys.

### Simple startup

To run a server with `n` hosts that join, run `run_paddles n`, e.g `run_paddles 2`. 

Running with argument 0 only starts up a server.

When the server started successfully, press any key to start the clients.

To exit the session, press again any key.

### Profiling

To automatically run a server with two hosts that join using profiling, run `run_profiling`.
Running a profiled program called x always writes its output to x.prof/ x.hp.
The workaround used here is to execute different copies of the executable.
See comments in the file to get the script to work correctly.

## Formatting

All Haskell source files are formatted via [Brittany](https://github.com/lspitzner/brittany/).

## network-transport-tcp

The library uses a modified version of the package `network-transport-tcp` with an updated dependency to the `network` package to version 3.

## distributed-process

A fork of `distributed-process` is used to get compatibility with GHC 8.8.
