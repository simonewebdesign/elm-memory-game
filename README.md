# Setup

You need to have a working copy of [Elm](http://elm-lang.org) in your `$PATH`.

After that, you can `make install` to pull needed dependencies (for now, only OSX and Linux 64bit are supported).

Then you can run `make` to build the project. Everything will be bundled in the `build` directory, which can be served
by any static file server.

## Development

For development, make sure you have [Watchman](https://facebook.github.io/watchman/) installed.

You can then run `make watch` to start watching files.

For convenience, `make server` will start a live-reload enabled server.
