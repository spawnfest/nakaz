      ____   _  ____    __  __  ____    ______
     |    \ | ||    \  |  |/ / |    \  |___   |
     |     \| ||     \ |     \ |     \  .-`.-`
     |__/\____||__|\__\|__|\__\|__|\__\|______|

           configuration management done right!
           making sysops happier one at a time!

Is it any good?
---------------

Yes. And very useful!

What is it for?
---------------
For easy and sexy config files processing and easy config reloading support.

Why do I need it?
-----------------
Usually config files for Erlang software is just a bunch of Erlang terms.
It is hard to read, write, parse and validate. And sometimes you need
to handle config reloading. All this makes pain in ass and puts your code
in mess.
Nakaz uses YAML for config files (which is human-readable and clean),
-record specs as config schema definition, has schema validation and has
support for config reloading interface and callbacks.



Why the name?
-------------

This is the dumbest name we could come up with, which roughly
[translates] [1] to `mandate` from old-Russian.

[1]: http://translate.google.com/#ru|en|%D0%BD%D0%B0%D0%BA%D0%B0%D0%B7
