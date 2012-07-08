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

Ho can I use it?
----------------

(hopefuly) the usage of `nakaz` is pretty straighforward, though you
still have to keep in mind **two** things:

* `nakaz` uses [YAML] [1] as base configuration format;
* `nakaz` requires you to structure your [YAML] [1] config in a *special*
  way, which is described bellow.

The basic configuration unit in `nakaz` is a **section**, which is
represented as a **named** [mapping] [2] on the YAML side. Each
configured application can have one or more sections, for example:

```yaml
example:
  log_conf:
    log: "priv/log.txt"
    severity: debug
```

Here, [example] [3] application defines a single section, named
`log_conf`. As you might have noticed, applications are defined
on the **top level** of the configuration file, with sections,
residing on the **second level**.

[1]: http://www.yaml.org
[2]: http://en.wikipedia.org/wiki/YAML#Associative_arrays
[3]: https://github.com/Spawnfest2012/holybrolly-nakaz/blob/master/example/priv/conf.yaml

Why the name?
-------------

This is the dumbest name we could come up with, which roughly
[translates] [1] to `mandate` from old-Russian.

[1]: http://translate.google.com/#ru|en|%D0%BD%D0%B0%D0%BA%D0%B0%D0%B7
