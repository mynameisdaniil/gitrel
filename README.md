gitrel
=====

Create and upload GitHub releases

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { gitrel, ".*", {git, "git@host:user/gitrel.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 gitrel
    ===> Fetching gitrel
    ===> Compiling gitrel
    <Plugin Output>
