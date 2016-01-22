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

Then add following section to rebar config:


    {gitrel, [
              {user, "mynameisdaniil"},
              {token, "you_github_acess_token"},
              {repo, "mynameisdaniil/gitrel"}
             ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 gitrel
    ===> Fetching gitrel
    ===> Compiling gitrel
    <Plugin Output>
