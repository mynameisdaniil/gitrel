-module(gitrel_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gitrel).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
                               {name, ?PROVIDER},            % The 'user friendly' name of the task
                               {module, ?MODULE},            % The module implementation of the task
                               {bare, true},                 % The task can be run by the user, always true
                               {deps, ?DEPS},                % The list of dependencies
                               {example, "rebar3 gitrel"}, % How to use the plugin
                               {opts, []},                   % list of options understood by the plugin
                               {short_desc, "Create and upload GitHub release"},
                               {desc, "Create and upload GitHub release"}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  RelxConfig = rebar_state:get(State, relx, []),
  case rebar_state:get(State, gitrel, false) of
    false ->
      {error, {?MODULE, "Can't find relx release configuration"}};
    GitrelConfig ->
      Repo = proplists:get_value(repo, GitrelConfig),
      Url = io_lib:format("https://api.github.com/repos/~s/releases", [Repo]),
      io:format("URL:\t~s\n", [Url]),
      case lists:keyfind(release, 1, RelxConfig) of
        false ->
          {error, {?MODULE, "Can't find relx release configuration"}};
        {release, {App, Version}, _} ->
          io:format("App:\t~p\nVer:\t~s\n", [App, Version]),
          JSON = io_lib:format("{\"tag_name\": \"~s\", \"name\": \"~s ~s\"}", [Version, App, Version]),
          AuthString = io_lib:format("~s:~s", [proplists:get_value(user, GitrelConfig), proplists:get_value(token, GitrelConfig)]),
          AuthStringBase64 = base64:encode_to_string(lists:flatten(AuthString)),
          AuthHeader = io_lib:format("Basic ~s", [AuthStringBase64]),
          io:format("WWW-Authenticate: ~s\n", [AuthHeader]),
          {ok, Result} = httpc:request(post, {Url, [{"WWW-Authenticate", AuthHeader}, {"User-Agent", "GitRel Rebar plugin/Erlang httpc"}], "application/json", JSON}, [{timeout, 60*1000}, {ssl, [{verify, 0}]}], [{sync, true}]),
          io:format("Result: ~p", [Result]),
          {ok, State}
      end
  end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).
