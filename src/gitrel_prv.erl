-module(gitrel_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gitrel).
-define(DEPS, [tar]).

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
  Dir = rebar_state:dir(State),
  RelxConfig = rebar_state:get(State, relx, []),
  case rebar_state:get(State, gitrel, false) of
    false ->
      {error, {?MODULE, "Can't find relx release configuration"}};
    GitrelConfig ->
      Repo = proplists:get_value(repo, GitrelConfig),
      Url = io_lib:format("https://api.github.com/repos/~s/releases", [Repo]),
      case lists:keyfind(release, 1, RelxConfig) of
        false ->
          {error, {?MODULE, "Can't find relx release configuration"}};
        {release, {App, Version}, _} ->
          JSON = lists:flatten(io_lib:format("{\"tag_name\":\"~s\"}", [Version])),
          AuthString = io_lib:format("~s:~s", [proplists:get_value(user, GitrelConfig), proplists:get_value(token, GitrelConfig)]),
          AuthStringBase64 = base64:encode_to_string(lists:flatten(AuthString)),
          AuthHeader = io_lib:format("Basic ~s", [AuthStringBase64]),
          io:format("URL:\t~s\n", [Url]),
          io:format("App:\t~s\nVer:\t~s\n", [App, Version]),
          CreateReleaseResult = post(Url, AuthHeader, "application/json", JSON),
          case CreateReleaseResult of
            {ok, {{_, 201, _}, Headers, _}} ->
              ReleaseName = io_lib:format("~s-~s.tar.gz", [App, Version]),
              Uploads = re:replace(proplists:get_value("location", Headers), "api\.github\.com", "uploads.github.com"),
              UploadUrl = lists:flatten(io_lib:format("~s/assets?name=~s", [Uploads, http_uri:encode(ReleaseName)])),
              io:format("UploadUrl: ~p\n", [UploadUrl]),
              {ok, ReleaseTGZ} = file:read_file(filename:join([Dir, "_build/default/rel/", App, ReleaseName])),
              UploadReleaseResult = post(UploadUrl, AuthHeader, "application/gzip", ReleaseTGZ),
              case UploadReleaseResult of
                {ok, {{_, 201, _}, _, _}} ->
                  io:format("Release ~s successfully uploaded!\n", [ReleaseName]),
                  {ok, State};
                {ok, {{_, StatusCode, StatusMessage}}} ->
                  {error, {?MODULE, lists:flatten(io_lib:format("Can't upload assets to GitHub release: ~B/~s", [StatusCode, StatusMessage]))}};
                Reason ->
                  {error, {?MODULE, lists:flatten(io_lib:format("Can't upload assets to GitHub release: ~p", [Reason]))}}
              end;
            {ok, {{_, StatusCode, StatusMessage}, _, _}} ->
              {error, {?MODULE, lists:flatten(io_lib:format("Can't create GitHub release: ~B/~s", [StatusCode, StatusMessage]))}};
            Reason ->
              {error, {?MODULE, lists:flatten(io_lib:format("Can't create GitHub release: ~p", [Reason]))}}
          end
      end
  end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

post(Url, AuthHeader, ContentType, Body) ->
  httpc:request(post, {Url, [
                             {"Authorization", AuthHeader},
                             {"User-Agent", "GitRel Rebar plugin/Erlang httpc"}
                            ], ContentType, Body}, [{timeout, 60*1000}, {ssl, [{verify, 0}]}], [{sync, true}]).
