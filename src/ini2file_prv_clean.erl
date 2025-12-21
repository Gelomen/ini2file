-module(ini2file_prv_clean).
-include("ini2file.hrl").

-export([init/1, do/1, format_error/1]).

%% @doc 插件相关
-define(NAMESPACE, ini2file).
-define(PROVIDER, clean).
-define(DEPS, []).

%% ========================================================================================
%%                                      外部 API
%% ========================================================================================

init(State) ->
    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 ini2file clean"},
        {opts, []},
        {short_desc, "Generate sys.config and vm.args by .ini config."},
        {desc, "Generate sys.config and vm.args by .ini config."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    clean(State),
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ========================================================================================
%%                                      内部 API
%% ========================================================================================

%% @doc 清理生成的文件
clean(State) ->
    % 获取项目 rebar.config 的配置
    Opts = rebar_state:opts(State),
    case dict:find(?NAMESPACE, Opts) of
        {ok, Ini2fileOpts} ->
            rebar_api:debug("Ini2file get opts: ~p.", [Ini2fileOpts]),
            % ini 配置
            IniCfg = proplists:get_value(?I2F_TYPE_INI, Ini2fileOpts, ?I2F_DEFAULT_INI),
            % templates 配置
            TemplatesCfg = proplists:get_value(?I2F_TYPE_TEMPLATES, Ini2fileOpts, ?I2F_DEFAULT_TEMPLATES),
            % 校验配置
            case ini2file_check:check(IniCfg, TemplatesCfg) of
                true ->
                    clean(IniCfg, TemplatesCfg);
                _ ->
                    error
            end;
        _ ->
            rebar_api:debug("Ini2file can't get opts.", []),
            skip
    end.

clean([], _TemplatesCfg) ->
    ok;
clean([{IniName, IniPath, TmplNameList} | IniCfg], TemplatesCfg) ->
    try
        % 读取 .ini 文件
        {ok, IniContent} = zucchini:parse_file(IniPath),
        % 清理生成的文件
        clean_by_cfg(IniName, IniContent, TmplNameList, TemplatesCfg),
        clean(IniCfg, TemplatesCfg)
    catch
        _Class:{badmatch, {error, enoent}}:_Stacktrace ->
            rebar_api:error("Ini2file ini file: ~p not found.", [IniPath]),
            error_ini_not_found;
        _Class:{badmatch, {error, {LineNum, zucchini_parser, ["syntax error before: ", ErrorContent]}}}:_Stacktrace ->
            rebar_api:error(
                "Ini2file fail to parser ini file: ~p in LineNum: ~p, syntax error before: ~p.",
                [IniPath, LineNum, ErrorContent]
            ),
            error_parser_ini_file;
        _Class:{badmatch, {error, {LineNum, zucchini_lexer, {illegal, IllegalContent}}, _}}:_Stacktrace ->
            rebar_api:error(
                "Ini2file fail to parser ini file: ~p in LineNum: ~p, illegal: ~p.",
                [IniPath, LineNum, IllegalContent]
            ),
            error_parser_ini_file;
        _Class:Error:Stacktrace ->
            rebar_api:error("Ini2file clean error: ~p", [{Error, Stacktrace}]),
            error
    end;
clean([Term | _IniCfg], _TemplatesCfg) ->
    rebar_api:error("Ini2file clean error: ~p", [{error_ini_cfg, Term}]),
    {error_ini_cfg, Term}.

%% ========================================================================================
%%                                      生成文件
%% ========================================================================================

%% @doc 根据配置清理生成的 sys.config 和 vm.args 文件
clean_by_cfg(_IniName, _IniContent, [], _TemplatesCfg) ->
    ok;
clean_by_cfg(IniName, IniContent, [TmplName | TmplNameList], TemplatesCfg) ->
    case lists:keyfind(TmplName, 1, TemplatesCfg) of
        {TmplName, Template} ->
            % 读取 文件名字 规则
            NameList = proplists:get_value(?I2F_KEY_NAME, Template),
            Suffix = proplists:get_value(?I2F_KEY_SUFFIX, Template),
            Path = proplists:get_value(?I2F_KEY_SAVE_PATH, Template),
            clean_file(IniName, NameList, Suffix, Path, IniContent),
            clean_by_cfg(IniName, IniContent, TmplNameList, TemplatesCfg);
        _ ->
            skip
    end.

%% @doc 清理生成的文件
clean_file(_IniName, _NameList, _Suffix, _Path, []) ->
    ok;
clean_file(IniName, NameList, Suffix, Path, [{SessionName, _Params} | IniContent]) ->
    FilePath = ini2file_util:file_path(Path, IniName, SessionName, NameList, Suffix),
    rebar_api:debug("Ini2file deleting ~ts", [FilePath]),
    file:delete(FilePath),
    clean_file(IniName, NameList, Suffix, Path, IniContent).
