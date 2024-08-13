-module(ini2file_prv_compile).
-include("ini2file.hrl").

-export([init/1, do/1, format_error/1]).

%% @doc 插件相关
-define(NAMESPACE, ini2file).
-define(PROVIDER, gen).
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
        {example, "rebar3 ini2file gen"},
        {opts, []},
        {short_desc, "Gen files .ini config."},
        {desc, "Generate files .ini config."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    generate(State),
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ========================================================================================
%%                                      内部 API
%% ========================================================================================

%% @doc 生成文件
generate(State) ->
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
                    generate(IniCfg, TemplatesCfg);
                _ ->
                    error
            end;
        _ ->
            rebar_api:debug("Ini2file can't get opts.", []),
            skip
    end.

generate([], _TemplatesCfg) ->
    ok;
generate([{IniName, IniPath, TmplNameList} | IniCfg], TemplatesCfg) ->
    try
        % 读取 .ini 文件
        {ok, IniContent} = zucchini:parse_file(IniPath),
        % 读取公共参数列表
        {CommonParams, NewIniContent} =
            case lists:keytake(?I2F_INI_SESSION_COMMON_KEY, 1, IniContent) of
                {value, {?I2F_INI_SESSION_COMMON_KEY, TempCommonParams}, OthersIniContent} ->
                    {TempCommonParams, OthersIniContent};
                _ ->
                    {[], IniContent}
            end,
        % 生成文件
        gen_by_cfg(IniName, CommonParams, NewIniContent, TmplNameList, TemplatesCfg),
        generate(IniCfg, TemplatesCfg)
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
            rebar_api:error("Ini2file generate error: ~p", [{Error, Stacktrace}]),
            error
    end;
generate([Term | _IniCfg], _TemplatesCfg) ->
    rebar_api:error("Ini2file generate error: ~p", [{error_ini_cfg, Term}]),
    {error_ini_cfg, Term}.

%% ========================================================================================
%%                                      生成文件
%% ========================================================================================

%% @doc 根据配置生成文件
gen_by_cfg(_IniName, _CommonParams, _IniContent, [], _TemplatesCfg) ->
    ok;
gen_by_cfg(IniName, CommonParams, IniContent, [TmplName | TmplNameList], TemplatesCfg) ->
    case lists:keyfind(TmplName, 1, TemplatesCfg) of
        {TmplName, Template} ->
            % 读取 生成文件名字和路径 配置
            NameList = proplists:get_value(?I2F_KEY_NAME, Template),
            Suffix = proplists:get_value(?I2F_KEY_SUFFIX, Template),
            Path = proplists:get_value(?I2F_KEY_SAVE_PATH, Template),
            % 读取 .tmpl 配置
            TmplPath = proplists:get_value(?I2F_KEY_TMPL_PATH, Template),
            gen_file(IniName, NameList, Suffix, Path, TmplPath, CommonParams, IniContent),
            gen_by_cfg(IniName, CommonParams, IniContent, TmplNameList, TemplatesCfg);
        _ ->
            rebar_api:error("Ini2file can't find template by name: ~p", [TmplName]),
            error_template_name
    end.

%% @doc 生成文件
gen_file(_IniName, _NameList, _Suffix, _Path, _TemplatePath, _CommonParams, []) ->
    ok;
gen_file(IniName, NameList, Suffix, Path, TemplatePath, CommonParams, [{SessionName, Params} | IniContent]) ->
    try
        FileName = ini2file_util:file_name(IniName, SessionName, NameList),
        FileFullName = ini2file_util:file_full_name(FileName, Suffix),
        FilePath = ini2file_util:file_path(Path, FileFullName),
        rebar_api:debug("Ini2file generating ~ts", [FilePath]),
        {ok, TemplateFileBin} = file:read_file(TemplatePath),
        Content = binary_to_list(TemplateFileBin),
        NewCommonParams = override_common_params(CommonParams, Params),
        PluginParamsList = ini2file_util:plugin_params_list(IniName, SessionName, FileName, FileFullName, FilePath),
        NewParams = NewCommonParams ++ Params ++ PluginParamsList,
        NewContent = replace_content(Content, NewParams),
        ok = file:write_file(FilePath, list_to_binary(NewContent)),
        gen_file(IniName, NameList, Suffix, Path, TemplatePath, CommonParams, IniContent)
    catch
        _Class:{badmatch, {error, enoent}}:_Stacktrace ->
            rebar_api:error("Ini2file template file: ~p not found.", [TemplatePath]),
            error_template_not_found;
        _Class:Error:Stacktrace ->
            rebar_api:error("Ini2file generate error: ~p", [{Error, Stacktrace}]),
            error
    end.

%% @doc 覆盖公共参数
override_common_params(CommonParams, []) ->
    CommonParams;
override_common_params(CommonParams, [{Key, _} | Params]) ->
    NewCommonParams =
        case lists:keymember(Key, 1, CommonParams) of
            true -> lists:keydelete(Key, 1, CommonParams);
            _ -> CommonParams
        end,
    override_common_params(NewCommonParams, Params).

%% @doc 替换内容
replace_content(Content, Params) ->
    lists:foldl(
        fun({Key, Value}, AccContent) ->
            KeyStr = io_lib:format("{{~p}}", [Key]),
            ValueStr =
                if
                    is_list(Value) -> Value;
                    is_bitstring(Value) -> bitstring_to_list(Value);
                    is_atom(Value) -> atom_to_list(Value);
                    is_integer(Value) -> integer_to_list(Value);
                    true -> error
                end,
            StringList = string:replace(AccContent, KeyStr, ValueStr, all),
            lists:foldl(fun(String, AccString) -> string:concat(AccString, String) end, "", StringList)
        end, Content, Params).
