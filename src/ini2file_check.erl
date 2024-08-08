%%%-------------------------------------------------------------------
%%% @doc
%%% ini2file 校验
%%% @end
%%%-------------------------------------------------------------------
-module(ini2file_check).
-include("ini2file.hrl").

%% API
-export([
    check/2
]).

%% @doc 配置校验
check(IniCfg, TemplatesCfg) ->
    IniCheck = ini_check(IniCfg, TemplatesCfg),
    TmplCheck = templates_check(TemplatesCfg),
    rebar_api:debug("Ini2file IniCheck: ~p, TmplCheck: ~p.", [IniCheck, TmplCheck]),
    IniCheck andalso TmplCheck.

%% @doc ini 配置校验
ini_check(IniCfg, TemplatesCfg) when is_list(IniCfg) ->
    rebar_api:debug("Ini2file check func: ~p, IniCfg: ~p, TemplatesCfg: ~p.", [?FUNCTION_NAME, IniCfg, TemplatesCfg]),
    lists:all(
        fun({IniName, IniPath, TmplNameList}) when is_list(TmplNameList) ->
            CheckList = [
                {fun check_cfg_name/2, [IniName, ?I2F_TYPE_INI], check_cfg_name},
                {fun check_path/3, [IniName, IniPath, ?I2F_TYPE_INI], check_path},
                {fun check_tmpl_name/3, [IniName, TmplNameList, TemplatesCfg], check_tmpl_name}
            ],
            loop_check(CheckList);
            ({IniName, _IniPath, TmplNameList}) ->
                rebar_api:error("Ini2file ini: ~p -> ~p must be of type list.", [IniName, TmplNameList]),
                false;
            (Term) ->
                rebar_api:error("Ini2file ini: ~p must be of type tuple: {ini_name, ini_path, [tmpl_name]}.", [Term]),
                false
        end, IniCfg);
ini_check(_IniCfg, _TemplatesCfg) ->
    rebar_api:error("Ini2file ini config must be of type list.", []),
    false.

%% @doc 模板配置校验
templates_check(TemplatesCfg) when is_list(TemplatesCfg) ->
    rebar_api:debug("Ini2file check func: ~p, TemplatesCfg: ~p.", [?FUNCTION_NAME, TemplatesCfg]),
    lists:all(
        fun({TmplName, TemplateCfg}) when is_list(TemplateCfg) ->
            CheckList = [
                {fun check_cfg_name/2, [TmplName, ?I2F_TYPE_TEMPLATES], check_cfg_name},
                {fun check_file/2, [TmplName, TemplateCfg], check_file},
                {fun check_tmpl_path/2, [TmplName, TemplateCfg], check_tmpl_path}
            ],
            loop_check(CheckList);
            ({TmplName, TemplateCfg}) ->
                rebar_api:error("Ini2file template: ~p -> ~p must be of type list.", [TmplName, TemplateCfg]),
                false;
            (Term) ->
                rebar_api:error("Ini2file template: ~p must be of type tuple: {tmpl_name, [tmpl_config]}.", [Term]),
                false
        end, TemplatesCfg);
templates_check(_TemplatesCfg) ->
    rebar_api:error("Ini2file templates config must be of type list.", []),
    false.

%% @doc 校验配置名字
check_cfg_name(Name, _Type) when is_atom(Name) ->
    true;
check_cfg_name(Name, Type) ->
    rebar_api:error("Ini2file ~p: ~p must be of type atom.", [Type, Name]),
    false.

%% @doc 校验模板名字
check_tmpl_name(_IniName, [], _TemplatesCfg) ->
    true;
check_tmpl_name(IniName, [TmplName | TmplNameList], TemplatesCfg) ->
    case lists:keymember(TmplName, 1, TemplatesCfg) of
        true ->
            check_tmpl_name(IniName, TmplNameList, TemplatesCfg);
        _ ->
            rebar_api:error("Ini2file ini: ~p -> ~p not in templates list.", [IniName, TmplName]),
            false
    end.

%% @doc 校验文件配置
check_file(TmplName, TemplateCfg) ->
    case lists:keyfind(?I2F_KEY_FILE, 1, TemplateCfg) of
        {?I2F_KEY_FILE, FileCfg} when is_list(FileCfg) ->
            CheckList = [
                {fun check_file_name/2, [TmplName, FileCfg], check_file_name},
                {fun check_suffix/2, [TmplName, FileCfg], check_suffix},
                {fun check_save_path/2, [TmplName, FileCfg], check_save_path}
            ],
            loop_check(CheckList);
        {?I2F_KEY_FILE, FileCfg} ->
            rebar_api:error("Ini2file template: ~p -> ~p must be of type list.", [TmplName, FileCfg]),
            false;
        _ ->
            rebar_api:error("Ini2file can't find template: ~p file config.", [TmplName]),
            false
    end.

%% @doc 校验文件名字
check_file_name(TmplName, FileCfg) ->
    case lists:keyfind(?I2F_KEY_NAME, 1, FileCfg) of
        {?I2F_KEY_NAME, NameList} when is_list(NameList) ->
            % 名字列表是否 为空 或者 里面的单词是否为 空字符串 或 带空格字符串
            case NameList == [] of
                true ->
                    rebar_api:error("Ini2file template: ~p -> name config is empty.", [TmplName]),
                    false;
                _ ->
                    CheckList = [{fun is_name_word_legal/2, [TmplName, Word], is_name_word_legal} || Word <- NameList],
                    loop_check(CheckList)
            end;
        {?I2F_KEY_NAME, NameList} ->
            rebar_api:error("Ini2file template: ~p -> ~p not a list type.", [TmplName, NameList]),
            false;
        _ ->
            rebar_api:error("Ini2file can't find template: ~p -> name config.", [TmplName]),
            false
    end.

%% @doc 校验后缀
check_suffix(TmplName, FileCfg) ->
    case lists:keyfind(?I2F_KEY_SUFFIX, 1, FileCfg) of
        {?I2F_KEY_SUFFIX, Suffix} ->
            IsString = is_string(Suffix),
            Regex = "^[a-zA-Z0-9.]+$",
            if
                IsString ->
                    case re:run(Suffix, Regex) of
                        {match, _} ->
                            true;
                        _ ->
                            rebar_api:error("Ini2file template: ~p -> ~p: ~p is illegal.", [TmplName, ?I2F_KEY_SUFFIX, Suffix]),
                            false
                    end;
                is_bitstring(Suffix) ->
                    case re:run(bitstring_to_list(Suffix), Regex) of
                        {match, _} ->
                            true;
                        _ ->
                            rebar_api:error("Ini2file template: ~p -> ~p: ~p is illegal.", [TmplName, ?I2F_KEY_SUFFIX, Suffix]),
                            false
                    end;
                true ->
                    rebar_api:error("Ini2file template: ~p -> ~p: ~p must be of types: string | bitstring.", [TmplName, ?I2F_KEY_SUFFIX, Suffix]),
                    false
            end;
        _ ->
            rebar_api:error("Ini2file can't find template: ~p -> ~p config.", [TmplName, ?I2F_KEY_SUFFIX]),
            false
    end.

%% @doc 校验路径
check_path(CfgName, Path, Type) ->
    IsString = is_string(Path),
    Regex = "^[a-zA-Z0-9_./-]*$",
    if
        IsString ->
            case re:run(Path, Regex) of
                {match, _} ->
                    true;
                _ ->
                    rebar_api:error("Ini2file ~p: ~p -> ~p is a illegal path.", [Type, CfgName, Path]),
                    false
            end;
        is_bitstring(Path) ->
            case re:run(bitstring_to_list(Path), Regex) of
                {match, _} ->
                    true;
                _ ->
                    rebar_api:error("Ini2file ~p: ~p -> ~p is a illegal path.", [Type, CfgName, Path]),
                    false
            end;
        true ->
            rebar_api:error("Ini2file ~p: ~p -> ~p must be of types: string | bitstring.", [Type, CfgName, Path]),
            false
    end.

%% @doc 校验保存路径
check_save_path(CfgName, Cfg) ->
    case lists:keyfind(?I2F_KEY_SAVE_PATH, 1, Cfg) of
        {?I2F_KEY_SAVE_PATH, Path} ->
            check_path(CfgName, Path, ?I2F_TYPE_TEMPLATES);
        _ ->
            rebar_api:error("Ini2file can't find templates: ~p -> ~p config.", [CfgName, ?I2F_KEY_SAVE_PATH]),
            false
    end.

%% @doc 校验模板路径
check_tmpl_path(CfgName, Cfg) ->
    case lists:keyfind(?I2F_KEY_TMPL_PATH, 1, Cfg) of
        {?I2F_KEY_TMPL_PATH, Path} ->
            check_path(CfgName, Path, ?I2F_TYPE_TEMPLATES);
        _ ->
            rebar_api:error("Ini2file can't find templates: ~p -> ~p config.", [CfgName, ?I2F_KEY_TMPL_PATH]),
            false
    end.

%% @doc 名字单词是否合法 atom | integer | string | bitstring
is_name_word_legal(TmplName, Word) ->
    IsString = is_string(Word),
    Regex = "^[a-zA-Z0-9_.-]+$",
    if
        IsString ->
            case re:run(Word, Regex) of
                {match, _} ->
                    true;
                _ ->
                    rebar_api:error("Ini2file template: ~p -> name: ~p is illegal.", [TmplName, Word]),
                    false
            end;
        is_bitstring(Word) ->
            case re:run(bitstring_to_list(Word), Regex) of
                {match, _} ->
                    true;
                _ ->
                    rebar_api:error("Ini2file template: ~p -> name: ~p is illegal.", [TmplName, Word]),
                    false
            end;
        is_atom(Word) ->
            true;
        is_integer(Word) ->
            true;
        true ->
            rebar_api:error("Ini2file template: ~p -> name words must be of types: atom | integer | string | bitstring.", [TmplName]),
            false
    end.

%% @doc 是否 string
is_string(List) when is_list(List) ->
    all_integer_ascii(List);
is_string(_Term) ->
    false.

%% @doc 列表内是否都是数字且在 ASCII 范围内
all_integer_ascii([]) -> true;
all_integer_ascii([H | T]) when is_integer(H), H >= 32, H =< 126 ->
    all_integer_ascii(T);
all_integer_ascii(_) -> false.

%% @doc 循环多个校验
loop_check([]) -> true;
loop_check([{Func, Args, FuncName} | CheckList]) ->
    case catch apply(Func, Args) of
        true ->
            rebar_api:debug("Ini2file func: ~p, args: ~p check success.", [FuncName, Args]),
            loop_check(CheckList);
        false ->
            rebar_api:debug("Ini2file func: ~p, args: ~p check fail.", [FuncName, Args]),
            false;
        Error ->
            rebar_api:error("Ini2file check error: ~p.", [Error]),
            false
    end.
