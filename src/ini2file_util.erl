-module(ini2file_util).
-include("ini2file.hrl").

-export([
    file_path/2,
    file_path/5,
    file_name/3,
    file_full_name/2,
    is_string/1,
    plugin_params_list/5
]).

%% @doc 文件路径
file_path(Path, FileFullName) ->
    filelib:ensure_dir(Path),
    filename:join(Path, FileFullName).
file_path(Path, IniName, SessionName, NameList, Suffix) ->
    FileName = file_name(IniName, SessionName, NameList),
    FileFullName = file_full_name(FileName, Suffix),
    filelib:ensure_dir(Path),
    filename:join(Path, FileFullName).

%% @doc 文件名字(不带后缀)
file_name(IniName, SessionName, NameList) ->
    lists:foldl(
        fun(Word, Acc) ->
            case word_transfer(Word, IniName, SessionName) of
                {ok, FirstWordStr} when Acc == "" -> FirstWordStr;
                {ok, WordStr} -> Acc ++ ?I2F_DELIMITER ++ WordStr;
                _ -> Acc
            end
        end, "", NameList).

%% @doc 文件完整名字(带后缀)
file_full_name(FileName, Suffix) ->
    case Suffix of
        [$. | _Tail] ->
            FileName ++ Suffix;
        [_ | _] ->
            FileName ++ "." ++ Suffix;
        <<$., _Tail/binary>> ->
            FileName ++ bitstring_to_list(Suffix);
        <<_/binary>> ->
            FileName ++ "." ++ bitstring_to_list(Suffix)
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

%% @doc 插件自带参数列表
plugin_params_list(IniName, SessionName, FileName, FileFullName, FilePath) ->
    [
        {?I2F_PARAM_KEY_INI_NAME, IniName},
        {?I2F_PARAM_KEY_SESSION_NAME, SessionName},
        {?I2F_PARAM_KEY_FILE_NAME, FileName},
        {?I2F_PARAM_KEY_FILE_FULL_NAME, FileFullName},
        {?I2F_PARAM_KEY_FILE_PATH, FilePath}
    ].

%% @doc 单词转换
word_transfer(Word, IniName, SessionName) ->
    case word_transfer_do(Word, IniName, SessionName) of
        WordStr when is_list(WordStr) ->
            {ok, WordStr};
        _ ->
            error
    end.
word_transfer_do(?I2F_PARAM_KEY_INI_NAME, IniName, _SessionName) -> atom_to_list(IniName);
word_transfer_do(?I2F_PARAM_KEY_SESSION_NAME, _IniName, SessionName) -> atom_to_list(SessionName);
word_transfer_do(Word, _IniName, _SessionName) when is_atom(Word) -> atom_to_list(Word);
word_transfer_do(Word, _IniName, _SessionName) when is_integer(Word) -> integer_to_list(Word);
word_transfer_do(Word, _IniName, _SessionName) when is_bitstring(Word) -> bitstring_to_list(Word);
word_transfer_do(Word, _IniName, _SessionName) when is_list(Word) -> Word;
word_transfer_do(_Word, _IniName, _SessionName) -> error.
