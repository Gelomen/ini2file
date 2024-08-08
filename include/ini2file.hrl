-ifndef(INI2FILE_H).
-define(INI2FILE_H, true).

%% @doc .ini 公共 session key
-define(I2F_INI_COMMON_SESSION_KEY, i2f_common_session).

%% @doc rebar.config 的 ini2file 配置类型
-define(I2F_TYPE_INI, ini).
-define(I2F_TYPE_TEMPLATES, templates).

%% @doc rebar.config 的 ini2file 配置下的 key
-define(I2F_KEY_FILE, file).
-define(I2F_KEY_NAME, name).
-define(I2F_KEY_SUFFIX, suffix).
-define(I2F_KEY_SAVE_PATH, save_path).
-define(I2F_KEY_TMPL_PATH, tmpl_path).

%% @doc ini2file 配置默认值
-define(I2F_DEFAULT_INI, []).
-define(I2F_DEFAULT_TEMPLATES, []).

%% @doc 分隔符
-define(I2F_DELIMITER, "_").

%% @doc 插件自带参数 key
-define(I2F_PARAM_KEY_INI_NAME, i2f_ini_name).
-define(I2F_PARAM_KEY_SESSION_NAME, i2f_session_name).
-define(I2F_PARAM_KEY_FILE_NAME, i2f_file_name).
-define(I2F_PARAM_KEY_FILE_FULL_NAME, i2f_file_full_name).
-define(I2F_PARAM_KEY_FILE_PATH, i2f_file_path).

-endif.
