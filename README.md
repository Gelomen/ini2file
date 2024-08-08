# ini2file

中文 | [English](./README_EN.md)

## 简介

读取 `.ini` 配置和 `.tmpl` 模板批量生成文件, `.ini` 中每个 `session` 对应一个文件(`i2f_common_session` 除外)

## 使用

在 `rebar.config` 的 `plugins` 添加 `ini2file`

```erlang

{plugins, [
    {ini2file, {git, "https://github.com/Gelomen/ini2file.git", {branch, "master"}}}
]}.

```

### 直接执行生成

```bash
rebar3 ini2file gen
```

### 钩子自动生成

在 `rebar.config` 的 `provider_hooks` 添加配置

```erlang

{provider_hooks, [
    {pre, [
        {compile, {ini2file, gen}}
    ]}
]}.

```

然后执行

```bash
rebar3 compile
```

## 配置

读取 `.ini` 文件的字段, 替换模板的内容, 如:

> `domain` 的值 `127.0.0.1` 替换模板文件中的 `{{domain}}`

### .ini

路径: `ini/my_config.ini`

```ini
[i2f_common_session]
domain = 127.0.0.1

[foo]
port = 8080

[bar]
port = 9090

```

> `session` 可以配置与 `[i2f_common_session]` 相同的参数以覆盖公共配置, 且不会影响其他 `session`

如:

```ini
[i2f_common_session]
domain = 127.0.0.1

[foo]
domain = localhost
port = 8080

[bar]
port = 9090

```

### .tmpl

路径: `templates/erl.tmpl`

```tmpl
%% Plugin generated, do not edit
%% Generated by `ini2file`.

-module({{i2f_file_name}}).

-export([
    host/0
]).

host() ->
    "{{domain}}:{{port}}".

```

## rebar.config

```erlang

{plugins, [
    {ini2file, {git, "https://github.com/Gelomen/ini2file.git", {branch, "master"}}}
]}.

{ini2file, [
    % ini 配置
    {ini, [
        % {ini_name, ini_path, [tmpl_name]}
        {my, "ini/my_config.ini", [erl]}                            % {配置名字, 配置路径, [模板名字]}
    ]},
    % templates 配置
    {templates, [
        {erl, [
            % 文件名字, 如下配置将得到: `src/my_foo_xxx.erl` 和 `src/my_bar_xxx.erl`
            {file, [
                {name, [i2f_ini_name, i2f_session_name, "xxx"]},    % 要拼接文件名字的单词, atom | int | string | bit_string
                {suffix, ".erl"},                                   % 文件后缀
                {save_path, "src"}                                  % 文件保存目录
            ]},
            {tmpl_path, "templates/erl.tmpl"}                       % 模板路径
        ]}
    ]}
]}.

```

## 内置参数

| 参数                       | 说明                       | 例                    |
|:-------------------------|:-------------------------|:---------------------|
| `{{i2f_ini_name}}`       | `ini` 的配置名字              | `my`                 |
| `{{i2f_session_name}}`   | `.ini` 文件里的 `session` 名字 | `foo`                |
| `{{i2f_file_name}}`      | 要生成的文件名字(不带后缀)           | `my_foo_xxx`         |
| `{{i2f_file_full_name}}` | 要生成的文件完整名字(带后缀)          | `my_foo_xxx.erl`     |
| `{{i2f_file_path}}`      | 要生成的文件路径                 | `src/my_foo_xxx.erl` |
