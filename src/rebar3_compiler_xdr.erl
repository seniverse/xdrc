%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(rebar3_compiler_xdr).

-export(
   [context/1,
    needed_files/4,
    dependencies/3,
    compile/4,
    clean/2]).

context(AppInfo) ->
    Dir = filename:join([rebar_app_info:dir(AppInfo),"src"]),
    #{src_dirs => ["src"],
      include_dirs => [],
      src_ext => ".x",
      out_mappings => [{".erl", Dir}]
    }.

needed_files(_, FoundFiles, Mappings, AppInfo) ->
    Files =
        [Source
         || Source <- FoundFiles,
            rebar_compiler:needs_compile(Source, ".erl", Mappings)],
    Opts = rebar_opts:get(rebar_app_info:opts(AppInfo), xdrc_opts, []),
    {{[], Opts}, {Files, Opts}}.

dependencies(_, _, _) ->
    [].

compile(Source, [{".erl",OutDir}], _Config, Opts) ->
    case xdrc:file(Source, [{outdir, OutDir}|Opts]) of
        {ok, Filename} ->
            {ok, [Filename]};
        {error, Error} ->
            rebar_compiler:error_tuple(Source, [Error], [], Opts)
    end.

clean(Files, _AppInfo) ->
    rebar_file_utils:delete_each(
      [filename:join(filename:dirname(F), [filename:basename(F, ".x"), ".erl"])
       || F <- Files]).
