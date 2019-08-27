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

-module(xdrc).

-export([file/2]).
-export([init/1]).

-import(
   erl_syntax,
   [ abstract/1,
     application/2,
     application/3,
     atom/1,
     attribute/2,
     binary/1,
     binary_field/3,
     case_expr/2,
     clause/3,
     cons/2,
     form_list/1,
     function/2,
     function_type/2,
     generator/2,
     infix_expr/3,
     integer/1,
     integer_range_type/2,
     list/1,
     list_comp/2,
     macro/1,
     map_expr/1,
     map_expr/2,
     map_field_assoc/2,
     map_field_exact/2,
     map_type/1,
     map_type_exact/2,
     match_expr/2,
     named_fun_expr/2,
     nil/0,
     operator/1,
     size_qualifier/2,
     tuple/1,
     tuple_type/1,
     type_application/2,
     type_union/1,
     underscore/0,
     variable/1
   ]).

init(S) ->
    {ok, rebar_state:prepend_compilers(S, [rebar3_compiler_xdr])}.

file(Name, Options) ->
    {ok, Bin} = file:read_file(Name),
    Base = filename:basename(Name, ".x"),
    Module = list_to_atom(Base),
    Dir = proplists:get_value(outdir, Options, filename:dirname(Name)),
    Filename = filename:join(Dir, [Base, ".erl"]),
    case parse(Bin, 1) of
        {ok, Tree} ->
            Forms = specification(Module, Tree),
            Erl = erl_prettypr:format(form_list(Forms), []),
            ok = file:write_file(Filename, Erl),
            {ok, Filename};
        Error ->
            Error
    end.

parse(String, Line) ->
    case xdr_lexer:string(unicode:characters_to_list(String), Line) of
        {ok, Tokens, _} ->
            xdr_parser:parse(Tokens);
        {error, Error, _} ->
            {error, Error}
    end.

specification(Module, Tree) ->
    [attribute(atom(module), [atom(filename:basename(Module))]),
     attribute(atom(compile), [atom(export_all)])
    ] ++ specification(Tree).

specification(Tree) ->
    Consts =
        [attribute(atom(define),[atom(Id), integer(Value)])
         || {const, Id, Value} <- Tree ],
    Types =
        definitions(
          [Declaration
           || {typedef, Declaration} <- Tree ]),

    Program =
        program(
          [Prog || {program, Prog} <- Tree]),

    Consts ++ Types ++ Program.

definitions(Types) ->
    Map = maps:from_list(Types),
    [ Form
      || Type <- Types,
         Form <- definition(Type, Map)].

definition(Declaration, Types) ->
    [attribute(
       atom(type),
       [abstract(type_declaration(Declaration, Types))]),
     attribute(
       atom(spec),
       [abstract(encode_spec(Declaration, Types))]),
     encode_fun(Declaration),
     attribute(
       atom(spec),
       [abstract(decode_spec(Declaration, Types))]),
     decode_fun(Declaration)].

type_declaration({Id, Spec}, Types) ->
    {Id, erl_syntax:revert(type_spec(Spec, Types)), []}.

bool() ->
    {enum, [{'FALSE', 0}, {'TRUE', 1}]}.

integer_min(unsigned, _) ->
    0;
integer_min(signed, Bits) ->
    0 - (1 bsl (Bits - 1)).

integer_range(Sign, Bits) ->
    Min = integer_min(Sign, Bits),
    {0 + Min, (1 bsl Bits) - 1 + Min}.

type_spec({_, T, _}, _) when T =:= opaque; T =:= string ->
    type_application(atom(binary), []);
type_spec({_, Spec, _}, Types) ->
    list([type_spec(Spec, Types)]);
type_spec({optional, Spec}, Types) ->
    type_union([type_spec(Spec, Types), atom(void)]);
type_spec(void, _) ->
    atom(void);
type_spec({Sign, Bits}, _)
  when Sign =:= signed; Sign =:= unsigned ->
    {First, Last} = integer_range(Sign, Bits),
    integer_range_type(integer(First), integer(Last));
type_spec({float, _}, _) ->
    type_application(atom(float), []);
type_spec(bool, Types) ->
    type_spec(bool(), Types);
type_spec({enum, Enumerators}, _) ->
    type_union(
      [atom(Name)
       || {Name, _} <- Enumerators ]);
type_spec({struct, Components}, Types) ->
    map_type(
      [map_type_exact(atom(Id), type_spec(Spec, Types))
       || {Id, Spec} <- Components ]);
type_spec({union, {_, Arms}}, Types) ->
    type_union(arms_spec(Arms, Types));
type_spec({union, {{_, Discriminant}, Arms, {_, Default}}}, Types) ->
    Matched = [Case || {Cases, _} <- Arms, Case <- Cases],
    DefaultCases = default_spec(Matched, resolve(Discriminant, Types)),
    type_union(
      arms_spec(Arms, Types)
      ++ [tuple_type(
            [type_union(DefaultCases),
             type_spec(Default, Types)])]);
type_spec(Id, _) when is_atom(Id) ->
    type_application(atom(Id), []).

resolve(bool, _) ->
    bool();
resolve(void, _) ->
    void;
resolve(Id, Types) when is_atom(Id) ->
    resolve(maps:get(Id, Types), Types);
resolve(Type, _) ->
    Type.

arms_spec(Arms, Types) ->
    [tuple_type(
       [type_union([literal(Case) || Case <- Cases]),
        type_spec(Spec, Types)])
     || {Cases, {_, Spec}} <- Arms].

default_spec(Matched, {enum, Enumerators}) ->
    S = sets:from_list(Matched),
    [atom(Name)
     || {Name,_} <- Enumerators,
        not sets:is_element(Name, S)];
default_spec(Matched, {Sign, Bits})
  when Sign =:= signed;
       Sign =:= unsigned ->
    List = lists:sort(Matched),
    Range = integer_range(Sign, Bits),
    [integer_range_type(integer(First), integer(Last))
     || {First, Last} <- delete_from_range(List, Range)].

delete_from_range([], Range) ->
    [Range];
delete_from_range([H|T], {H, Last}) ->
    delete_from_range(T, {H+1, Last});
delete_from_range([H|T], {First, Last})
  when H > First, H < Last ->
    [{First, H-1}|delete_from_range(T, {H+1, Last})].

encode_spec({Id, _}, Types) ->
    {{encode_name(Id), 1},
     [erl_syntax:revert(
        function_type(
          [type_spec(Id, Types)],
          type_application(atom(iodata), [])))]}.

encode_name(Id) ->
    list_to_atom("encode_" ++ atom_to_list(Id)).

encode_fun({Id, _}=Declaration) ->
    function(atom(encode_name(Id)), type_encode(Declaration)).

type_encode({Id, {Mod, T, _}=Declaration})
  when T =:= opaque;
       T =:= string ->
    V = var(Id),
    Length = application(atom(byte_size), [V]),
    Bin = [V, pad(Length)],
    [clause(
       [V],
       length_encode_guard(Length, Declaration),
       [list(
          case Mod of
              fixed -> Bin;
              variable -> [binary([length_field(Length)])|Bin]
          end)
       ])
    ];
type_encode({Id, {Mod, Spec, _}=Declaration}) ->
    A = var("A", Id),
    V = var(Id),
    Length = application(atom(length), [A]),
    List = list_comp(type_encode(V, {Id, Spec}), [generator(V, A)]),
    [clause(
       [A],
       length_encode_guard(Length, Declaration),
       case Mod of
           fixed -> [List];
           variable -> [list([binary([length_field(Length)]), List])]
       end
      )
    ];
type_encode({Id, {optional, Spec}}) ->
    V = var(Id),
    [clause([atom(void)], none, [binary([enum_field(integer(0))])]),
     clause(
       [V],
       none,
       [list([binary([enum_field(integer(1))]),
              type_encode(V, {Id, Spec})])])];
type_encode({Id, {Sign, Bits}})
  when Sign =:= signed; Sign =:= unsigned ->
    V = var(Id),
    {First, Last} = integer_range(Sign, Bits),
    [clause(
       [V],
       [infix_expr(integer(First), operator('=<'), V),
        infix_expr(V, operator('=<'), integer(Last))],
       [binary([binary_field(V, integer(Bits), [atom(Sign)])])])];
type_encode({Id, {float, Bits}}) ->
    V = var(Id),
    [clause(
       [V],
       none,
       [binary([binary_field(V, integer(Bits), [atom(float)])])])];
type_encode({Id, bool}) ->
    type_encode({Id, bool()});
type_encode({_, {enum, Enumerators}}) ->
    [clause(
       [atom(Name)],
       none,
       [binary([enum_field(integer(Value))])])
     || {Name, Value} <- Enumerators ];
type_encode({_, {struct, Components}}) ->
    [clause(
       [map_expr(
          [map_field_exact(atom(CI), var("C",CI))
           || {CI, _} <- Components])],
       none,
       [list(
          [type_encode(var("C",CI), Component)
           || {CI, _} = Component <- Components])])];
type_encode({_, {union, {{DI,_}=Discriminant, Arms}}}) ->
    D = var("C",DI),
    [clause(
       [tuple([D, var("C",CI)])],
       arm_guard(D, Cases),
       [list([type_encode(D, Discriminant),
              type_encode(var("C",CI), Component)])])
     || {Cases, {CI,_}=Component} <- Arms];
type_encode({Id, {union, {Discriminant, Arms, Default}}}) ->
    type_encode({Id, {union, {Discriminant, Arms ++[{[], Default}]}}});
type_encode({Id, Type}) when is_atom(Type) ->
    V = var(Id),
    [clause(
       [V],
       none,
       [application(atom(encode_name(Type)), [V])])].

type_encode(_, {void, void}) ->
    nil();
type_encode(Value, Declaration) ->
    case_expr(Value, type_encode(Declaration)).

enum_field(V) ->
    binary_field(V, integer(32), [atom(signed)]).

length_field(V) ->
    binary_field(V, integer(32), []).

length_encode_guard(Length, {Mod, _, N}) ->
    [infix_expr(
       Length,
       operator(
         case Mod of
             fixed -> '=:=';
             variable -> '=<'
         end),
       compound_size(N))].

arm_guard(_, []) ->
    none;
arm_guard(D, Cases) ->
    [[infix_expr(D, operator('=:='), literal(Case))]
     || Case <- Cases ].

compound_size(N) when is_integer(N) ->
    integer(N);
compound_size(A) when is_atom(A) ->
    macro(atom(A)).

pad_size(Size) ->
    infix_expr(
      infix_expr(
        integer(4),
        operator('-'),
        infix_expr(Size, operator('rem'), integer(4))),
      operator('rem'),
      integer(4)).

pad(Size) ->
    binary(
      [binary_field(
         integer(0),
         pad_size(Size),
         [size_qualifier(atom(unit), integer(8))])]).

decode_spec({Id, _}, Types) ->
    Binary = type_application(atom(binary), []),
    {{decode_name(Id), 1},
     [erl_syntax:revert(
        function_type(
          [Binary],
          type_union(
            [tuple_type([atom(ok), type_spec(Id, Types), Binary]),
             atom(error)]))
       )]}.

decode_name(Id) ->
    list_to_atom("decode_" ++ atom_to_list(Id)).

decode_fun({Id, _}=Declaration) ->
    function(atom(decode_name(Id)), type_decode(Declaration)).


type_decode({Id, {fixed, Spec, N}}) ->
    Bin = var("Z",Id),
    Length = compound_size(N),
    [clause(
       [Bin],
       none,
       case Spec of
           opaque -> binary_decode(Id, Length);
           string -> binary_decode(Id, Length);
           _ -> [application(array_decode({Id, Spec}), [Length, Bin])]
       end)];
type_decode({Id, {variable, Spec, N}}) ->
    Bin = var("Z",Id),
    Length = var("L",Id),
    Limit = compound_size(N),
    [clause(
       [binary(
          [length_field(Length),
           binary_field(Bin, none, [atom(binary)])])],
       [infix_expr(Length, operator('=<'), Limit)],
       case Spec of
           opaque -> binary_decode(Id, Length);
           string -> binary_decode(Id, Length);
           _ -> [application(array_decode({Id, Spec}), [Length, Bin])]
       end),
     error()];
type_decode({Id, {optional, Spec}}) ->
    Bin = var("B",Id),
    Rest = var("R",Id),
    [clause(
       [binary(
          [enum_field(integer(1)),
           binary_field(Bin, none, [atom(binary)])])],
       none,
       [type_decode(Bin, {Id, Spec})]),
     clause(
       [binary(
          [enum_field(integer(0)),
           binary_field(Rest, none, [atom(binary)])])],
       none,
       [ok(atom(void), Rest)]),
     error()];
type_decode({Id, {Sign, Bits}})
  when Sign =:= signed; Sign =:= unsigned ->
    V = var(Id),
    Rest = var("R",Id),
    [clause(
       [binary(
          [binary_field(V, integer(Bits), [atom(Sign)]),
           binary_field(Rest, none, [atom(binary)])])],
       none,
       [ok(V,Rest)]),
     error()];
type_decode({Id, {float, Bits}}) ->
    V = var(Id),
    Rest = var("R",Id),
    [clause(
       [binary(
          [binary_field(V, integer(Bits), [atom(float)]),
           binary_field(Rest, none, [atom(binary)])])],
       none,
       [ok(V, Rest)]),
     error()];
type_decode({Id, bool}) ->
    type_decode({Id, bool()});
type_decode({Id, {enum, Enumerators}}) ->
    Rest = var("R",Id),
    [clause(
       [binary(
          [enum_field(integer(Value)),
           binary_field(Rest, none, [atom(binary)])])],
       none,
       [ok(atom(Name), Rest)])
     || {Name, Value} <- Enumerators ] ++ [error()];
type_decode({Id, {struct, Components}}) ->
    Bin = var("B",Id),
    Map =
        map_expr(
          [map_field_assoc(atom(CI),var("C",CI))
           || {CI,_} <- Components]),
    [clause(
       [Bin],
       none,
       [component_decode(Bin, Components, Map)]
      )
    ];
type_decode({Id, {union, {Discriminant, Arms}}}) ->
    Bin = var("B", Id),
    [clause(
       [Bin],
       none,
       [case_expr(
          type_decode(Bin, Discriminant),
          [ arm_decode(Discriminant, Cases, Declaration)
           || {Cases, Declaration} <- Arms
          ] ++ [error()])
       ])
    ];
type_decode({Id, {union, {Discriminant, Arms, Default}}}) ->
    type_decode({Id, {union, {Discriminant, Arms ++[{[], Default}]}}});
type_decode({Id, Type}) when is_atom(Type) ->
    Bin = var("B",Id),
    [clause(
       [Bin],
       none,
       [application(atom(decode_name(Type)), [Bin])])].

type_decode(Bin, Declaration) ->
    case_expr(Bin, type_decode(Declaration)).

binary_decode(Id, Size) ->
    Bin = var("Z",Id),
    Rest = var("R",Id),
    Pad = var("P", Id),
    V = var(Id),
    [match_expr(Pad, pad_size(Size)),
     case_expr(
       Bin,
       [clause(
          [binary(
             [binary_field(V, Size, [atom(binary)]),
              binary_field(underscore(), Pad, [atom(binary)]),
              binary_field(Rest, none, [atom(binary)])])],
          none,
          [ok(V, Rest)]),
        error()])].

array_decode(Declaration = {Id, _}) ->
    Bin = var("B",Id),
    N = var("N", Id),
    E = var("E", Id),
    Rest = var("S", Id),
    A = var("A", Id),
    Rest1 = var("T", Id),
    Fun = var("F", Id),
    named_fun_expr(
      Fun,
      [clause([integer(0), Bin], none, [ok(nil(), Bin)]),
       clause(
         [N, Bin],
         none,
         [case_expr(
            type_decode(Bin, Declaration),
            [clause(
               [ok(E, Rest)],
               none,
               [case_expr(
                  application(Fun, [infix_expr(N, operator('-'), integer(1)), Rest]),
                  [clause(
                     [ok(A, Rest1)],
                     none,
                     [ok(cons(E, A), Rest1)]
                    ),
                   error()])]),
             error()])
         ])
      ]).

component_decode(Bin, [], Map) ->
    ok(Map, Bin);
component_decode(Bin, [{Id, _} = H|T], Map) ->
    V = var("C",Id),
    Rest = var("S",Id),
    case_expr(
      type_decode(Bin, H),
      [clause(
         [ok(V, Rest)],
         none,
         [component_decode(Rest, T, Map)]
        ),
       error()
      ]).

arm_decode({DI, _}, Cases, {CI, _} = Component) ->
    D = var("C", DI),
    C = var("C", CI),
    Rest = var("S", DI),
    Rest1 = var("S", CI),
    clause(
      [ok(D, Rest)],
      arm_guard(D, Cases),
      [case Component of
           {void, void} ->
               ok(tuple([D, atom(void)]), Rest);
           _ ->
               case_expr(
                 type_decode(Rest, Component),
                 [clause(
                    [ok(C, Rest1)],
                    none,
                    [ok(tuple([D,C]), Rest1)]),
                  error()])
       end]).

ok(Value, Rest) ->
    tuple([atom(ok), Value, Rest]).

error() ->
    clause([underscore()], none, [atom(error)]).

literal(Id) when is_atom(Id) ->
    atom(Id);
literal(Int) when is_integer(Int) ->
    integer(Int).

var(_, void) ->
    atom(void);
var(Prefix, Atom) when is_atom(Atom) ->
    variable(Prefix ++ atom_to_list(Atom)).

var(Atom) ->
    var("V", Atom).


program([]) ->
    [];
program(Programs) ->
    [function(
       atom(call),
       [ procedure(Prog, ProgNum, Version, VersionNum, Proc)
         || {Prog, Versions, ProgNum} <- Programs,
            {Version, Procs, VersionNum} <- Versions,
            Proc <- Procs
       ]
      ),
     function(
       atom(pmapproc_getport),
       [ getport(Prog, ProgNum, Version, VersionNum, Prot, ProtNum)
         || {Prog, Versions, ProgNum} <- Programs,
            {Version, _, VersionNum} <- Versions,
            {Prot, ProtNum} <- [{tcp, 6}, {udp, 17}]
       ])
    ].

procedure(Prog, ProgNum, Version, VersionNum, {Return, Proc, ArgTypes, ProcNum}) ->
    Client = variable("Client"),
    Reply = variable("Reply"),
    Payload = variable("Payload"),
    Result = variable("Result"),
    Index = lists:seq(1, length(ArgTypes)),
    clause(
      [Client, atom(Prog), atom(Version), atom(Proc), list([arg(I)||I<-Index])],
      none,
      [case_expr(
         application(
           atom(sunrpc_client),
           atom(call),
           [Client,
            integer(ProgNum),
            integer(VersionNum),
            integer(ProcNum),
            list(
              [ type_encode(arg(I), {varg(I), Type})
                || {I, Type} <- lists:zip(Index, ArgTypes)])]
          ),
         [clause(
            [tuple(
               [atom('ok'),
                Payload
               ])
            ],
            none,
            [case Return of
                 void ->
                     match_expr(Result, atom(void));
                 _ ->
                     match_expr(
                       tuple([atom(ok), Result, underscore()]),
                       type_decode(Payload, {result, Return})
                      )
             end,
             tuple(
               [atom('ok'),
                Result])
            ]),
          clause(
            [Reply],
            none,
            [Reply]
           )
         ]
        )
      ]).

varg(N) ->
    list_to_atom("V" ++ integer_to_list(N)).

arg(N) ->
    variable("A" ++ integer_to_list(N)).

getport(Prog, ProgNum, Version, VersionNum, Prot, ProtNum) ->
    Client = variable("Client"),
    clause(
      [Client, atom(Prog), atom(Version), atom(Prot)],
      none,
      [application(
         atom(pmap_prot),
         atom(call),
         [Client,
          atom('PMAP_PROG'),
          atom('PMAP_VERS'),
          atom('PMAPPROC_GETPORT'),
          list(
            [map_expr(
               [map_field_assoc(atom(prog), integer(ProgNum)),
                map_field_assoc(atom(vers), integer(VersionNum)),
                map_field_assoc(atom(prot), integer(ProtNum)),
                map_field_assoc(atom(port), integer(0))
               ]
              )
            ])
         ]
        )
      ]
     ).
