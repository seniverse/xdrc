%% derived from IETF RFC4506 and RFC5531
Nonterminals identifier constant declaration type_specifier value enum_body enum_fields enum_field struct_body struct_fields struct_field union_body case_specs case_spec case_values case_value definition specification version_defs version_def proc_defs proc_def proc_return type_specifiers.
Terminals i c '[' ']' '<' '>' '{' '}' '(' ')' '*' ',' '=' ';' ':' bool case const default double quadruple enum float hyper int opaque string struct switch typedef union unsigned void program version.
Rootsymbol specification.

identifier -> i:
    element(3, '$1').

constant -> c:
    element(3, '$1').

declaration -> type_specifier identifier:
    {'$2', '$1'}.

declaration -> type_specifier identifier '[' value ']':
    {'$2', {fixed, '$1', '$4'}}.

declaration -> type_specifier identifier '<' '>':
    {'$2', {variable, '$1', 16#FFFFFFFF}}.

declaration -> type_specifier identifier '<' value '>':
    {'$2', {variable, '$1', '$4'}}.

declaration -> opaque identifier '[' value ']':
    {'$2', {fixed, opaque, '$4'}}.

declaration -> opaque identifier '<' '>':
    {'$2', {variable, opaque, 16#FFFFFFFF}}.

declaration -> opaque identifier '<' value '>':
    {'$2', {variable, opaque, '$4'}}.

declaration -> string identifier '<' '>':
    {'$2', {variable, string, 16#FFFFFFFF}}.

declaration -> string identifier '<' value '>':
    {'$2', {variable, string, '$4'}}.

declaration -> type_specifier '*' identifier:
    {'$3', {optional, '$1'}}.

declaration -> void:
    {void, void}.

value -> constant:
    '$1'.

value -> identifier:
    '$1'.

type_specifier -> unsigned int:
    {unsigned, 32}.

type_specifier -> int:
    {signed, 32}.

type_specifier -> unsigned hyper:
    {unsigned, 64}.

type_specifier -> hyper:
    {signed, 64}.

type_specifier -> float:
    {float, 32}.

type_specifier -> double:
    {float, 64}.

type_specifier -> quadruple:
    {float, 128}.

type_specifier -> bool:
    bool.

type_specifier -> enum enum_body:
    {enum, '$2'}.

type_specifier -> struct struct_body:
    {struct, '$2'}.

type_specifier -> union union_body:
    {union, '$2'}.

type_specifier -> identifier:
    '$1'.

enum_body -> '{' enum_fields '}':
    '$2'.

enum_fields -> enum_fields ',' enum_field:
    '$1' ++ ['$3'].

enum_fields -> enum_field:
    ['$1'].

enum_field -> identifier '=' value:
    {'$1', '$3'}.

struct_body -> '{' struct_fields '}':
    '$2'.

struct_fields -> struct_fields struct_field:
    '$1' ++ ['$2'].

struct_fields -> struct_field:
    ['$1'].

struct_field -> declaration ';':
    '$1'.

union_body -> switch '(' declaration ')' '{' case_specs '}':
    {'$3', '$6'}.

union_body -> switch '(' declaration ')' '{' case_specs default ':' declaration ';' '}':
    {'$3', '$6', '$9'}.

case_specs -> case_specs case_spec:
    '$1' ++ ['$2'].

case_specs -> case_spec:
    ['$1'].

case_spec -> case_values declaration ';':
    {'$1', '$2'}.

case_values -> case_values case_value:
    '$1' ++ ['$2'].

case_values -> case_value:
    ['$1'].

case_value -> case value ':':
    '$2'.

proc_return -> type_specifier:
    '$1'.

proc_return -> void:
    void.

type_specifiers -> type_specifiers ',' type_specifier:
    '$1' ++ ['$3'].

type_specifiers -> type_specifier:
    ['$1'].

proc_def -> proc_return identifier '(' void ')' '=' constant ';':
    {'$1', '$2', [], '$7'}.

proc_def -> proc_return identifier '(' type_specifiers ')' '=' constant ';':
    {'$1', '$2', '$4', '$7'}.

proc_defs -> proc_defs proc_def:
    '$1' ++ ['$2'].

proc_defs -> proc_def:
    ['$1'].

version_def -> version identifier '{' proc_defs '}' '=' constant ';':
    {'$2', '$4', '$7'}.

version_defs -> version_defs version_def:
    '$1' ++ ['$2'].

version_defs -> version_def:
    ['$1'].

definition -> const identifier '=' constant ';':
    {const, '$2', '$4'}.

definition -> typedef declaration ';':
    {typedef, '$2'}.

definition -> enum identifier enum_body ';':
    {typedef, {'$2', {enum, '$3'}}}.

definition -> struct identifier struct_body ';':
    {typedef, {'$2', {struct, '$3'}}}.

definition -> union identifier union_body ';':
    {typedef, {'$2', {union, '$3'}}}.

definition -> program identifier '{' version_defs '}' '=' constant ';':
    {program, {'$2', '$4', '$7'}}.

specification -> '$empty':
    [].

specification -> specification definition:
    '$1' ++ ['$2'].

Erlang code.
