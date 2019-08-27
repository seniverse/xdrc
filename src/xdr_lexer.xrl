%% derived from IETF RFC4506 and RFC5531
Definitions.

Comment    = /[*]([^*]|[*][^/])*[*]/
Whitespace = [\000-\s]+
Keyword    = [(){}<>\[\]*,;=:]|bool|case|const|default|double|quadruple|enum|float|hyper|int|opaque|string|struct|switch|typedef|union|unsigned|void|program|version
Identifier = [A-Za-z][A-Za-z0-9_]*
Dec        = -?[1-9][0-9]*
Hex        = 0x[0-9a-fA-f]+
Oct        = 0[0-7]*

Rules.

{Comment}    : skip_token.
{Whitespace} : skip_token.
{Keyword}    : {token, {list_to_atom(TokenChars), TokenLine}}.
{Identifier} : {token, {i, TokenLine, list_to_atom(TokenChars)}}.
{Dec}        : {token, {c, TokenLine, list_to_integer(TokenChars, 10)}}.
{Hex}        : {token, {c, TokenLine, list_to_integer(tl(tl(TokenChars)), 16)}}.
{Oct}        : {token, {c, TokenLine, list_to_integer(TokenChars, 8)}}.

Erlang code.
