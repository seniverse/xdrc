-module(xdrc_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [test_file, test_datatypes].

test_datatype(Type, Value) ->
    Encode = list_to_atom("encode_"++atom_to_list(Type)),
    Decode = list_to_atom("decode_"++atom_to_list(Type)),
    {ok, Value, <<>>} =
        datatypes_xdr:Decode(
          iolist_to_binary(datatypes_xdr:Encode(Value))).

test_datatypes(Config) ->
    Datadir = ?config(data_dir, Config),
    {ok, _} = xdrc:file(filename:join(Datadir, "datatypes_xdr.x"), []),
    {ok, datatypes_xdr} =
        compile:file(
          filename:join(Datadir, "datatypes_xdr.erl"),
          [{outdir, Datadir}]),
    {module, datatypes_xdr} = code:load_abs(filename:join(Datadir, "datatypes_xdr")),

    Tests =
        [{tuint, 16#80000000},
         {tint, -1},
         {tuhyper, 16#8000000000000000},
         {thyper, -1},
         {tfloat, 1.0},
         {tdouble, 1.0},
         {tbool, 'TRUE'},
         {tbool, 'FALSE'},
         {tarray, [1]},
         {tuarray, [1,2]},
         {tvarray, []},
         {tvarray, [1]},
         {topaque, <<1>>},
         {tuopaque, <<1,2>>},
         {tvopaque, <<>>},
         {tvopaque, <<1>>},
         {tustring, <<1,2>>},
         {tvstring, <<>>},
         {tvstring, <<1>>},
         {toptional, 1},
         {toptional, void},
         {tstruct, #{cint => 1}},
         {teunion, {'TRUE', 1}},
         {teunion, {'FALSE', void}},
         {tiunion, {1, 1}},
         {tiunion, {2, 2}},
         {tiunion, {3, void}},
         {talias, 1}
        ],

    [test_datatype(Type, Value)
     || {Type, Value} <- Tests],
    ok.

test_file(Config) ->
    Datadir = ?config(data_dir, Config),
    {ok, _} = xdrc:file(filename:join(Datadir, "file_xdr.x"), []),
    {ok, file_xdr} =
        compile:file(
          filename:join(Datadir, "file_xdr.erl"),
          [{outdir, Datadir}]),
    {module, file_xdr} = code:load_abs(filename:join(Datadir, "file_xdr")),

    HEX =
        <<"00 00 00 09 ",
          "73 69 6c 6c ",
          "79 70 72 6f ",
          "67 00 00 00 ",
          "00 00 00 02 ",
          "00 00 00 04 ",
          "6c 69 73 70 ",
          "00 00 00 04 ",
          "6a 6f 68 6e ",
          "00 00 00 06 ",
          "28 71 75 69 ",
          "74 29 00 00 "
        >>,

    Bin =
        list_to_binary(
          [binary_to_integer(X, 16)
           || X <- binary:split(HEX, <<" ">>, [global, trim])
          ]),

    File =
        #{filename => <<"sillyprog">>,
          type => {'EXEC', <<"lisp">>},
          owner => <<"john">>,
          data => <<"(quit)">>
         },

    Bin = iolist_to_binary(file_xdr:encode_file(File)),
    {ok, File, <<>>} = file_xdr:decode_file(Bin),
    ok.
