-module(escalus_stanza_SUITE).
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(eq(E, A), ?assertEqual(E, A)).

all() ->
    [nullary_snippet_to_xmlel,
     unary_snippet_to_xmlel,
     type_matrix_accepted,
     term_as_argument,
     attribute_as_argument].

%%
%% Tests
%%

nullary_snippet_to_xmlel(_) ->
    M = escalus_stanza,
    ?eq(#xmlel{name = <<"el">>}, M:from_xml("<el/>")),
    ?eq(#xmlel{name = <<"el">>}, M:from_xml(<<"<el/>">>)).

unary_snippet_to_xmlel(_) ->
    M = escalus_stanza,
    ?eq(#xmlel{name = <<"el">>,
               attrs = [{<<"attr">>, <<"value">>}]},
        M:from_template("<el attr='{{val}}'/>",
                        [{val, "value"}])).

type_matrix_accepted(_) ->
    M = escalus_stanza,
    Example = #xmlel{name = <<"el">>,
                     attrs = [{<<"attr">>, <<"value">>}]},
    ?eq(Example, M:from_template("<el attr='{{val}}'/>", [{val, "value"}])),
    ?eq(Example, M:from_template(<<"<el attr='{{val}}'/>">>, [{val, "value"}])),
    ?eq(Example, M:from_template(<<"<el attr='{{val}}'/>">>, [{val, <<"value">>}])),
    ?eq(Example, M:from_template("<el attr='{{val}}'/>", [{val, <<"value">>}])).

term_as_argument(_) ->
    M = escalus_stanza,
    Inner = #xmlel{name = <<"inner">>},
    Example = #xmlel{name = <<"outer">>,
                     children = [Inner]},
    ?eq(Example, M:from_template("<outer>{{{inner}}}</outer>", [{inner, Inner}])).

attribute_as_argument(_) ->
    M = escalus_stanza,
    Attr = {<<"name">>, <<"value">>},
    Example = #xmlel{name = <<"el">>,
                     attrs = [Attr]},
    ?eq(Example, M:from_template("<el {{attr}}/>", [{attr, Attr}])).
