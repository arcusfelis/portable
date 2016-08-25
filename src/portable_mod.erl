-module(portable_mod).
%% API
-export([copy_module_forms/2]).

-compile(debug_info).

copy_module_forms(Module, Node) ->
    Forms = get_module_forms(Module),
    Forms2 = delete_unwanted_forms(Forms),
    {ok, Module, Binary} = rpc:call(Node, compile, forms, [Forms2]),
    {module, Module} = rpc:call(Node, code, load_binary, [Module, "nofile", Binary]),
    ok.

get_module_forms(Module) ->
    Res = code:get_object_code(Module), % for binary
    get_module_forms_2(Module, Res).

get_module_forms_2(Module, {_, Beam, _}) ->
    Res = beam_lib:chunks(Beam, [abstract_code]),
    get_module_forms_3(Module, Res).

get_module_forms_3(Module, {ok,{_,[{abstract_code,{_,Forms}}]}}) ->
    Forms.

delete_unwanted_forms(Forms) ->
    lists:filter(fun is_wanted_form/1, Forms).

is_wanted_form(Form) ->
    not is_unwanted_attr_form(Form).

is_unwanted_attr_form(Form) ->
    is_attr_form(Form)
    andalso
    is_unwanted_attr_name(form_to_attr_name(Form)).

is_attr_form(Form) ->
    form_to_type(Form) =:= attribute.

is_unwanted_attr_name(AttrName) ->
    lists:member(AttrName, unwanted_attr_names()).

form_to_type(Form) ->
    element(1, Form).

form_to_attr_name(Form) ->
    element(3, Form).

unwanted_attr_names() ->
    [spec,
     export_type,
     type,
     opaque].
