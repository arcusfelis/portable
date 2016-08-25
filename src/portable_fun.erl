-module(portable_fun).
%% API
-export([call/2,
         call/3]).

%% private
-export([eval_and_apply/3]).

%% Examples
-export([plus_one_f/0,
         plus_f/1]).

-compile(debug_info).

plus_one_f() ->
    fun(Y) -> Y + 1 end.

plus_f(X) ->
    fun(Y) -> X + Y end.

call(Node, Fun) when is_atom(Node), is_function(Fun, 0) ->
    call(Node, Fun, []).

call(Node, Fun, Args) when is_atom(Node), is_function(Fun, length(Args)), is_list(Args), node() =:= Node ->
    apply_locally(Fun, Args);
call(Node, Fun, Args) when is_atom(Node), is_function(Fun, length(Args)), is_list(Args) ->
    Info = erlang:fun_info(Fun),
    FunType = proplists:get_value(type, Info),
    call_2(FunType, Node, Fun, Info, Args).

call_2(external, Node, Fun, Info, Args) ->
    call_external_fun(Node, Fun, Args);
call_2(local, Node, Fun, Info, Args) ->
    maybe_copy_module_forms(Node, ?MODULE),
    Module = proplists:get_value(module, Info),
    FunName = proplists:get_value(name, Info),
    Forms = module_forms(Module),
    {ParentFunName, ParentArity, FunPos} = split_function_name(FunName),
    {ok, ParentFunForms} = find_function(ParentFunName, ParentArity, Forms),
    SubFlattenFunForms = flatten_funs(ParentFunForms),
    FunForms = find_sub_function(FunPos, SubFlattenFunForms),
    FreeVals = get_free_vals(Info),
    FunSrc = erl_prettypr:format(FunForms) ++ ".",
    FreeVars = get_free_var_names(FunForms),
    Vars = lists:zip(FreeVars, FreeVals),
    remote_eval_and_apply(Node, FunSrc, Vars, Args).

remote_eval_and_apply(Node, FunSrc, Vars, Args) ->
    rpc:call(Node, ?MODULE, eval_and_apply, [FunSrc, Vars, Args]).

eval_and_apply(FunSrc, Vars, Args) ->
    Fun = eval(FunSrc, Vars),
    erlang:apply(Fun, Args).

eval(FunSrc, Vars) ->
    {ok, Tokens, _} = erl_scan:string(FunSrc),
    {ok, [Form]} = erl_parse:parse_exprs(Tokens),
    Bindings = bindings_from_list(Vars),
    {value, Value, _} = erl_eval:expr(Form, Bindings),
    Value.

%% Multible add_binding/3
bindings_from_list(BindingList) ->
    Bindings = erl_eval:new_bindings(),
    bindings_from_list(BindingList, Bindings).

bindings_from_list([{Name,Value}|BindingList], Bindings) ->
    Bindings2 = erl_eval:add_binding(Name, Value, Bindings),
    bindings_from_list(BindingList, Bindings2);
bindings_from_list([], Bindings) ->
    Bindings.

get_free_vals(Info) ->
    proplists:get_value(env, Info).

%% fun M:F/A
call_external_fun(Node, Fun, Args) ->
    rpc:call(Node, erlang, apply, [Fun, Args]).

find_sub_function(FunPos, FlattenFuns) ->
    lists:nth(FunPos, FlattenFuns).

module_forms(Module) ->
    Beam = code:which(Module),
    {ok,{_,[{abstract_code,{_,Forms}}]}} = beam_lib:chunks(Beam, [abstract_code]),
    Forms.

find_function(FunName, FunArity, [Form|Forms]) ->
    case erl_syntax:type(Form) of
        function ->
            CurrentFunName = erl_syntax:atom_value(erl_syntax:function_name(Form)),
            CurrentFunArity = erl_syntax:function_arity(Form),
            case {CurrentFunName, CurrentFunArity} of
                {FunName, FunArity} ->
                    {ok, Form};
                _ ->
                    find_function(FunName, FunArity, Forms)
            end;
        _ ->
            find_function(FunName, FunArity, Forms)
    end;
find_function(_FunName, _FunArity, []) ->
    {error, not_found}.

flatten_funs(Tree) ->
    Acc = erl_syntax_lib:fold(fun flatten_funs_visitor/2, [], Tree),
    lists:reverse(Acc).

flatten_funs_visitor(Form, Acc) ->
    Type = erl_syntax:type(Form),
    case Type of
        fun_expr ->
            [Form|Acc];
        _ ->
            Acc
    end.

%% From https://github.com/saleyn/util/blob/master/src/decompiler.erl
split_function_name(FunName) ->
    [Fs, As, _, Rs] = string:tokens(atom_to_list(FunName), "-/"),
    {list_to_atom(Fs), list_to_integer(As), list_to_integer(Rs)+1}.


get_free_var_names(Form) ->
    AnnForm = erl_syntax_lib:annotate_bindings(Form, []),
    Anns = erl_syntax:get_ann(AnnForm),
    proplists:get_value(free, Anns).

maybe_copy_module_forms(Node, Module) ->
    Same = compare_module_versions(Node, Module),
    maybe_copy_module_forms(Same, Node, Module).

maybe_copy_module_forms(_Same=true, Node, Module) ->
    ok;
maybe_copy_module_forms(_Same=false, Node, Module) ->
    copy_module_forms(Node, Module).

compare_module_versions(Node, Module) ->
    Module:module_info(attributes) =:=
    rpc:call(Node, Module, module_info, [attributes]).

copy_module_forms(Node, Module) ->
    portable_mod:copy_module_forms(Module, Node). % different order of args

apply_locally(Fun, Args) ->
    erlang:apply(Fun, Args).
