defmodule PlymioAstSignature1Test do

  use PlymioAstHelpersTest

  # Some of these test were prototypes for the doctests

  test "signature_create: runner 1" do

    helper_run_tests_signature1(
      t: [

        # explicit vars
        ["(arg0)", Macro.var(:arg0,nil)],
        ["(arg0)", [Macro.var(:arg0,nil)]],

        ["(arg0)", Macro.var(:arg0,__MODULE__)],
        ["(arg0)", [Macro.var(:arg0,__MODULE__)]],

        # no unquotables - just a var
        ["(arg0)", :arg0],
        ["(arg0)", [:arg0]],

        # w/ unquotables
        ["(uA_arg0)", :arg0, :opts_set1],
        ["(uA_arg0)", [:arg0], :opts_set1],

        ["(uB_arg0)", :arg0, :opts_set2],
        ["(uB_arg0)", [:arg0], :opts_set2],

        # w/ explicit default;  no unquotables
        ["(arg0 \\\\ 42)", {:arg0, 42}],
        ["(arg0 \\\\ %{a: 1, b: 2, c: 3})", {:arg0, %{a: 1, b: 2, c: 3}}],
        ["(arg0 \\\\ {1, :two, \"tre\"})", {:arg0, {1, :two, "tre"}}],

        # w/ var default;  no unquotables
        ["(arg0 \\\\ %{a: 1, b: 2, c: 3})", {:arg0, %{a: 1, b: 2, c: 3} |> Macro.escape}],
        ["(arg0 \\\\ [:a, 1, :b, 2, :c, 3])", {:arg0, [:a, 1, :b, 2, :c, 3] |> Macro.escape}],
        ["(arg0 \\\\ [:a, 2, :b, %{b: 2}, :c, {3, :tre, \"tre\"}])", {:arg0, [:a, 2, :b, %{b: 2}, :c, {3, :tre, "tre"}] |> Macro.escape}],
        ["(arg0 \\\\ {1, :two, \"tre\"})", {:arg0, {1, :two, "tre"} |> Macro.escape}],

        # w/ unquotables default
        ["(uA_arg0 \\\\ 42)", {:arg0, :default_42}, :opts_set1],
        ["(uA_arg0 \\\\ %{a: 1, b: 2, c: 3})", {:arg0, :default_map1}, :opts_set1],
        ["(uA_arg0 \\\\ {1, :two, \"tre\"})", {:arg0, :default_tuple1}, :opts_set1],
        # an atom default not in unquotables is just the atom
        ["(uA_arg0 \\\\ :default_unknown)", {:arg0, :default_unknown}, :opts_set1],

        # special defaults
        ["(uA_arg0 \\\\ nil)", {:arg0, nil}, :opts_set1],
        ["(uA_arg0 \\\\ true)", {:arg0, true}, :opts_set1],
        ["(uA_arg0 \\\\ false)", {:arg0, false}, :opts_set1],

        ["(uA_arg0 \\\\ %{})", {:arg0, :empty_map}, :opts_set1],
        ["(uA_arg0 \\\\ [])", {:arg0, :empty_list}, :opts_set1],
        ["(uA_arg0 \\\\ {})", {:arg0, :empty_tuple}, :opts_set1],

        # multiple args
        ["(arg0 \\\\ %{}, arg1 \\\\ [], arg2 \\\\ {})", [{:arg0, :empty_map}, {:arg1, :empty_list}, {:arg2, :empty_tuple}]],

        ["(uA_arg0 \\\\ %{}, uA_arg1 \\\\ [], arg3 \\\\ 42)", [{:arg0, :empty_map}, {:arg1, :empty_list}, {:arg3, 42}], :opts_set1],

        # w/ explicit module attribute default
        ["(arg0 \\\\ @default_attr1)", {:arg0, {:@, [], [{:default_attr1, [context: Elixir], Elixir}]}}],
        ["(arg0 \\\\ @default_attr2)", {:arg0, {:@, [], [{:default_attr2, [], nil}]}}],

      ])

  end

end

