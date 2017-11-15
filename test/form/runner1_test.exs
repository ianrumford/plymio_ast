defmodule PlymioAstFormRunner1Test do

  use PlymioAstHelpersTest
  use Harnais.Attributes

  test "realise: 1" do

    helper_run_tests_form1(
      test_specs: [

        # basic values
        [42],
        ["string"],
        [:atom],
        [nil],

        # lists of basic values
        [[1, nil, :two, nil, "tre"]],
        [[1,2,3]],

        # maps
        [%{a: 1, b: 2, c: 3}],
        [%{a: %{a11: 11}}],
        [%{a: %{a11: %{a111: 111}}}],
        [%{a: %{a11: 11}, b: %{b21: 21, b22: 22}, c: %{c31: 31, c32: 32, c: %{c331: 331, c332: 332, c333: 333}}}],
        [@harnais_state_deep],

        # tuples
        [{1,2}],
        [{%{a: %{a11: 11}, b: %{b21: 21, b22: 22}, c: %{c31: 31, c32: 32, c: 33}}}],

        # list of tuples, maps, etc
        [[{1,2}, {3,4}]],

        [[%{a: 1, b: 2, c: 3}, %{x: 10, y: 11, z: 12}, %{p: 100, q: 101, r: 102}]],

        [[%{a: 1, b: 2, c: 3}, {1,2}, %{x: 10, y: 11, z: 12}, {3,4}, [1,2,3]]],

        {:r, :maybe_ast_realise_map, [], {:ok, %{a: 1, b: 2, c: 3}}, %{a: 1, b: 2, c: 3}},
        # keyword converted to a map
        {:r, :maybe_ast_realise_map, [], {:ok, %{a: 1, b: 2, c: 3}}, [a: 1, b: 2, c: 3]},
        {:r, :maybe_ast_realise_map, [], :error, 42},
        {:r, :maybe_ast_realise_map, [], :error, :atom},
        {:r, :maybe_ast_realise_map, [], :error, "string"},
        {:r, :maybe_ast_realise_map, [], :error, [1,2,3]},
        {:r, :maybe_ast_realise_map, [], :error, {1,2}},

        {:r, :maybe_ast_realise_map!, [], %{a: 1, b: 2, c: 3}, %{a: 1, b: 2, c: 3}},
        # keyword converted to a map
        {:r, :maybe_ast_realise_map!, [], %{a: 1, b: 2, c: 3}, [a: 1, b: 2, c: 3]},
        {{:e, BadMapError}, :maybe_ast_realise_map!, [], nil, 42},
        {{:e, BadMapError}, :maybe_ast_realise_map!, [], nil, :atom},
        {{:e, BadMapError}, :maybe_ast_realise_map!, [], nil, "string"},
        {{:e, BadMapError}, :maybe_ast_realise_map!, [], nil, [1,2,3]},
        {{:e, BadMapError}, :maybe_ast_realise_map!, [], nil, {1,2}},

      ])

  end

end
