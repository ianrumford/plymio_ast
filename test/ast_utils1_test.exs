defmodule PlymioAstUtils1Test do

  use PlymioAstHelpersTest
  alias Plymio.Ast.Utils, as: PAU

  # Many of these test were prototypes for the doctests

  test "ast_validate!: 1" do

    helper_run_tests_ast_validate!(
      t: [
        1,
        :two,
        "tre",
        %{a: 1} |> Macro.escape,
        {1, :two, "tre"} |> Macro.escape,
        [%{a: 1}, {:e, ArgumentError}],
        [{1, :two, "tre"}, {:e, ArgumentError}],

        quote(do: use X1),
        quote(do: a = x + y),
        quote do
          def(abc(x,y,x), do: a = x + y)
         end ,

        :x |> Macro.var(nil),
        :x |> Macro.var(__MODULE__),
      ])

  end

  test "asts_reduce: eval" do

    binding = [x: 40, y: 2]

    {42, "a = x + y"} |> helper_assert_asts_reduce_eval(binding, quote(do: a = x + y))

    {2738, "(\na = x\nb = y\nc = a + b\nd = c - 5\ne = d * d\nf = e * y\n)"}
    |> helper_assert_asts_reduce_eval(binding,
    [ [],
      quote(do: a = x),
      quote(do: b = y),
      nil,
      quote(do: c = a + b),
      nil,
      [
        quote(do: d = c - 5),
        nil,
        quote(do: e = d * d),
      ],
      quote(do: f = e * y),
      [],
      nil
    ])

    {nil, "def(abc(x, y, x)) do\nx + y + x\nend"}
    |> helper_assert_asts_reduce_eval(nil,
    [ quote do
      def abc(x,y,x) do
        x + y + x
      end
    end])

    {nil, "def(abc(x, y, x)) do\nx + y(+z)\nend"}
    |> helper_assert_asts_reduce_eval(nil,
    quote(do: def(abc(x,y,x), do: x + y +z)))

  end

  test "asts_group: 1" do

     asts = quote(do: use X1)

     asts_actual = asts |> PAU.asts_group

     asts_expect = %{use: [asts]}

     assert asts_expect == asts_actual

   end

   test "asts_group: 2" do

     asts = [
       quote(do: use X1),
       quote(do: use X2),
       quote(do: use X3),
     ]

     asts_actual = asts |> PAU.asts_group

     asts_expect = %{use: asts}

     assert asts_expect == asts_actual

   end

   test "asts_group: 3" do

     asts = [
       quote(do: import X2),
       quote do
         alias X3, as: AliasX3
       end ,
       quote(do: require X1),
     ]

     asts_actual = asts |> PAU.asts_group

     asts_expect = %{

       require: [quote(do: require X1)],

       alias: [quote do
                alias X3, as: AliasX3
              end ],

       import: [quote(do: import X2)],

     }

     assert Map.equal?(asts_expect, asts_actual)

   end

   test "asts_sort: 1" do

     asts = quote(do: use X1)

     asts_sorted = asts |> PAU.asts_sort

     assert [asts] == asts_sorted

   end

   test "asts_sort: 2" do

     asts = quote(do: use X1)

     assert ["use(X1)"] =  asts |> helper_asts_sort_to_string

   end

   test "asts_sort: 3" do

     asts = nil

     assert [] =  asts |> helper_asts_sort_to_string

   end

   test "asts_sort: 4" do

     asts = [
       quote(do: use X1),
       quote(do: require X2),
       quote(do: import X3),
     ]

     assert ["import(X3)", "use(X1)", "require(X2)"] == asts |> helper_asts_sort_to_string(%{import: 1, use: 2})
end

  test "ast_from_mfa:" do

    "X1.f1()" |> helper_assert_ast_from_mfa({X1, :f1, []})

    "X2.f2(1, :two, \"tre\")" |> helper_assert_ast_from_mfa({X2, :f2, [1, :two, "tre"]})

    "X3.f3({1, :two, \"tre\"})" |> helper_assert_ast_from_mfa({X3, :f3, [{1, :two, "tre"}]})
    "X3.f3({1, :two, \"tre\"})" |> helper_assert_ast_from_mfa({X3, :f3, [{1, :two, "tre"} |> Macro.escape]})

    "X4.f4(%{a: 1, b: :two, c: \"tre\"})" |> helper_assert_ast_from_mfa({X4, :f4, [%{a: 1, b: :two, c: "tre"}]})
    "X4.f4(%{a: 1, b: :two, c: \"tre\"})" |> helper_assert_ast_from_mfa({X4, :f4, [%{a: 1, b: :two, c: "tre"} |> Macro.escape]})

    "X5.f5(1, {1, :two, \"tre\"}, %{a: 1, b: :two, c: \"tre\"})"
    |> helper_assert_ast_from_mfa(
      {X5, :f5,
       [1,
        {1, :two, "tre"},
        %{a: 1, b: :two, c: "tre"}
       ]})

    "X5.f5(1, {1, :two, \"tre\"}, %{a: 1, b: :two, c: \"tre\"})"
    |> helper_assert_ast_from_mfa(
      {X5, :f5,
       [1,
        {1, :two, "tre"} |> Macro.escape,
        %{a: 1, b: :two, c: "tre"} |> Macro.escape
       ]})

  end

end

