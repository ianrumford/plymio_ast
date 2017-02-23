defmodule PlymioAstTransform1Test do

  use PlymioAstHelpersTest

  # Many of these test were prototypes for the doctests

  test "pipe_before: singleton ast" do

    helper_assert_transform_eval(
      43,
      [x: 42],
      Macro.var(:x, nil),
      pipe_before: quote(do: fn v -> v + 1 end.())
    )

  end

  test "pipe_before: singleton ast with index" do

     helper_assert_transform_eval(
      58,
      [x: 42, y: 99],
      Macro.var(:x, nil),
      pipe_before: {quote(do: Kernel.-(100)), 1}
     )

  end

  test "pipe_before: multiple asts" do

    ast1 = quote(do: fn v -> v + 1 end.())

    ast2 = quote(do: fn v -> v * v end.())

    ast3 = quote(do: fn v -> 1 - v end.())

    helper_assert_transform_eval(
      -1848,
      [x: 42],
      Macro.var(:x, nil),
      pipe_before: [ast1, ast2, ast3]
    )

  end

  test "pipe_before: multiple asts with indicies" do

    helper_assert_transform_eval(
      12.0,
      [x: 7],
      Macro.var(:x, nil),
      pipe_before: [

        {quote(do: Kernel./(42)), 1},
        quote(do: List.wrap),
        {quote(do: Kernel.++([1,2,3])), 1},
        {quote(do: Enum.reduce([&Enum.sum/1, ], fn f, s -> f.(s) end)), 1}

      ])

  end

  test "pipe_after: singleton ast" do

    helper_assert_transform_eval(
      43,
      [x: 42],
      quote(do: fn v -> v + 1 end.()),
      pipe_after: Macro.var(:x, nil)
    )

  end

  test "pipe_after: multiple asts" do

    helper_assert_transform_eval(
      -1848,
      [x: 42],
      quote(do: fn v -> 1 - v end.()),
      pipe_after: [
        Macro.var(:x, nil),
        quote(do: fn v -> v + 1 end.()),
        quote(do: fn v -> v * v end.()),
      ]
    )

  end

  test "postwalk: single ast" do

    ast = quote  do
      x = x + x
      x = x * x
      x = x - x
    end

    {^ast, acc} = ast
    |> helper_transform(
      postwalk:
      {0, fn
        {:x, [], PlymioAstTransform1Test} = ast, acc -> {ast, acc + 1}
        ast, acc ->

        {ast, acc}
      end}

    )

    assert 9 == acc

  end

  test "transform: single ast" do

    ast = quote  do
      x = x + x
      x = x * x
      x = x - x
    end

    ast = ast
    |> helper_transform(
      postwalk:
      {0, fn
        {:x, [], PlymioAstTransform1Test} = ast, acc -> {ast, acc + 1}
        ast, acc ->

        {ast, acc}
      end},

      transform: fn {ast, _acc} -> ast end

    )

    str = ast
    |> Macro.to_string
    |> fn str -> Regex.replace(~r/\n\s*/, str, "|") end.()

    str <> <<0>>

    assert "(|x = x + x|x = x * x|x = x - x|)" = str

  end

  test "splice: signature" do

    arg0_ast = quote(do: a)
    arg1_ast = quote(do: b \\ 9)
    arg2_ast = quote(do: c \\ 42)

    signature_ast = [arg0_ast, arg1_ast, arg2_ast]

    ast = quote do
      def fun1() do
        a + b + c
      end
    end

    fun1_ast = ast
    |> helper_transform(postwalk:
    fn

      {:fun1, _, _} = fun1_ast ->

        fun1_ast |> helper_transform(splice: signature_ast)

      # passthru
      x -> x

    end)

    # create a module with the function ast
    quote do
      defmodule PlymioAstTransformModuleA1Test do
        unquote(fun1_ast)
      end
    end
    |> Code.eval_quoted([], __ENV__)

    assert 6 == PlymioAstTransform1Test.PlymioAstTransformModuleA1Test.fun1(1,2,3)
    assert 45 == PlymioAstTransform1Test.PlymioAstTransformModuleA1Test.fun1(1,2)
    assert 52 == PlymioAstTransform1Test.PlymioAstTransformModuleA1Test.fun1(1)

  end

end
