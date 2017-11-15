ExUnit.start()

defmodule PlymioAstHelpersTest do

  import ExUnit.Assertions
  alias Plymio.Ast.Utility, as: PAU

  def helper_run_tests_form1(opts \\ []) do

    test_mapper = fn

      [code_string, result] when is_binary(code_string) ->

        code_from_string = code_string |> Code.string_to_quoted!

        fun_deliver_value = fn _ -> code_from_string end

        deliver_result =
         case result do
           x when is_function(x) -> fn v -> result == v end
           x -> x
          end

      [c: :maybe_ast_realise, v: fun_deliver_value, r: deliver_result]

      [value] ->

        quoted_value = value |> Macro.escape

        [c: :maybe_ast_realise, v: quoted_value, r: value]

        test when is_tuple(test) -> test

    end

    [test_mapper: test_mapper,
     test_module: Plymio.Ast.Form,
    ] ++ opts
    |> Harnais.run_tests_default_test_value

  end

  def helper_asts_sort_to_string(asts, weights \\ nil) do
    asts
    |> PAU.asts_sort(weights)
    |> Enum.map(&Macro.to_string/1)
  end

  def helper_ast_validate!(actual) do
    assert actual == actual |> PAU.ast_validate!
    actual
  end

  def helper_ast_run_tests_validate!(opts \\ []) do

    test_mapper = fn

      [value, flag] ->

        [f: flag, c: :helper_ast_validate!, v: value, r: value]

      value ->

        [c: :helper_ast_validate!, v: value, r: value]

    end

    [
      test_mapper: test_mapper,
      test_module: __MODULE__,
    ] ++ opts
    |> Harnais.run_tests_default_test_value

  end

  defmacro __using__(_opts \\ []) do

    quote do
      use ExUnit.Case, async: true
      import PlymioAstHelpersTest
      import PlymioAstEvalHelper
      alias Plymio.Ast.Utility, as: PAU
    end

  end

end

