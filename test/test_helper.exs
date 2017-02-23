ExUnit.start()

defmodule PlymioAstHelpersTest do

  import ExUnit.Assertions
  alias Plymio.Ast.Transform, as: PAT
  alias Plymio.Ast.Utils, as: PAU
  alias Plymio.Ast.Signature, as: PAS

  def helper_ast_eval(binding, ast, opts \\ [])

  def helper_ast_eval(_binding, nil, _opts) do
    {nil, ""}
  end

  def helper_ast_eval(nil, ast, _opts) do
    {nil, ast |> Macro.to_string}
  end

  def helper_ast_eval(binding, ast, _opts) do

    result = ast
    # ensure all vars are not associated with a module
    |> Macro.postwalk(fn
      {name, [], mod} when is_atom(name) and is_atom(mod) -> Macro.var(name, nil)
      x -> x
    end)
    |> fn ast ->

      # paranoia
      :ok = ast |> Macro.validate

      case binding do

        x when is_nil(x) -> {:error, :no_binding}

        _ ->

          ast
          |> Code.eval_quoted(binding, __ENV__)
          |> elem(0)

      end

    end.()

    {result, ast |> Macro.to_string}

  end

  @code_edits1 %{

    ~r/ case / => " case() "

  }

  @code_edits_final %{

    ~r/\-\>\n/ => "-> ",
    ~r/\n\|\>/ => " |>",

  }

  defp helper_code_edits(code, edits) when is_binary(code) do

    edits
    |> Enum.reduce(code, fn {r,v}, s ->
      Regex.replace(r, s, v)
    end)

  end

  def helper_code_clean(code) when is_binary(code) do

    code
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.map(fn str ->

      str |> helper_code_edits(@code_edits1)

    end)
    |> Enum.reject(fn str -> String.length(str) == 0 end)
    |> Enum.join("\n")
    |> fn code ->
      code |> helper_code_edits(@code_edits_final)
    end.()

  end

  def helper_code_compare(actual_code, expect_code) do

    actual_code = actual_code |> helper_code_clean
    expect_code = expect_code |> helper_code_clean

    case Kernel.==(actual_code, expect_code) do

      true -> {:ok, actual_code}

      _ ->

        {:error, :no_match, {expect_code, actual_code}}
    end

  end

  def helper_assert_code_compare(actual_code, expect_code) do
    assert {:ok, code} = helper_code_compare(actual_code, expect_code)
    code
  end

  def helper_asts_transform_eval(binding, fun_transform, asts \\ []) do

    actual_ast = asts |> fun_transform.()

    {actual_result, actual_code} = binding |> helper_ast_eval(actual_ast)

    # code before ast as easier to see with trace
    {actual_result, actual_code, actual_ast}

  end

  def helper_assert_asts_transform_eval({expect_result, expect_code}, binding, fun_transform, value \\ []) do

    {actual_result, actual_code, _actual_ast} = binding
    |> helper_asts_transform_eval(fun_transform, value)

    actual_code = helper_assert_code_compare(actual_code, expect_code)

    assert expect_result == actual_result

    {actual_result, actual_code}

  end

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

  def helper_transform(ast, opts \\ [])

  def helper_transform(ast, opts) do

    ast
    |> PAT.transform(opts)

  end

  def helper_transform_eval(binding, ast, opts \\ [])

  def helper_transform_eval(binding, ast, opts) do

    ast = ast
    |> helper_transform(opts)

    result = ast
    |> Code.eval_quoted(binding, __ENV__)
    |> elem(0)

    {result, ast |> Macro.to_string}

  end

  def helper_assert_transform_eval(expect, binding, ast, opts \\ [])

  def helper_assert_transform_eval(expect, binding, ast, opts) do

    {actual, _code}  = helper_transform_eval(binding, ast, opts)

    assert expect == actual

    actual

  end

  def helper_asts_sort_to_string(asts, weights \\ nil) do
    asts
    |> PAU.asts_sort(weights)
    |> Enum.map(&Macro.to_string/1)
  end

  def helper_asts_reduce(asts \\ []) do
    ast = asts
    |> PAU.asts_reduce
    |> Macro.postwalk(fn
      {name, ctx, __MODULE__} -> {name, ctx, nil}
      x -> x
    end)

    {ast, ast |> Macro.to_string}
  end

  def helper_asts_reduce_eval(binding, asts \\ []) do
    helper_asts_transform_eval(binding, &PAU.asts_reduce/1, asts)
    |> Tuple.delete_at(2)
  end

  def helper_assert_asts_reduce_eval(result, binding, asts \\ []) do
    helper_assert_asts_transform_eval(result, binding, &PAU.asts_reduce/1, asts)
  end

  def helper_ast_from_mfa_to_string(mfa) do
    helper_asts_transform_eval(nil, &PAU.ast_from_mfa/1, mfa)
    |> elem(1)
  end

  def helper_assert_ast_from_mfa(expect, mfa) do
    helper_assert_asts_transform_eval({nil,expect}, nil, &PAU.ast_from_mfa/1, mfa)
  end

  def helper_signature_create_tuple(signature, opts \\ []) do

    signature_asts = signature
    |> PAS.signature_create(opts)

    signature_code = signature_asts
    |> Enum.map(fn ast -> ast |> Macro.to_string end)
    |> Enum.join(", ")
    |> fn str -> "(" <> str <> ")" end.()

    {signature_asts, signature_code}

  end

  def helper_signature_create(signature, opts \\ []) do

    signature
    |> helper_signature_create_tuple(opts)
    |> elem(0)

  end

  def helper_signature_create_to_string(signature, opts \\ []) do

    signature
    |> helper_signature_create_tuple(opts)
    |> elem(1)

  end

  def helper_assert_signature_create(expect, signature, opts \\ []) do

    {_signature_asts, signature_code} = signature |> helper_signature_create_tuple(opts)

    assert expect == signature_code

  end

  @signature_unquotablesA %{

    arg0: :uA_arg0 |> Macro.var(nil),
    arg1: :uA_arg1 |> Macro.var(nil),
    arg2: :uA_arg2 |> Macro.var(nil),

    default_42: 42,
    default_map1: %{a: 1, b: 2, c: 3} |> Macro.escape,
    default_map2: %{x: 10, y: 11, z: 12} |> Macro.escape,
    default_tuple1: {1, :two, "tre"} |> Macro.escape,

  }

    @signature_unquotablesB %{
    arg0: :uB_arg0 |> Macro.var(nil),
    arg1: :uB_arg1 |> Macro.var(nil),
    arg2: :uB_arg2 |> Macro.var(nil),
    }

    @signature_opts_sets %{
      opts_set1: [unquotables: @signature_unquotablesA],
      opts_set2: [unquotables: @signature_unquotablesB],
    }

  def helper_run_tests_signature1(opts \\ []) do

    test_mapper = fn

      [ code_string, signature] when is_binary(code_string) ->

        [c: :helper_signature_create_to_string, v: signature, r: code_string]

     [code_string, signature, opts_set] when is_binary(code_string) ->

        opts = @signature_opts_sets |> Map.fetch!(opts_set)

      [c: :helper_signature_create_to_string, v: signature, a: [opts], r: code_string]

    end

    [
      test_mapper: test_mapper,
      test_module: __MODULE__,
    ] ++ opts
    |> Harnais.run_tests_default_test_value

  end

  def helper_assert_ast_validate!(actual) do
    assert actual == actual |> PAU.ast_validate!
    actual
  end

  def helper_run_tests_ast_validate!(opts \\ []) do

    test_mapper = fn

      [value, flag] ->

        [f: flag, c: :helper_assert_ast_validate!, v: value, r: value]

      value ->

        [c: :helper_assert_ast_validate!, v: value, r: value]

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
    end

  end

end

