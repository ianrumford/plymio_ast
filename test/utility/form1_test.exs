defmodule PlymioAstUtilityForm1Test do

  use PlymioAstHelpersTest
  alias Plymio.Ast.Utility, as: PAU

   test "forms_pipe: eval" do

    binding = [x: 42, y: 2]

    assert {:ok, ast} = [
      Macro.var(:x, nil),
      quote(do: fn x -> x * x end.()),
      quote(do: List.wrap)
    ]
    |> PAU.forms_pipe

    ast
    |> helper_ast_eval(binding: binding, result: 1764,
    texts: ["x |> (fn x -> x * x end).() |> List.wrap()"])

  end

end
