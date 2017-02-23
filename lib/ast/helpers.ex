defmodule Plymio.Ast.Helpers do

  @moduledoc false

  def ast_edit_var_context_nil(ast)

  def ast_edit_var_context_nil({name, ctx, mod}) when is_atom(name) and is_atom(mod)
  and is_list(ctx) and (length(ctx) == 0) do
    {name, [], nil}
  end

  def ast_edit_var_context_nil(ast) do
    ast
  end

end
