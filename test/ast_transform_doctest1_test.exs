defmodule PlymioAstTransformDoctest1Test do

  use ExUnit.Case, async: true

  use PlymioAstHelpersTest

  import Plymio.Ast.Transform

  doctest Plymio.Ast.Transform

end
