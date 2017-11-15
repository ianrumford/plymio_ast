defmodule PlymioAstUtilityDoctestTest do

  use ExUnit.Case, async: true

  use PlymioAstHelpersTest
  import Plymio.Ast.Utility

  doctest Plymio.Ast.Utility

end
