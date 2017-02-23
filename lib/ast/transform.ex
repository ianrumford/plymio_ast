defmodule Plymio.Ast.Transform do

  @moduledoc ~S"""
  Utility Functions for Transforming Asts (Quoted Forms).

  `transform/2` implements a [dsl](http://elixir-lang.org/getting-started/meta/domain-specific-languages.html) to transform the ast.

  ## Applying the Transform DSL

  `transform/2` take the ast (`Macro.t`) together with a set of dsl options (`Keyword`) where the key is the transform *verb* (e.g. `:pipe_before` - see later) and the value the data needed by the *verb*.

  The dsl options define a *pipeline* of transformations sequentially applied to the original ast.

  Many of the dsl verbs are implemented by functions in `Plymio.Ast.Utils`.

  ## TO SORT

  LOOKS OK; FINAL REVIEW.

  ## Helper Functions for the Doctests

  The tests below use helper functions.

  One helper,  `helper_transform_eval/3`,
  applies the dsl (`transform/2`) and run the returned ast
  (`Code.eval_quoted/3`) with the supplied binding. The helper returns
  a 2tuple with the result of the eval and the visualised
  (`Macro.to_string/1`) code. The helper looks like this:

      def helper_transform_eval(binding, ast, dsl_opts) do
        ast = ast |> Plymio.Ast.Transform.transform(dsl_opts)
        result = ast |> Code.eval_quoted(binding, __ENV__) |> elem(0)
        {result, ast |> Macro.to_string}
      end

  ## Transform DSL Verbs

  The dsl implements a number of *verbs* given in the `dsl_options` (`Keyword`).

  ### DSL Verb - `:pipe_before`

  The `:pipe_before` verb pipes the *main* ast argument *before* the ast(s) given as the value of verb.

  This example pipes a var (`x`) into a call to an anonymous function that adds 1:

      iex> ast = Macro.var(:x, nil)
      ...> before_ast = quote(do: fn v -> v + 1 end.())
      ...> [x: 42] # binding
      ...> |> helper_transform_eval(ast, pipe_before: before_ast)
      {43, "x |> (fn v -> v + 1 end).()"}

   This example supplies a list of asts to the `:pipe_before` verb:

      iex> ast = Macro.var(:x, nil)
      ...> before_ast1 = quote(do: fn v -> v + 1 end.())
      ...> before_ast2 = quote(do: fn v -> v * v end.())
      ...> before_ast3 = quote(do: fn v -> 1 - v end.())
      ...> [x: 42]
      ...> |> helper_transform_eval(ast,
      ...>      pipe_before: [before_ast1, before_ast2, before_ast3])
      {-1848, "x |> (fn v -> v + 1 end).() |> (fn v -> v * v end).() |> (fn v -> 1 - v end).()"}

  Implicit in the examples above has been that the left side ast of
  the pipe (`|>`) should become the zeroth argument of the right side
  ast. However the call to `Macro.pipe/3` that does the piping takes
  the index (`Integer` - zero offset) to use (in the example above it was set automatically to zero).

  To specify the pipe index, *any* of the asts can be a 2tuple where the first element is the "pure" ast and the second the pipe index.

  These two simple examples of a subtraction show why the pipe index
  is important: The first uses the default index of zero (so the code
  becomes `x - 100`) while the second supplies an index of 1 (so the code
  becomes `100 - x`).

  > When the index is zero, a  `left |> right` ast is generated (first example), otherwise the generated ast inserts the left ast directly in the right ast at the index. This is just to make the code, after `Macro.to_string/1`, visually more obvious.

      iex> ast = Macro.var(:x, nil)
      ...> before_ast = quote(do: Kernel.-(100))
      ...> [x: 42]
      ...> |> helper_transform_eval(ast, pipe_before: before_ast)
      {-58, "x |> Kernel.-(100)"}

      iex> ast = Macro.var(:x, nil)
      ...> before_ast = {quote(do: Kernel.-(100)), 1}
      ...> [x: 42]
      ...> |> helper_transform_eval(ast, pipe_before: before_ast)
      {58, "Kernel.-(100, x)"}

  ### DSL Verb - `:pipe_after`

  The `:pipe_after` verb pipes the *main* ast argument *after* the
  ast(s) given as the value of the `:pipe_after`.

  This is the same example as above that pipes a var (`x`) into a call
  to an anonymous function that adds 1 *but* the arguments are
  switched around:

      iex> ast = quote(do: fn v -> v + 1 end.())
      ...> after_ast = Macro.var(:x, nil)
      ...> [x: 42]
      ...> |> helper_transform_eval(ast, pipe_after: after_ast)
      {43, "x |> (fn v -> v + 1 end).()"}

  Again, the same example above that supplies a list of asts to the
  `:pipe_before` verb but with the arguments switched around:

      iex> ast = quote(do: fn v -> 1 - v end.())
      ...> after_ast1 = Macro.var(:x, nil)
      ...> after_ast2 = quote(do: fn v -> v + 1 end.())
      ...> after_ast3 = quote(do: fn v -> v * v end.())
      ...> [x: 42]
      ...> |> helper_transform_eval(ast,
      ...>      pipe_after: [after_ast1, after_ast2, after_ast3])
      {-1848, "x |> (fn v -> v + 1 end).() |> (fn v -> v * v end).() |> (fn v -> 1 - v end).()"}

  `:pipe_after` also support supplying an index for *any* ast. This
  somewhat complicated example demonstrates using multiple asts with
  indicies:

      iex> ast = Macro.var(:x, nil)
      ...> [x: 7] # binding
      ...> |> helper_transform_eval(ast, pipe_before: [
      ...>   {quote(do: Kernel./(42)), 1}, # index is 1
      ...>   quote(do: List.wrap),
      ...>   {quote(do: Kernel.++([1,2,3])), 1}, # index is 1
      ...>   {quote(do: Enum.reduce([&Enum.sum/1, fn v -> v * v end], fn f, s -> f.(s) end)), 1},
      ...> ])
      {144.0, "Enum.reduce([&Enum.sum/1, fn v -> v * v end], Kernel.++([1, 2, 3], Kernel./(42, x) |> List.wrap()), fn f, s -> f.(s) end)"}

  ### DSL Verb - `:postwalk`

  `:postwalk` runs `Macro.postwalk/2` or `Macro.postwalk/3` depending
  on whether the verb's value is a either function of arity one, or a
  2tuple where the first element is the accumulator and the second a
  function of arity two.

  This simple example just changes the x var (`{:x, [], nil}`) to y (`{:y, [], nil}`).

      iex> ast = Macro.var(:x, nil)
      ...> [y: 99]
      ...> |> helper_transform_eval(ast, postwalk:
      ...>      fn {:x, [], nil} -> Macro.var(:y, nil)
      ...>      # passthru
      ...>      any -> any
      ...> end)
      {99, "y"}

   This more involved example returns an `{ast, acc}` 2tuple where the
   acc(umulator) is a count of how often the x var was used. Refer to
   `Macro.postwalk/3` for explanation of an accumulator function.

      iex> ast = quote do
      ...>   x = x + x
      ...>   x = x * x
      ...>   x = x - x
      ...> end
      ...> {_, acc} = ast
      ...> |> helper_transform(postwalk: {
      ...>      0, # the initial accumulator
      ...>      fn {:x, [], _} = ast, acc -> {ast, acc + 1}
      ...>      # passthru
      ...>      any, acc -> {any, acc}
      ...>    end})
      ...> match?(9, acc)
      true

  ### DSL Verb - `:prewalk`

  `:prewalk` runs `Macro.prewalk/2` or `Macro.prewalk/3` depending
  on whether the verb's value is a either function of arity one, or a
  2tuple where the first element is the accumulator and the second a
  function of arity two.

  In many situations, `:prewalk` and `:postwalk` do the same thing: this is the `:postwalk` example rewritten to use `:prewalk`:

      iex> ast = Macro.var(:x, nil)
      ...> [y: 99]
      ...> |> helper_transform_eval(ast, prewalk:
      ...>      fn {:x, [], nil} -> Macro.var(:y, nil)
      ...>      # passthru
      ...>      any -> any
      ...> end)
      {99, "y"}

  ### DSL Verb - `:transform`

  The `:transform` verb applies an arbitrary function to the current ast / value.

  This example retrieves the ast from an `{ast, acc}` 2tuple from the example above.

      iex> quote do
      ...>   x = x + x
      ...>   x = x * x
      ...>   x = x - x
      ...> end
      ...> |> helper_transform(postwalk: {
      ...>       0, # the initial accumulator
      ...>      fn {:x, [], _} = ast, acc -> {ast, acc + 1}
      ...>      # passthru
      ...>      any, acc -> {any, acc}
      ...>      end},
      ...>      transform: fn {ast, _acc} -> ast end)
      ...> |> Macro.to_string
      ...> |> fn str -> Regex.replace(~r/\n\s*/, str, "|") end.() # tidy code string
      "(|x = x + x|x = x * x|x = x - x|)"

  ### DSL Verb - `:splice`

  The `:splice` verb is akin to
  `Kernel.SpecialForms.unquote_splicing/1` where the one or more asts
  given as the verb's value are inserted consecutively into the
  arguments of the main ast.

  If main ast has an index (e.g. `{ast,index}`), the insertion starts at the index, else zero.

  This example splices the first thrre arguments into an anonymous function call,
  starting at the zeroth argument. Note the anon function call in the
  ast is defined as a "partial" call with the last three arguments
  already provided.

      iex> ast = quote(do: fn a,b,c,x,y,z -> [a,b,c,x,y,z] end.(:this_is_x, :y, "z"))
      ...> args012 = [:a, {:b21, :b22}, %{c: 3}] |> Enum.map(&Macro.escape/1)
      ...> [] |> helper_transform_eval(ast, splice: args012)
      {[:a, {:b21, :b22}, %{c: 3}, :this_is_x, :y, "z"],
       "(fn a, b, c, x, y, z -> [a, b, c, x, y, z] end).(:a, {:b21, :b22}, %{c: 3}, :this_is_x, :y, \"z\")"}

  This example specifies the index (`{ast, 2}`) to start the splicing at the 3rd argument:

      iex> ast = quote(do: fn a,b,c,x,y,z -> [a,b,c,x,y,z] end.(:a, {:b21, :b22}, "z"))
      ...> args234 = [%{c: 3}, :this_is_x, :y] |> Enum.map(&Macro.escape/1)
      ...> [] |> helper_transform_eval({ast, 2}, splice: args234)
      {[:a, {:b21, :b22}, %{c: 3}, :this_is_x, :y, "z"],
       "(fn a, b, c, x, y, z -> [a, b, c, x, y, z] end).(:a, {:b21, :b22}, %{c: 3}, :this_is_x, :y, \"z\")"}

  This example show how a named function's signature could be spliced.
  Remember though that the arguments to a function are the arguments in the
  first argument in the `def` ast. For example the quoted *fun1* `def`
  below looks like:

       {:def,
            [context: Elixir, import: Kernel],
            [{:fun1, [context: Elixir],
                     # here are fun1's arguments
                     [{:a, [], Elixir}, {:b, [], Elixir}, {:c, [], Elixir}]},
            [do: {:+, [context: Elixir, import: Kernel],
                [{:+, [context: Elixir, import: Kernel],
                [{:a, [], Elixir}, {:b, [], Elixir}]}, {:c, [], Elixir}]}]]}

  The example uses a `:postwalk` function to (recursively) call `transform/1` to splice the argumnets into the `{:fun1, ctx, args}` ast.

      iex> arg0_ast = quote(do: a)
      ...> arg1_ast = quote(do: b \\ 9)
      ...> arg2_ast = quote(do: c \\ 42)
      ...> signature = [arg0_ast, arg1_ast, arg2_ast]
      ...> quote do
      ...>   def fun1() do
      ...>     a + b + c
      ...>    end
      ...> end
      ...> |> helper_transform(postwalk: fn
      ...>      {:fun1, _, _} = fun1_ast -> fun1_ast |> transform(splice: signature)
      ...>      x -> x # passthru
      ...>      end)
      ...> |> Macro.to_string
      "def(fun1(a, b \\\\ 9, c \\\\ 42)) do\n  a + b + c\nend"
  """

  require Logger
  alias Plymio.Ast.Utils, as: PAU

  @type dsl_ast_pure :: Macro.t
  @type dsl_ast_index :: {Macro.t, integer}

  @type dsl_ast :: dsl_ast_pure | dsl_ast_index

  @type dsl_ast_result :: Macro.t

  @type dsl_fun_transform :: (any -> any)

  @type dsl_option ::
  {:transform, dsl_fun_transform} |
  {:splice, any} |
  {:pipe_before, dsl_ast_pure} |
  {:pipe_after, dsl_ast_pure}

  @type dsl_options :: [dsl_option]

  # header
  defp transform_verb(ast, verb, value)

  # before: ast goes before value
  defp transform_verb(ast, :pipe_before, nil), do: ast

  defp transform_verb(ast, :pipe_before, value) do
    PAU.asts_pipe([ast | List.wrap(value)])
  end

  # after: ast goes after value
  defp transform_verb(ast, :pipe_after, nil), do: ast

  defp transform_verb(ast, :pipe_after, value) do
    #PAU.asts_pipe([value, ast])
    PAU.asts_pipe(List.wrap(value) ++ [ast])
  end

  defp transform_verb(ast, :transform, nil), do: ast
  defp transform_verb(ast, :transform, fun) when is_function(fun) do
    ast |> fun.()
  end

  defp transform_verb(ast, verb, nil)
  when verb in [:postwalk, :edit] do
    ast
  end

  defp transform_verb(ast, verb, value)
  when verb in [:postwalk, :edit] do
    PAU.ast_postwalk(ast, value)
  end

  defp transform_verb(ast, verb, value)
  when verb in [:prewalk] do
    PAU.ast_prewalk(ast, value)
  end

  defp transform_verb(ast, :pipe_index, value) when is_integer(value) do
    {ast, value}
  end

  defp transform_verb({ast, _ndx}, :splice, []), do: ast
  defp transform_verb(ast, :splice, []), do: ast
  defp transform_verb(ast, :splice, value) do

    ast_ndx = case ast do
                {_ast, _ndx} -> ast
                _ -> {ast, 0}

              end

    {ast, _} = value
    |> List.wrap
    |> Enum.reduce(ast_ndx,
    fn

      next_ast, {last_ast, last_ndx} ->

        {Macro.pipe(next_ast, last_ast, last_ndx), last_ndx + 1}

    end)

    ast

  end

  @doc ~S"""
  `transform/2` implements the dsl.

  See the examples above of each individual option.
  """

  @spec transform(dsl_ast, dsl_options) :: dsl_ast

  # header
  def transform(ast, opts \\ [])

  def transform({_,_,_} = ast, []) do
    ast
  end

  def transform({{_,_,_} = ast, ndx}, []) when is_integer(ndx) do
    ast
  end

  def transform(ast, opts) do

    opts
    |> Enum.reduce(ast, fn {k,v}, s -> s |> transform_verb(k,v) end)

  end

end

