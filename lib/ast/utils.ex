defmodule Plymio.Ast.Utils do

  @moduledoc ~S"""
  Utility Functions for Asts (Quoted Forms)
  """

  require Logger
  require Plymio.Ast.Helpers, as: PAH

  @type ast :: Macro.t
  @type asts :: ast | [ast]

  @type ast_pipe_pure :: Macro.t
  @type ast_pipe_index :: {Macro.t, integer}

  @type ast_pipe :: ast_pipe_pure | ast_pipe_index

  @type asts_pipe :: ast_pipe | [ast_pipe]

  @type ast_fun_or_acc_fun_tuple ::
  atom |
  (ast -> ast) |
  {any, atom} |
  {any, (ast, any -> {ast, any})}

  @type ast_or_ast_acc_tuple :: ast | {ast, any}

  @type asts_sort_weight_key :: atom
  @type asts_sort_weight_map :: %{optional(asts_sort_weight_key) => any}

  # values can be fun or {acc, fun} tuples
  @ast_edit_verb_map %{

    var_context_nil: &PAH.ast_edit_var_context_nil/1,

  }

  defp ast_edit_resolve_verb(verb) do
    @ast_edit_verb_map |> Map.fetch!(verb)
  end

  @doc ~S"""

  `ast_postwalk/2` runs `Macro.postwalk/2` or `Macro.postwalk/3` depending
  on whether the 2nd argument is a either function of arity one, or a
  2tuple where the first element is the accumulator and the second a
  function of arity two.

  If the second argument is nil, the call to `Macro.postwalk` is
  prempted and the ast returned unchanged.

  ## Examples

  This examples changes occurences of the `x` var to the `a` var.

      iex> ast = quote do
      ...>   x = x + 1
      ...> end
      ...> |> ast_postwalk(fn
      ...>      {:x, _, _} -> Macro.var(:a, nil)
      ...>      # passthru
      ...>      x -> x
      ...> end)
      ...> [a: 42] # binding
      ...> |> helper_ast_eval(ast)
      {43, "a = a + 1"}

   This example changes `x` to `a` and uses an accumulator to count the occurences of the `a` var:

      iex> {ast, acc} = quote do
      ...>   x = x + 1
      ...>   x = x * x
      ...>   x = x - 5
      ...> end
      ...> |> ast_postwalk(
      ...>     {0, fn
      ...>      {:x, _, _}, acc -> {Macro.var(:a, nil), acc + 1}
      ...>      # passthru
      ...>      x,s -> {x,s}
      ...> end})
      ...> [a: 42] # binding
      ...> |> helper_ast_eval(ast)
      ...> |> Tuple.insert_at(0, acc) # add the accumulator
      {7, 1844, "(\n  a = a + 1\n  a = a * a\n  a = a - 5\n)"}

  """

  @spec ast_postwalk(ast, ast_fun_or_acc_fun_tuple) :: ast_or_ast_acc_tuple

  def ast_postwalk(ast, value \\ nil)

  def ast_postwalk(ast, nil) do
    ast
  end

  def ast_postwalk(ast, {acc, verb})
  when is_tuple(ast) and is_atom(verb) do

    # this way to satisfy dialyzer
    fun = verb |> ast_edit_resolve_verb

    ast_postwalk(ast, {acc, fun})

  end

  def ast_postwalk(ast, verb)
  when is_tuple(ast) and is_atom(verb) do

    # this way to satisfy dialyzer
    value = verb |> ast_edit_resolve_verb

    ast_postwalk(ast, value)

  end

  def ast_postwalk(ast, {acc, fun})
  when is_tuple(ast) and is_function(fun, 2) do

    {ast, acc} = ast
    |> Macro.postwalk(acc, fun)

    {ast, acc}

  end

  def ast_postwalk(ast, fun)
  when is_tuple(ast) and is_function(fun, 1) do

    ast
    |> Macro.postwalk(fun)

  end

  @doc ~S"""
  `ast_prewalk/2` takes the same arguments as `ast_postwalk/2`.
  """

  @spec ast_prewalk(ast, ast_fun_or_acc_fun_tuple) :: ast_or_ast_acc_tuple

  # header
  def ast_prewalk(ast, value \\ nil)

  def ast_prewalk(ast, nil) do
    ast
  end

  def ast_prewalk(ast, {acc, fun})
  when is_tuple(ast) and is_function(fun, 2) do

    {ast, acc} = ast
    |> Macro.prewalk(acc, fun)

    {ast, acc}

  end

  def ast_prewalk(ast, fun)
  when is_tuple(ast) and is_function(fun, 1) do

    ast
    |> Macro.prewalk(fun)

  end

  @doc ~S"""
  `ast_validate/1` runs `Macro.validate/1` on the argument and if the
  result is not `:ok` raises an `ArgumentError` exception.

  ## Examples

      iex> 1 |> ast_validate!
      1

      iex> nil |> ast_validate! # nil is a valid ast
      nil

      iex> [:x, :y] |> ast_validate!
      [:x, :y]

      iex> ast = {:x, :y} # this 2tuple is a valid ast without escaping
      ...> result = ast |> ast_validate!
      ...> match?(^result, ast)
      true

      iex> {:x, :y, :z} |> ast_validate!
      ** (ArgumentError) expected an ast, got: {:error, {:x, :y, :z}}

      iex> %{a: 1, b: 2, c: 3} |> ast_validate! # map not a valid ast
      ** (ArgumentError) expected an ast, got: {:error, %{a: 1, b: 2, c: 3}}

      iex> ast = %{a: 1, b: 2, c: 3} |> Macro.escape # escaped map is a valid ast
      ...> result = ast |> ast_validate!
      ...> match?(^result, ast)
      true

  """

  @spec ast_validate!(ast) :: ast | no_return
  def ast_validate!(ast) do
    case ast |> Macro.validate do
      :ok -> ast
      {:error, remainder} = error ->
         Logger.error "#{inspect __MODULE__}.ast_validate!: not a valid ast #{inspect ast} remainder #{inspect remainder}"
         raise ArgumentError, message: "expected an ast, got: #{inspect error}"
    end
  end

  @doc ~S"""

  `asts_validate!/1` validates a list of asts using `ast_validate!/1`, returns the asts or
  raises an `ArgumentError` exception on the first invalid one.

  ## Examples

      iex> [1, 2, 3] |> asts_validate!
      [1, 2, 3]

      iex> [1, {2, 2}, :three] |> asts_validate!
      [1, {2, 2}, :three]

      iex> [1, {2, 2, 2}, %{c: 3}] |> asts_validate!
      ** (ArgumentError) expected an ast, got: {:error, {2, 2, 2}}
  """

  @spec asts_validate!([ast]) :: [ast] | no_return
  def asts_validate!(asts) when is_list(asts) do
    asts |> Enum.each(fn ast -> ast |> ast_validate! end)
    asts
  end

  defp asts_enumerate(asts) do
    asts
    |> List.wrap
    |> List.flatten
    |> Stream.reject(&is_nil/1)
    |> Enum.flat_map(fn ast -> ast_enumerate(ast) end)
  end

  defp ast_enumerate(ast) when is_tuple(ast) do

    case elem(ast, 0) do

      # if a block the args are the individual "statements"
      :__block__ -> elem(ast, 2)

      # just an ast but must return a list
      _ -> [ast]

    end

  end

  @doc ~S"""
  `asts_group/` take one or more asts and returns a `Enum.group_by/1`
  map using the first element of the tuple as the key.
  """

  @spec asts_group(asts) :: map

  def asts_group(asts) do

    asts
    |> List.wrap
    |> Enum.group_by(fn {key, _, _} -> key end)

  end

  @plymio_ast_utils_sort_weight_default 9999

  @doc ~S"""
  `asts_sort_weight_default/0` returns the hardcoded "backstop" weight:

  Examples

      iex> asts_sort_weight_default()
      9999
  """

  @spec asts_sort_weight_default() :: any
  def asts_sort_weight_default() do
    @plymio_ast_utils_sort_weight_default
  end

  @plymio_ast_utils_sort_weights_default %{
    require: 1000,
    use: 1500,
    alias: 2000,
    defdelegate: 2500,
    import: 3000,
    defmacro: 4000,
    def: 5000,
    default: @plymio_ast_utils_sort_weight_default}

  @plymio_ast_utils_sort_weights_values_sorted @plymio_ast_utils_sort_weights_default
  |> Map.values
  |> Stream.uniq
  |> Enum.sort

  @doc ~S"""
  `asts_sort_weights_default/0` returns the default map used to sort asts.
  """

  @spec asts_sort_weights_default() :: map
  def asts_sort_weights_default() do
    @plymio_ast_utils_sort_weights_default
  end

  @doc ~S"""
  `asts_sort_weight_get/2` takes an ast "key" (the first element), and
  an (optional) weight map, and returns the weight.

  If no weight map is supplied, the default map is used.

  If the key does not exist in the weight map, the `:default` key is
  tried, else the "backstop" weight (`asts_sort_weight_default/0`) returned.

  Examples

      iex> :use |> asts_sort_weight_get
      1500

      iex> :require |> asts_sort_weight_get
      1000

      iex> :use |> asts_sort_weight_get(%{use: 42, require: 43, import: 44})
      42

      iex> :unknown |> asts_sort_weight_get(%{use: 42, require: 43, import: 44})
      9999

      iex> :unknown |> asts_sort_weight_get(%{use: 42, require: 43, import: 44, default: 99})
      99
  """

  @spec asts_sort_weight_get(asts_sort_weight_key, asts_sort_weight_map) :: any

  def asts_sort_weight_get(key, weights \\ @plymio_ast_utils_sort_weights_default) do

    weights
    |> Map.get(
      key,
      Map.get(weights, :default, @plymio_ast_utils_sort_weight_default))

  end

  @doc ~S"""
  `asts_sort/2` take one or more asts together with an (optional)
  weight map and returns a list of asts with the lower weight ast
  earlier in the list.

  Examples

      iex> quote(do: use X1) |> helper_asts_sort_to_string
      ["use(X1)"]

      iex> nil |> helper_asts_sort_to_string
      []

      iex> [
      ...>  quote(do: use X1),
      ...>  quote(do: require X2),
      ...>  quote(do: import X3),
      ...> ] |> helper_asts_sort_to_string
      ["require(X2)", "use(X1)", "import(X3)"]

      iex> [
      ...>  quote(do: use X1),
      ...>  quote(do: require X2),
      ...>  quote(do: import X3),
      ...> ] |> helper_asts_sort_to_string(%{import: 1, use: 3})
      ["import(X3)", "use(X1)", "require(X2)"]

      iex> [
      ...>  quote(do: use X1),
      ...>  quote(do: require X2),
      ...>  quote(do: import X3),
      ...> ] |> helper_asts_sort_to_string(%{import: 1, use: 3, default: 2})
      ["import(X3)", "require(X2)", "use(X1)"]

  """

  @spec asts_sort(asts, asts_sort_weight_map | nil) :: asts

  def asts_sort(asts, weights \\ nil) do

    {working_weights, ordered_weights} =
      case weights do
        x when is_nil(x) ->

          {@plymio_ast_utils_sort_weights_default, @plymio_ast_utils_sort_weights_values_sorted}

        x ->

          {x,
           x
           |> Map.values
           # need to add in the backstop
           |> Kernel.++([@plymio_ast_utils_sort_weight_default])
           |> Stream.uniq
           |> Enum.sort
          }

      end

    weighted_asts = asts
    |> asts_enumerate
    # need to delete dups
    |> Enum.uniq
    |> asts_group
    # use key to find weight
    |> Enum.map(fn {key, asts} ->
      {asts_sort_weight_get(key, working_weights), asts}
    end)
    |> Enum.group_by(fn {w,_a} -> w end)

    # now collect in weight order
    ordered_weights
    |> Enum.flat_map(fn w -> weighted_asts |> Map.get(w, []) end)
    # drop weights from the tuples leaving the asts
    |> Enum.flat_map(fn {_w,a} -> a end)

  end

  @doc ~S"""
  `asts_reduce/1` takes one or more asts and reduces them to a single
  ast using `Kernel.SpecialForms.unquote_splicing/1`.

  The list is first flattened and any `nils` removed before splicing.

  An empty list reduces to nil.

  ## Examples

      iex> ast = quote(do: a = x + y)
      ...> [x: 42, y: 8, c: 5] |> helper_asts_reduce_eval(ast)
      {50, "a = x + y"}

      iex> ast = [quote(do: a = x + y),
      ...>        quote(do: a * c)]
      ...> [x: 42, y: 8, c: 5] |>  helper_asts_reduce_eval(ast)
      {250, "(\n  a = x + y\n  a * c\n)"}

      iex> ast = nil
      ...> [x: 42, y: 8, c: 5] |> helper_asts_reduce_eval(ast)
      {nil, ""}

      iex> ast = [
      ...>   quote(do: a = x + y),
      ...>   nil,
      ...>   [
      ...>    quote(do: b = a / c),
      ...>    nil,
      ...>    quote(do: d = b * b),
      ...>   ],
      ...>   quote(do: e = a + d),
      ...> ]
      ...> [x: 42, y: 8, c: 5] |>helper_asts_reduce_eval(ast)
      {150.0, "(\n  a = x + y\n  b = a / c\n  d = b * b\n  e = a + d\n)"}

  """

  @spec asts_reduce(asts) :: ast

  def asts_reduce(asts \\ [])

  def asts_reduce([]), do: nil

  def asts_reduce({_,_,_} = ast), do: ast
  def asts_reduce([{_,_,_} = ast | []]), do: ast

  def asts_reduce(asts) do

    asts
    |> List.wrap
    |> List.flatten
    |> Enum.reject(&is_nil/1)
    |> fn asts ->

      case asts |> length do
         0 -> nil
         1 -> asts |> List.first
         _ ->
           quote do
             unquote_splicing(asts)
           end
      end

    end.()

  end

  @doc ~S"""

  `asts_pipe/1` takes one or more asts and uses `Macro.pipe/3` to pipe them together and create a new ast.

  Each ast in the list is passed to an `Enum.reduce/3` function, together with the result (ast) of all the pipe operations to date (i.e. the accumulator).

  The default behaviour is for the latest ast to become the zeroth
  argument in the accumulator ast (i.e. just as the left hand side
  of `|>` becomes the zeroth argument of the right hand
  side)

  However the call to `Macro.pipe/3` that does the piping takes the
  zero-offset index.

  To specify the pipe index, *any* of the asts in the list can be a 2tuple where the first element is the "pure" ast and the second the pipe index. No index (i.e. just the "pure" ast) implies index 0.

  > When the index is zero, a  `left |> right` ast is generated, otherwise the generated ast inserts the latest ast directly into the auumulator ast at the index. This is just to make the code, after `Macro.to_string/1`, visually more obvious.

  Any `nil` asts in the list are ignored. An empty list returns `nil`.

  ## Examples

  This example show what happens when all the asts do not have an explicit index:

       iex> ast = [
       ...> Macro.var(:x, nil),
       ...> quote(do: fn x -> x * x end.()),
       ...> quote(do: List.wrap)
       ...> ]
       ...> |> asts_pipe
       ...> [x: 42] |> helper_ast_eval(ast)
       {[1764], "x |> (fn x -> x * x end).() |> List.wrap()"}

  This example show what happens when an index of 2 is used to insert
  the value of `x` (42) as the 3rd argument in the call to a "partial" anonymous
  function which already has the 1st, 2nd and 4th arguments.

       iex> ast = [
       ...>   Macro.var(:x, nil),
       ...>   {quote(do: fn p, q, x, y -> [{y, x}, {p,q}] end.(:p, :this_is_q, "y")), 2},
       ...>   quote(do: Enum.into(%{}))
       ...> ]
       ...> |> asts_pipe
       ...> [x: 42] |> helper_ast_eval(ast)
       {%{:p => :this_is_q, "y" => 42},
        "(fn p, q, x, y -> [{y, x}, {p, q}] end).(:p, :this_is_q, x, \"y\") |> Enum.into(%{})"}
  """

  @spec asts_pipe(asts_pipe) :: ast

  # header
  def asts_pipe(asts \\ [])

  def asts_pipe([]), do: nil

  def asts_pipe([{{_,_,_} = ast, index} | []]) when is_integer(index) do
    ast
  end

  def asts_pipe([{_,_,_} = ast | []]) do
    ast
  end

  def asts_pipe(asts) do

    asts
    |> List.wrap
    |> Stream.reject(&is_nil/1)
    # now pipe earlier ast into later, using init_value as first seed
    |> Enum.reduce(nil,
    fn

      # ignore any nil asts
      nil, pipe -> pipe

      {ast, ndx}, nil when is_integer(ndx) -> ast
      ast, nil -> ast

      {ast, ndx}, pipe when is_integer(ndx) -> Macro.pipe(pipe, ast, ndx)

      ast, pipe ->

          # easier to scan by eye
          quote do
            unquote(pipe) |> unquote(ast)
          end

    end)

  end

  defp asts_from_mfas(mfas) do

    mfas
    |> List.wrap
    |> Enum.map(fn

      {m, f, a} when is_atom(m) and is_atom(f) and is_list(a) ->

      # need to escape the args
      args =
        case a do
          [] -> []
          args -> args |> Enum.map(&Plymio.Ast.Form.maybe_ast_escape/1)
        end

      quote do
        unquote(m).unquote(f)(unquote_splicing(args))
      end

    end)

  end

  @doc ~S"""
  `ast_from_mfa/1` creates an ast to implement the function call defined in an MFA 3tuple (`{module,function,arguments}`).

  Each argument is escaped if necessary (i.e. if not already a valid ast).

  It can be thought of as the static / ast "equivalent" of `Kernel.apply/1`.

  ## Examples

      iex> {X1, :f1, []} |> helper_ast_from_mfa_to_string
      "X1.f1()"

      iex> {X1, :f1, [1, :two, "tre"]} |> helper_ast_from_mfa_to_string
      "X1.f1(1, :two, \"tre\")"

      iex> {X2, :f2, [{1, :two, "tre"}]} |> helper_ast_from_mfa_to_string
      "X2.f2({1, :two, \"tre\"})"

      iex> {X2, :f2, [{1, :two, "tre"} |> Macro.escape]} |> helper_ast_from_mfa_to_string
      "X2.f2({1, :two, \"tre\"})"

      iex> {X3, :f3, [%{a: 1, b: %{b: 2}, c: {3, :tre, "tre"}}]} |> helper_ast_from_mfa_to_string
      "X3.f3(%{a: 1, b: %{b: 2}, c: {3, :tre, \"tre\"}})"

      iex> {X3, :f3, [%{a: 1, b: %{b: 2}, c: {3, :tre, "tre"}} |> Macro.escape]} |> helper_ast_from_mfa_to_string
      "X3.f3(%{a: 1, b: %{b: 2}, c: {3, :tre, \"tre\"}})"

  """

  def ast_from_mfa(mfa) do
    mfa |> asts_from_mfas |> List.first
  end

end

