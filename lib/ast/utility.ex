defmodule Plymio.Ast.Utility do

  @moduledoc ~S"""
  Utility Functions for Asts (Quoted Forms)

  ## Documentation Terms

  In the documentation there are terms, usually in *italics*, used to mean the same thing (e.g. *form*).

  ### *form*

  A *form* is an ast.

  ### *forms*

  A *forms* is zero, one or more *form*s.

  ### *form_index*

  A *form_index* value is a 2tuple where the first element is a *form* and the second an `index` (an integer).

  ## Function Results

  Many (new) functions either return `{:ok, value}` or `{:error, error}` where `error` will be as `Exception`.

  The default action for bang functions when fielding an `{:error, error}` result is to raise the `error`.

  """

  require Logger

  @type error :: %ArgumentError{}

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

  @type form :: Macro.t
  @type forms :: form | [form]

  @doc ~S"""
  `form_validate/1` calls `Macro.validate/1` on the argument (the expected *form*)
  and if the result is `:ok` returns {:ok, form}, else `{:error, error}`.

  ## Examples

      iex> 1 |> form_validate
      {:ok, 1}

      iex> nil |> form_validate # nil is a valid ast
      {:ok, nil}

      iex> [:x, :y] |> form_validate
      {:ok, [:x, :y]}

      iex> ast = {:x, :y} # this 2tuple is a valid ast without escaping
      ...> {:ok, result} = ast |> form_validate
      ...> ast |> helper_ast_compare(result)
      {:ok, {:x, :y}}

      iex> {:error, error} = {:x, :y, :z} |> form_validate
      ...> match?(%ArgumentError{message: "expected a form; got: {:x, :y, :z}"}, error)
      true

      iex> {:error, error} = %{a: 1, b: 2, c: 3} |> form_validate # map not a valid ast
      ...> match?(%ArgumentError{message: "expected a form; got: %{a: 1, b: 2, c: 3}"}, error)
      true

      iex> ast = %{a: 1, b: 2, c: 3} |> Macro.escape # escaped map is a valid ast
      ...> {:ok, result} = ast |> form_validate
      ...> ast |> helper_ast_compare(result)
      {:ok,  %{a: 1, b: 2, c: 3} |> Macro.escape}

  """

  @spec form_validate(any) :: {:ok, form} | {:error, error}

  def form_validate(form)

  def form_validate(form) do
    case form |> Macro.validate do
      :ok -> {:ok, form}
      {:error, _remainder} -> {:error, %ArgumentError{message: "expected a form; got: #{inspect form}"}}
    end
  end

  @doc ~S"""

  `form_validate!/1` calls `form_validate/1` with the argument and if the
  result is `{:ok, form}` returns the `form`.

  ## Examples

      iex> 1 |> form_validate!
      1

      iex> nil |> form_validate! # nil is a valid ast
      nil

      iex> [:x, :y] |> form_validate!
      [:x, :y]

      iex> ast = {:x, :y} # this 2tuple is a valid ast without escaping
      ...> result = ast |> form_validate!
      ...> ast |> helper_ast_compare!(result)
      {:x, :y}

      iex> {:x, :y, :z} |> form_validate!
      ** (ArgumentError) expected a form; got: {:x, :y, :z}

      iex> %{a: 1, b: 2, c: 3} |> form_validate! # map not a valid ast
      ** (ArgumentError) expected a form; got: %{a: 1, b: 2, c: 3}

      iex> ast = %{a: 1, b: 2, c: 3} |> Macro.escape # escaped map is a valid ast
      ...> result = ast |> form_validate!
      ...> ast |> helper_ast_compare!(result)
      %{a: 1, b: 2, c: 3} |> Macro.escape

  """

  @spec form_validate!(any) :: ast | no_return

  def form_validate!(ast) do
    case ast |> form_validate do
      {:ok, ast} -> ast
      {:error, error} ->
         Logger.error Exception.message(error)
         raise error
    end
  end

  @doc ~S"""

  `forms_validate/1` validates the *forms* using `form_validate/1` on each *form*, returning `{:ok, forms}` if all are valid, else `{:error, error}`.

  ## Examples

      iex> [1, 2, 3] |> forms_validate
      {:ok, [1, 2, 3]}

      iex> [1, {2, 2}, :three] |> forms_validate
      {:ok, [1, {2, 2}, :three]}

      iex> {:error, error} = [1, {2, 2, 2}, %{c: 3}] |> forms_validate
      ...> match?(%ArgumentError{message: "expected valid forms; got invalid_indices: [1, 2]"}, error)
      true

  """

  @spec forms_validate(list) :: {:ok, forms} | {:error, error}

  def forms_validate(forms) when is_list(forms) do

    forms
    |> Stream.with_index
    |> Enum.reduce([],
    fn {form,index}, invalid_indices ->
      case form |> form_validate do
        {:ok, _} -> invalid_indices
        {:error, _} -> [index | invalid_indices]
      end
    end)
    |> case do

         # no invalid forms
         [] -> {:ok, forms}

         invalid_indices ->

           {:error, %ArgumentError{message: "expected valid forms; got invalid_indices: #{inspect Enum.reverse(invalid_indices)}"}}

       end

  end

  @doc ~S"""

  `forms_validate!/1` validates a *forms* using `forms_validate/1`.

  If the result is `{:ok, forms}` returns the `forms`.

  ## Examples

      iex> [1, 2, 3] |> forms_validate!
      [1, 2, 3]

      iex> [1, {2, 2}, :three] |> forms_validate!
      [1, {2, 2}, :three]

      iex> [1, {2, 2, 2}, %{c: 3}] |> forms_validate!
      ** (ArgumentError) expected valid forms; got invalid_indices: [1, 2]

  """

  @spec forms_validate!(list) :: forms | no_return

  def forms_validate!(asts) when is_list(asts) do
    case asts |> forms_validate do
      {:ok, asts} -> asts
      {:error, error} -> raise error
    end
  end

  @doc ~S"""

  `forms_reduce/1` takes a *forms* and reduces the forms to a single
  *form* using `Kernel.SpecialForms.unquote_splicing/1`.

  If the reduction suceeds, `{:ok, reduced_form}` is returned else `{:error, error}`.

  The list is first flattened and any `nils` removed before splicing.

  An empty list reduces to `{:ok, nil}`.

  ## Examples

      iex> {:ok, reduced_form} = quote(do: a = x + y) |> forms_reduce
      ...> reduced_form |> helper_ast_test_forms_result_texts(binding: [x: 42, y: 8, c: 5])
      {:ok, {50, ["a = x + y"]}}

      iex> {:ok, reduced_form} = [
      ...>  quote(do: a = x + y),
      ...>  quote(do: a * c)
      ...> ] |> forms_reduce
      ...> reduced_form |> helper_ast_test_forms_result_texts(binding: [x: 42, y: 8, c: 5])
      {:ok, {250, ["(a = x + y\n a * c)"]}}

      iex> nil
      ...> |> helper_ast_test_forms_result_texts(binding: [x: 42, y: 8, c: 5])
      {:ok, {nil, [""]}}

      iex> {:ok, form} = [
      ...>  quote(do: a = x + y),
      ...>  nil,
      ...>  [
      ...>   quote(do: b = a / c),
      ...>   nil,
      ...>   quote(do: d = b * b),
      ...>  ],
      ...>  quote(do: e = a + d),
      ...> ] |> forms_reduce
      ...> form |> helper_ast_test_forms_result_texts(binding: [x: 42, y: 8, c: 5])
      {:ok, {150.0, ["(a = x + y\n b = a / c\n d = b * b\n e = a + d)"]}}

  """

  @spec forms_reduce(any) :: {:ok, form} | {:error, error}

  def forms_reduce(asts \\ [])

  def forms_reduce([]), do: {:ok, nil}

  def forms_reduce(forms) do

    forms
    |> List.wrap
    |> List.flatten
    |> Enum.reject(&is_nil/1)
    |> forms_validate
    |> case do

         {:error, _} = result -> result

         {:ok, forms} ->

           forms
           |> length
           |> case do
                0 -> {:ok, nil}
                1 -> {:ok, forms |> List.first}
                _ ->
                  form = quote do
                    unquote_splicing(forms)
                  end

                  {:ok, form}
              end

       end

  end

  @doc ~S"""

  `forms_reduce!/1` calls `forms_reduce/1` and if the result is `{:ok, forms}` returns `forms`.

  ## Examples

      iex> reduced_form = quote(do: a = x + y) |> forms_reduce!
      ...> reduced_form |> helper_ast_test_forms_result_texts(binding: [x: 42, y: 8, c: 5])
      {:ok, {50, ["a = x + y"]}}

      iex> [] |> forms_reduce!
      nil

      iex> nil |> forms_reduce!
      nil

  """

  @spec forms_reduce!(any) :: form | no_return

  def forms_reduce!(asts \\ [])

  def forms_reduce!([]), do: nil

  def forms_reduce!(forms) do

    with {:ok, forms} <- forms |> forms_reduce do
      forms
    else
      {:error, error} -> raise error
    end

  end

  @doc ~S"""

  `forms_pipe/1` takes a *forms* and uses `Macro.pipe/3` to pipe them together and create a new *form*.

  It returns `{:ok, form}` or `{:error, error}`.

  Each *form* in the *forms* is passed to an `Enum.reduce/3` function, together with the result (*form*) of all the pipe operations to date (i.e. the accumulator).

  The default behaviour is for the latest *form* to become the zeroth
  argument in the accumulator form (i.e. just as the left hand side
  of `|>` becomes the zeroth argument of the right hand
  side)

  However the call to `Macro.pipe/3` that does the piping takes the
  zero-offset index.

  To specify the pipe index, *any* of the forms in the list can be a *form_index*.
  No index (i.e. just the *form*) implies index 0.

  Any `nil` forms are ignored. An empty list returns `{:ok, nil}`.

  ## Examples

  No or empty arguments:

      iex> forms_pipe()
      {:ok, nil}
      iex> [] |> forms_pipe()
      {:ok, nil}

  Simple arguments:

      iex> 42 |> forms_pipe
      {:ok, 42}
      iex> :atom |> forms_pipe
      {:ok, :atom}

  An impossible pipe:

      iex> {:ok, ast} = [42, :atom] |> forms_pipe
      ** (ArgumentError) cannot pipe 42 into :atom, can only pipe into local calls foo(), remote calls Foo.bar() or anonymous functions calls foo.()

  This example show what happens when all the forms do not have an explicit index:

      iex> {:ok, form} = [
      ...>   Macro.var(:x, nil),
      ...>   quote(do: fn x -> x * x end.()),
      ...>   quote(do: List.wrap)
      ...> ]
      ...> |> forms_pipe
      ...> form |> helper_ast_test_forms_result_texts(binding: [x: 42])
      {:ok, {[1764], ["List.wrap((fn x -> x * x end).(x))"]}}

  This example show what happens when an index of 2 is used to insert
  the value of `x` (42) as the 3rd argument in the call to a "partial" anonymous
  function which already has the 1st, 2nd and 4th arguments.

       iex> {:ok, form} = [
       ...>   Macro.var(:x, nil),
       ...>   {quote(do: fn p, q, x, y -> [{y, x}, {p,q}] end.(:p, :this_is_q, "y")), 2},
       ...>   quote(do: Enum.into(%{}))
       ...> ]
       ...> |> forms_pipe
       ...> form |> helper_ast_test_forms_result_texts(binding: [x: 42])
       {:ok, {%{:p => :this_is_q, "y" => 42}, ["Enum.into((fn p, q, x, y -> [{y, x}, {p, q}] end).(:p, :this_is_q, x, \"y\"), %{})"]}}

  """

  @spec forms_pipe(any) :: {:ok, form} | {:error, error}

  # header
  def forms_pipe(forms \\ [])

  def forms_pipe([]) do
    {:ok, nil}
  end

  def forms_pipe([{{_,_,_} = ast, index} | []]) when is_integer(index) do
    {:ok, ast}
  end

  def forms_pipe([{_,_,_} = ast | []]) do
    {:ok, ast}
  end

  def forms_pipe(forms) do

    forms
    |> List.wrap
    |> Stream.reject(&is_nil/1)
    |> Enum.reduce_while([],
    fn value, fi_tuples ->

      case value |> form_index_normalise do
        {:ok, {_form, _index} = fi_pair} -> {:cont, [fi_pair | fi_tuples]}
        {:error, _} = result -> {:halt, result}
      end

    end)
    |> case do

         {:error, _} = result -> result

         fi_pairs ->

           ast = fi_pairs
           |> Enum.reverse
           |> Enum.reduce(nil,
           fn

             {form, _index}, nil -> form

             {form, index}, pipe ->

             Macro.pipe(pipe, form, index)

           end)

           {:ok, ast}

       end

  end

  @doc ~S"""

  `forms_pipe!/1` calls `forms_pipe/1` and if the result is `{:ok, form}` returns `form`.

  ## Examples

      iex> form = [
      ...>   Macro.var(:x, nil),
      ...>   quote(do: fn x -> x * x end.()),
      ...>   quote(do: List.wrap)
      ...> ]
      ...> |> forms_pipe!
      ...> form |> helper_ast_test_forms_result_texts(binding: [x: 42])
      {:ok, {[1764], ["List.wrap((fn x -> x * x end).(x))"]}}

  """

  @spec forms_pipe!(any) :: form | no_return

  def forms_pipe!(forms \\ [])

  def forms_pipe!([]), do: nil

  def forms_pipe!(forms) do

    with {:ok, forms} <- forms |> forms_pipe do
      forms
    else
      {:error, error} -> raise error
    end

  end

  @doc ~S"""

  `form_enumerate/1` takes a *form* and, if a `:__block__`, returns `{:ok, args}` where `args` are the individual statements in the block.

  ## Examples

      iex> 1 |> form_enumerate
      {:ok, [1]}

      iex> [1, 2, 3] |> form_enumerate
      {:ok, [[1, 2, 3]]}

      iex> :two |> form_enumerate
      {:ok, [:two]}

      iex> quote do
      ...>   x = 1
      ...>   y = 2
      ...>   z = x + y
      ...> end
      ...> |> form_enumerate
      {:ok, [quote(do: x = 1), quote(do: y = 2), quote(do: z = x + y)]}

      iex> %{a: 1} |> form_enumerate
      {:error, %ArgumentError{message: "expected a form; got: %{a: 1}"}}

  """

  @spec form_enumerate(any) :: {:ok, forms} | {:error, error}

  def form_enumerate(form) do

    with {:ok, form} <- form |> form_validate do

      case form do

        {:__block__, _, args} -> {:ok, args}

        _ -> {:ok, [form]}

      end

    else
      {:error, _} = result -> result
    end

  end

  @doc ~S"""

  `form_enumerate!/1` calls `form_enumerate/1` and if the result is `{:ok, forms}` returns `forms`.

  ## Examples

      iex> quote do
      ...>   x = 1
      ...>   y = 2
      ...>   z = x + y
      ...> end
      ...> |> form_enumerate!
      [quote(do: x = 1), quote(do: y = 2), quote(do: z = x + y)]

  """

  @spec form_enumerate!(any) :: forms | no_return

  def form_enumerate!(form)

  def form_enumerate!(nil), do: nil

  def form_enumerate!(form) do

    with {:ok, forms} <- form |> form_enumerate do
      forms
    else
      {:error, error} -> raise error
    end

  end

  @doc ~S"""
  `form_index_normalise/2` take a value and an optional `default_index` and returns either `{:ok, {form, index}` or `{:error, error}`.

  If the value is already a valid *form_index* it is returned unchanged as `{:ok, {form, index}`.

  If the value is a just a *form*, `{:ok, {form, default_index}}` is returned. The default `default_index` is `0` but can be overidden on the call..

  Both  `form` and `index` (or `default_index`) are validated (using `Macro.validate/1`, `is_integer/1`) and if either fails `{:error, error}` is returned.

  ## Examples

      iex> quote(do: a = x + y)
      ...> |> form_index_normalise
      {:ok, {quote(do: a = x + y), 0}}

      iex> quote(do: a = x + y)
      ...> |> form_index_normalise(-1)
      {:ok, {quote(do: a = x + y), -1}}

      iex> {:error, error} = %{a: 1}
      ...> |> form_index_normalise
      ...> match?(%ArgumentError{message: "expected a form; got: %{a: 1}"}, error)
      true

      iex> {:error, error} = {quote(do: a = x + y), :this_is_not_a_valid_index}
      ...> |> form_index_normalise
      ...> match?(%ArgumentError{message: "expected a valid index; got: :this_is_not_a_valid_index"}, error)
      true

  """

  @spec form_index_normalise(any, any) :: {:ok, {form, integer}} | {:error, error}

  def form_index_normalise(value, default_index \\ 0) do

    value
    |> case do
         {form, index} -> {form, index}
         form -> {form, default_index}
       end
    |> case do

         {form, index} when is_integer(index) ->

           case form |> Macro.validate do
             :ok -> {:ok, {form, index}}
             _ -> {:error, %ArgumentError{message: "expected a form; got: #{inspect form}"}}
           end

         {_form, index} -> {:error, %ArgumentError{message: "expected a valid index; got: #{inspect index}"}}
       end

  end

  @doc ~S"""

  `form_index_normalise!/1` calls `form_index_normalise/1` and if the result is `{:ok, form_index}` returns `form_index`.

  ## Examples

      iex> quote(do: a = x + y)
      ...> |> form_index_normalise!
      {quote(do: a = x + y), 0}

  """

  @spec form_index_normalise!(any) :: {form, integer} | no_return

  def form_index_normalise!(value) do

    with {:ok, form_index} <- value |> form_index_normalise do
      form_index
    else
      {:error, error} -> raise error
    end

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

      iex> quote do
      ...>   x = x + 1
      ...> end
      ...> |> ast_postwalk(fn
      ...>      {:x, _, _} -> Macro.var(:a, nil)
      ...>      # passthru
      ...>      x -> x
      ...> end)
      ...> |> helper_ast_test_forms_result_texts(binding: [a: 42])
      {:ok, {43, ["a = a + 1"]}}

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
      ...> {:ok, result} = ast |> helper_ast_test_forms_result_texts(binding: [a: 42])
      ...> result |> Tuple.insert_at(0, acc) # add the accumulator
      {7, 1844, ["(a = a + 1\n a = a * a\n a = a - 5)"]}

  """

  @spec ast_postwalk(ast, ast_fun_or_acc_fun_tuple) :: ast_or_ast_acc_tuple

  def ast_postwalk(ast, value \\ nil)

  def ast_postwalk(ast, nil) do
    ast
  end

  def ast_postwalk(ast, {acc, fun})
  when is_function(fun, 2) do

    {ast, acc} = ast
    |> Macro.postwalk(acc, fun)

    {ast, acc}

  end

  def ast_postwalk(ast, fun)
  when  is_function(fun, 1) do

    ast
    |> Macro.postwalk(fun)

  end

  @doc ~S"""
  `ast_prewalk/2` takes the same arguments as `ast_postwalk/2` and works in an equivalen mannner to the latter.
  """

  @spec ast_prewalk(ast, ast_fun_or_acc_fun_tuple) :: ast_or_ast_acc_tuple

  # header
  def ast_prewalk(ast, value \\ nil)

  def ast_prewalk(ast, nil) do
    ast
  end

  def ast_prewalk(ast, {acc, fun}) when is_function(fun, 2) do

    {ast, acc} = ast
    |> Macro.prewalk(acc, fun)

    {ast, acc}

  end

  def ast_prewalk(ast, fun) when is_function(fun, 1) do

    ast
    |> Macro.prewalk(fun)

  end

  defdelegate ast_validate!(ast), to: __MODULE__, as: :form_validate!

  defdelegate asts_validate!(asts), to: __MODULE__, as: :forms_validate!

  @doc ~S"""

  `asts_enumerate/1` takes zero (nil), one or more asts, passes each ast to `ast_enumerate/1`, and "flat_maps" the results.

  ## Examples

      iex> nil |> asts_enumerate
      []

      iex> 1 |> asts_enumerate
      [1]

      iex> :two |> asts_enumerate
      [:two]

      iex> [1, nil, :two, nil, "tre"] |> asts_enumerate
      [1, :two, "tre"]

      iex> quote do
      ...>   x = 1
      ...>   y = 2
      ...>   z = x + y
      ...> end
      ...> |> asts_enumerate
      [quote(do: x = 1), quote(do: y = 2), quote(do: z = x + y)]

      iex> [quote do
      ...>   x = 1
      ...>   y = 2
      ...>   z = x + y
      ...> end,
      ...> nil,
      ...> quote(do: a = 42),
      ...> nil,
      ...> quote do
      ...>   b = 7
      ...>   c = a - b
      ...> end]
      ...> |> asts_enumerate
      [quote(do: x = 1), quote(do: y = 2), quote(do: z = x + y),
       quote(do: a = 42), quote(do: b = 7), quote(do: c = a - b)]

      iex> %{a: 1} |> asts_enumerate
      ** (ArgumentError) expected an ast; got: %{a: 1}

  """

  @spec asts_enumerate(asts) :: asts

  def asts_enumerate(asts) do
    asts
    |> list_wrap_flat_just
    |> Enum.flat_map(fn ast -> ast_enumerate(ast) end)
  end

  @doc ~S"""

  `ast_enumerate/1` takes an ast and, if a `:__block__`, returns the list of args.

  If not a  `:__block__`, the ast is returned in a list.

  ## Examples

      iex> 1 |> ast_enumerate
      [1]

      iex> :two |> ast_enumerate
      [:two]

      iex> quote do
      ...>   x = 1
      ...>   y = 2
      ...>   z = x + y
      ...> end
      ...> |> ast_enumerate
      [quote(do: x = 1), quote(do: y = 2), quote(do: z = x + y)]

      iex> %{a: 1} |> ast_enumerate
      ** (ArgumentError) expected an ast; got: %{a: 1}

  """

  @spec ast_enumerate(ast) :: asts

  def ast_enumerate(ast) do

    case Macro.validate(ast) do

      :ok ->

        case ast do

          x when is_tuple(x) ->

            case elem(ast, 0) do

              # if a block the args are the individual "statements"
              :__block__ -> elem(ast, 2) |> asts_enumerate

              # just an ast but must return a list
              _ -> [ast]

            end

          _ -> [ast]

        end
      _ ->

        message = "expected an ast; got: #{inspect ast}"
        Logger.error message
        raise ArgumentError, message: message

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

  ## Examples

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

  `asts_reduce/1` takes zero, one or more asts and reduces them to a single
  ast using `Kernel.SpecialForms.unquote_splicing/1`.

  The list is first flattened and any `nils` removed before splicing.

  An empty list reduces to nil.

  ## Examples

      iex> quote(do: a = x + y)
      ...> |> asts_reduce
      ...> |> helper_ast_test_forms_result_texts(binding: [x: 42, y: 8, c: 5])
      {:ok, {50, ["a = x + y"]}}

      iex> [quote(do: a = x + y),
      ...>  quote(do: a * c)]
      ...> |> asts_reduce
      ...> |> helper_ast_test_forms_result_texts(binding: [x: 42, y: 8, c: 5])
      {:ok, {250, ["(a = x + y\n a * c)"]}}

      iex> nil
      ...> |> asts_reduce
      ...> |> helper_ast_test_forms_result_texts(binding: [x: 42, y: 8, c: 5])
      {:ok, {nil, [""]}}

      iex> [
      ...>   quote(do: a = x + y),
      ...>   nil,
      ...>   [
      ...>    quote(do: b = a / c),
      ...>    nil,
      ...>    quote(do: d = b * b),
      ...>   ],
      ...>   quote(do: e = a + d),
      ...> ]
      ...> |> asts_reduce
      ...> |> helper_ast_test_forms_result_texts(binding: [x: 42, y: 8, c: 5])
      {:ok, {150.0, ["(a = x + y\n b = a / c\n d = b * b\n e = a + d)"]}}

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

       iex> [
       ...>   Macro.var(:x, nil),
       ...>   quote(do: fn x -> x * x end.()),
       ...>   quote(do: List.wrap)
       ...> ]
       ...> |> asts_pipe
       ...> |> helper_ast_test_forms_result_texts(binding: [x: 42])
       {:ok, {[1764], ["x |> (fn x -> x * x end).() |> List.wrap()"]}}

  This example show what happens when an index of 2 is used to insert
  the value of `x` (42) as the 3rd argument in the call to a "partial" anonymous
  function which already has the 1st, 2nd and 4th arguments.

       iex> [
       ...>   Macro.var(:x, nil),
       ...>   {quote(do: fn p, q, x, y -> [{y, x}, {p,q}] end.(:p, :this_is_q, "y")), 2},
       ...>   quote(do: Enum.into(%{}))
       ...> ]
       ...> |> asts_pipe
       ...> |> helper_ast_test_forms_result_texts(binding: [x: 42])
       {:ok, {%{:p => :this_is_q, "y" => 42},
        ["(fn p, q, x, y -> [{y, x}, {p, q}] end).(:p, :this_is_q, x, \"y\") |> Enum.into(%{})"]}}

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

      iex> {X1, :f1, []}
      ...> |> ast_from_mfa
      ...> |> helper_ast_test_forms_texts
      {:ok, ["X1.f1()"]}

      iex> {X1, :f1, [1, :two, "tre"]}
      ...> |> ast_from_mfa
      ...> |> helper_ast_test_forms_texts
      {:ok, ["X1.f1(1, :two, \"tre\")"]}

      iex> {X2, :f2, [{1, :two, "tre"}]}
      ...> |> ast_from_mfa
      ...> |> helper_ast_test_forms_texts
      {:ok, ["X2.f2({1, :two, \"tre\"})"]}

      iex> {X2, :f2, [{1, :two, "tre"} |> Macro.escape]}
      ...> |> ast_from_mfa
      ...> |> helper_ast_test_forms_texts
      {:ok, ["X2.f2({1, :two, \"tre\"})"]}

      iex> {X3, :f3, [%{a: 1, b: %{b: 2}, c: {3, :tre, "tre"}}]}
      ...> |> ast_from_mfa
      ...> |> helper_ast_test_forms_texts
      {:ok, ["X3.f3(%{a: 1, b: %{b: 2}, c: {3, :tre, \"tre\"}})"]}

      iex> {X3, :f3, [%{a: 1, b: %{b: 2}, c: {3, :tre, "tre"}} |> Macro.escape]}
      ...> |> ast_from_mfa
      ...> |> helper_ast_test_forms_texts
      {:ok, ["X3.f3(%{a: 1, b: %{b: 2}, c: {3, :tre, \"tre\"}})"]}

  """

  def ast_from_mfa(mfa) do
    mfa |> asts_from_mfas |> List.first
  end

  @doc false

  @spec list_wrap_flat_just(any) :: [any]

  defp list_wrap_flat_just(value) do
    value
    |> List.wrap
    |> List.flatten
    |> Enum.reject(&is_nil/1)
  end

end

