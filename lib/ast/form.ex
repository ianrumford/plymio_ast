defmodule Plymio.Ast.Form do

  @moduledoc ~S"""
  Utility Functions for Manipulating Asts (Quoted Forms)

  Many functions return either `{:ok, value}` or `{:error, error}` where `error` with be usually an `ArgumentError`
  """

  require Logger

  @type error :: %ArgumentError{}

  @doc ~S"""
  Takes a maybe quoted value and returns the realised value as `{:ok, value}`.

  Otherwise `{:error, error}` is returned.

  Realisation in this context means extracting the underlying ("unquoted") value.

  ## Examples

      iex> 1 |> maybe_form_realise
      {:ok, 1}

      iex> :atom |> maybe_form_realise
      {:ok, :atom}

      iex> "string" |> maybe_form_realise
      {:ok, "string"}

      iex> [1, :atom, "string"] |> maybe_form_realise
      {:ok, [1, :atom, "string"]}

      iex> {:x, 42} |> maybe_form_realise
      {:ok, {:x, 42}}

      iex> ast = {:x, 42} |> Macro.escape
      ...> ast |> maybe_form_realise
      {:ok, {:x, 42}}

      iex> %{a: 1, b: 2, c: 3} |> maybe_form_realise
      {:ok, %{a: 1, b: 2, c: 3}}

      iex> ast = %{a: 1, b: 2, c: 3} |> Macro.escape
      ...> ast |> maybe_form_realise
      {:ok, %{a: 1, b: 2, c: 3}}

      iex> fun = fn x -> x + 5 end
      ...> {:ok, fun} = fun |> maybe_form_realise
      ...> 42 |> fun.()
      47

      iex> ast = "fn x -> x + 5 end" |> Code.string_to_quoted!
      ...> {:ok, fun} = ast |> maybe_form_realise
      ...> 42 |> fun.()
      47

   A map's keys and values are recursively realised:

      iex> ast = %{a: %{a1: 1}, b: %{b21: 21, b22: 22}, c: 3} |> Macro.escape
      iex> ast |> maybe_form_realise
      {:ok, %{a: %{a1: 1}, b: %{b21: 21, b22: 22}, c: 3}}

   The elements of a tuple are recursively realised:

      iex> ast = [{:x, 2, [1,2,3], %{a: %{a1: 1}, b: %{b21: 21, b22: 22}, c: 3}}] |> Macro.escape
      iex> ast |> maybe_form_realise
      {:ok, [{:x, 2, [1,2,3], %{a: %{a1: 1}, b: %{b21: 21, b22: 22}, c: 3}}]}

   The elements of a list are recursively realised:

      iex> ast = [{:x,:y,:z}, [1,2,3], %{a: %{a1: 1}, b: %{b21: 21, b22: 22}, c: 3}] |> Macro.escape
      iex> ast |> maybe_form_realise
      {:ok, [{:x,:y,:z}, [1,2,3], %{a: %{a1: 1}, b: %{b21: 21, b22: 22}, c: 3}]}

  """

  @spec maybe_form_realise(any) :: {:ok, any} | {:error, error}

  def maybe_form_realise(value)

  def maybe_form_realise(value)
  when is_atom(value)
  or is_bitstring(value)
  or is_boolean(value)
  or is_function(value)
  or is_number(value)
  or is_pid(value)
  or is_port(value)
  or is_reference(value) do
    {:ok, value}
  end

  # list with maybe quoted elements
  def maybe_form_realise(value) when is_list(value) do
    value
    |> Enum.reduce_while([], fn
      v, values ->
        case v |> maybe_form_realise do
          {:ok, value} -> {:cont, [value | values]}
          {:error, _} = result -> {:halt, result}
        end
    end)
    |> case do
         {:error, _} = result -> result
         values -> {:ok, values |> Enum.reverse}
       end
  end

  # map with maybe quoted keys and/or values
  def maybe_form_realise(value) when is_map(value) do

    value
    |> Enum.flat_map(fn{k,v} -> [k,v] end)
    |> maybe_form_realise
    |> case do
         {:error, _} = result -> result
         {:ok, values} ->
         {:ok, values |> Enum.chunk_every(2) |> Map.new(fn [k,v] -> {k,v} end)}
       end

  end

  # quoted module attribute - leave alone
  def maybe_form_realise({:@, _, [{attr_name, _, _}]} = value) when is_atom(attr_name) do
    value
  end

  # quoted map
  def maybe_form_realise({:%{}, _, args} = _value) do

    args
    |> maybe_form_realise
    |> case do
         {:error, _} = result -> result
         {:ok, values} -> {:ok, values |> Enum.into(%{})}
       end

  end

  # quoted tuple
  def maybe_form_realise({:{}, _, args} = _value) do

    args
    |> maybe_form_realise
    |> case do
         {:error, _} = result -> result
         {:ok, values} -> {:ok, values |> List.to_tuple}
       end

  end

  def maybe_form_realise({_, _, _} = value) do

    case value |> Macro.validate do

      :ok -> {:ok, value |> Code.eval_quoted([], __ENV__) |> elem(0)}

      _ ->

        value
        |> Tuple.to_list
        |> maybe_form_realise
        |> case do
             {:error, _} = result -> result
             {:ok, values} -> {:ok, values |> List.to_tuple}
           end

    end

  end

  # tuple with maybe quoted elements
  def maybe_form_realise(value) when is_tuple(value) do

    value
    |> Tuple.to_list
    |> maybe_form_realise
    |> case do
         {:error, _} = result -> result
         {:ok, values} -> {:ok, values |> List.to_tuple}
       end

  end

  # default
  def maybe_form_realise(value) do
    {:ok, value}
  end

  @doc ~S"""
  Takes a maybe quoted value, realises it and, if a `Map`, returns
  `{:ok, map}`. Otherwise `{:error, error}` where `error` will be a `BadMapError`.

  If the realised value is a `Keyword`, its is converted to a map and
  `{:ok, map}` returned.

  The keys and values are recursively realised.

      iex> %{a: 1, b: %{b21: 21, b22: 22}, c: 3} |> maybe_form_realise_map
      {:ok, %{a: 1, b: %{b21: 21, b22: 22}, c: 3}}

      iex> ast = %{a: 1, b: %{b21: 21, b22: 22}, c: 3} |> Macro.escape
      iex> ast |> maybe_form_realise_map
      {:ok, %{a: 1, b: %{b21: 21, b22: 22}, c: 3}}

      iex> 42 |> maybe_form_realise_map
      {:error, %BadMapError{term: 42}}

      iex> {:x, 42} |> Macro.escape |> maybe_form_realise_map
      {:error, %BadMapError{term: Macro.escape({:x, 42})}}

  """

  @spec maybe_form_realise_map(any) :: {:ok, map} | {:error, error} | {:error, %BadMapError{}}

  def maybe_form_realise_map(value) do

    value
    |> maybe_form_realise
    |> case do
         {:error, _} = result -> result
         {:ok, realise_value} ->

           cond do
             is_map(realise_value) -> {:ok, realise_value}
             Keyword.keyword?(realise_value) -> {:ok, realise_value |> Enum.into(%{})}
             true -> {:error, %BadMapError{term: realise_value}}
           end

       end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it using
  `maybe_form_realise_map/1`, and if the result is `{:ok, map}`, returns the map,
  else raises `error` in `{:error, error}`.

      iex> %{a: 1, b: %{b21: 21, b22: 22}, c: 3} |> maybe_form_realise_map!
      %{a: 1, b: %{b21: 21, b22: 22}, c: 3}

      iex> ast = %{a: 1, b: %{b21: 21, b22: 22}, c: 3} |> Macro.escape
      iex> ast |> maybe_form_realise_map!
      %{a: 1, b: %{b21: 21, b22: 22}, c: 3}

      iex> 42 |> maybe_form_realise_map!
      ** (BadMapError) expected a map, got: 42

      iex> ast = {:x, 42} |> Macro.escape
      iex> ast |> maybe_form_realise_map!
      ** (BadMapError) expected a map, got: {:x, 42}

  """
  @spec maybe_form_realise_map!(any) :: map | no_return

  def maybe_form_realise_map!(value) do

    value
    |> maybe_form_realise_map
    |> case do

         {:ok, map} when is_map(map) -> map

         {:error, error} ->

           Logger.error Exception.message(error)
           raise error

       end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it and, if a tuple, returns `{:ok, tuple}`.

  Anything else returns `{:error, error}`.

      iex> {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3} |> maybe_form_realise_tuple
      {:ok, {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3}}

      iex> {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3}
      ...> |> Macro.escape
      ...> |> maybe_form_realise_tuple
      {:ok, {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3}}

      iex> {:error, error} = 42 |> maybe_form_realise_tuple
      ...> match?(%ArgumentError{message: "expected a tuple; got: 42"}, error)
      true

      iex> {:error, error} = %{x: 42}
      ...> |> Macro.escape
      ...> |> maybe_form_realise_tuple
      ...> match?(%ArgumentError{message: "expected a tuple; got: %{x: 42}"}, error)
      true

  """

  @spec maybe_form_realise_tuple(any) :: {:ok, tuple} | {:error, error}

  def maybe_form_realise_tuple(value) do

    case maybe_ast_realise(value) do

      tuple when is_tuple(tuple) -> {:ok, tuple}

      x -> {:error, %ArgumentError{message: "expected a tuple; got: #{inspect x}"}}

    end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it using
  `maybe_form_realise_tuple/1`, and if the result is `{:ok, tuple}`, returns the tuple,
  else raises an `ArgumentError` exception.

      iex> {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3} |> maybe_form_realise_tuple!
      {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3}

      iex> {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3}
      ...> |> Macro.escape
      ...> |> maybe_form_realise_tuple!
      {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3}

      iex> 42 |> maybe_form_realise_tuple!
      ** (ArgumentError) expected a tuple; got: 42

      iex> %{x: 42}
      ...> |> Macro.escape
      ...> |> maybe_form_realise_tuple!
      ** (ArgumentError) expected a tuple; got: %{x: 42}

  """

  @spec maybe_form_realise_tuple!(any) :: tuple | no_return

  def maybe_form_realise_tuple!(value) do

    value
    |> maybe_form_realise_tuple
    |> case do

      {:ok, tuple} when is_tuple(tuple) -> tuple

      {:error, error} ->

        Logger.error Exception.message(error)
        raise error

    end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it and, if a function, returns
  `{:ok, function}`.

  Anything else returns `{:error, error}` where `error` is a `BadFunctionError`.

      iex> fun = fn x -> x end
      ...> result = fun |> maybe_form_realise_function
      ...> match?({:ok, ^fun}, result)
      true

      iex> quoted_fun = quote(do: fn x -> x end)
      ...> {:ok, fun} = quoted_fun |> maybe_form_realise_function
      ...> is_function(fun, 1)
      true

      iex> {:error, error} = 42 |> maybe_form_realise_function
      ...> match?(%BadFunctionError{term: 42}, error)
      true

      iex> {:error, error} = {:x, 42} |> Macro.escape
      ...> |> maybe_form_realise_function
      ...> match?(%BadFunctionError{term: {:x, 42}}, error)
      true

  """

  @spec maybe_form_realise_function(any) :: {:ok, fun} | {:error, error} | {:error, %BadFunctionError{}}

  def maybe_form_realise_function(value) do

    value
    |> maybe_form_realise
    |> case do
         {:error, _} = result -> result
         {:ok, fun} ->

           case fun |> is_function do
             true -> {:ok, fun}
             _ -> {:error, %BadFunctionError{term: fun}}
           end

       end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it using
  `maybe_form_realise_function/1`, and, if `{:ok, function}`, returns the
  function, else raises `error` in `{:error, error}`.

      iex> fun = fn x -> x end
      ...> result = fun |> maybe_form_realise_function!
      ...> match?(^fun, result)
      true

      iex> quoted_fun = quote(do: fn x -> x end)
      ...> fun = quoted_fun |> maybe_form_realise_function!
      ...> is_function(fun, 1)
      true

      iex> 42 |> maybe_form_realise_function!
      ** (BadFunctionError) expected a function, got: 42

      iex> {:x, 42}
      ...> |> Macro.escape
      ...> |> maybe_form_realise_function!
      ** (BadFunctionError) expected a function, got: {:x, 42}

  """

  @spec maybe_form_realise_function!(any) :: fun | no_return

  def maybe_form_realise_function!(value) do

    value
    |> maybe_form_realise_function
    |> case do
         {:ok, fun} -> fun
         {:error, error} ->

           Logger.error Exception.message(error)
           raise error

       end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it and, if a *compiled* module, returns `{:ok, module}`.

  Tests whether the module's `__info__` function works to confirm an actual module.

  Anything else returns `{:error, error}`.

      iex> mod = (defmodule ABC1, do: nil) |> elem(1)
      ...> result = mod |> maybe_form_realise_module
      ...> match?({:ok, ^mod}, result)
      true

      iex> mod = (defmodule ABC2, do: nil) |> elem(1)
      ...> quoted_mod = mod |> Macro.escape
      ...> result = quoted_mod |> maybe_form_realise_module
      ...> match?({:ok, ^mod}, result)
      true

      iex> {:error, error} = 42 |> maybe_form_realise_module
      ...> match?(%ArgumentError{message: "expected a module; got: 42"}, error)
      true

      iex> {:error, error} = {:x, 42}
      ...> |> Macro.escape
      ...> |> maybe_form_realise_module
      ...> match?(%ArgumentError{message: "expected a module; got: {:x, 42}"}, error)
      true

  """

  @spec maybe_form_realise_module(any) :: {:ok, atom} | {:error, error}
  def maybe_form_realise_module(value) do

    value
    |> maybe_form_realise
    |> case do

         {:error, _} = result -> result

         {:ok, mod} when is_atom(mod) ->

             try do
               mod.__info__(:functions)
               {:ok, mod}
             rescue
               _any ->
                 {:error, %ArgumentError{message: "expected a module; got: #{inspect mod}"}}
             end

         {:ok, value} ->

           {:error, %ArgumentError{message: "expected a module; got: #{inspect value}"}}

       end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it using
  `maybe_form_realise_module/1`, and, if `{:ok, module}`, returns the
  module, else raises `error` in `{:error, error}`.

      iex> mod = (defmodule ABC3, do: nil) |> elem(1)
      ...> result = mod |> maybe_form_realise_module!
      ...> match?(^mod, result)
      true

      iex> mod = (defmodule ABC, do: nil) |> elem(1)
      ...> quoted_mod = mod |> Macro.escape
      ...> result = quoted_mod |> maybe_form_realise_module!
      ...> match?(^mod, result)
      true

      iex> :an_atom_but_not_a_module |> maybe_form_realise_module!
      ** (ArgumentError) expected a module; got: :an_atom_but_not_a_module

      iex> 42 |> maybe_form_realise_module!
      ** (ArgumentError) expected a module; got: 42

      iex> {:x, 42}
      ...> |> Macro.escape
      ...> |> maybe_form_realise_module!
      ** (ArgumentError) expected a module; got: {:x, 42}

  """

  @spec maybe_form_realise_module!(any) :: atom | no_return

  def maybe_form_realise_module!(value) do

    value
    |> maybe_form_realise_module
    |> case do
         {:ok, mod} -> mod
         {:error, error} ->

        Logger.error Exception.message(error)
        raise error

       end

  end

  @doc ~S"""
  Takes a maybe quoted value and returns the realised value.

  Realisation in this context means extracting the underlying ("unquoted") value.

  ## Examples

      iex> 1 |> maybe_ast_realise
      1

      iex> :atom |> maybe_ast_realise
      :atom

      iex> "string" |> maybe_ast_realise
      "string"

      iex> [1, :atom, "string"] |> maybe_ast_realise
      [1, :atom, "string"]

      iex> {:x, 42} |> maybe_ast_realise
      {:x, 42}

      iex> ast = {:x, 42} |> Macro.escape
      ...> ast |> maybe_ast_realise
      {:x, 42}

      iex> %{a: 1, b: 2, c: 3} |> maybe_ast_realise
      %{a: 1, b: 2, c: 3}

      iex> ast = %{a: 1, b: 2, c: 3} |> Macro.escape
      ...> ast |> maybe_ast_realise
      %{a: 1, b: 2, c: 3}

      iex> fun = fn x -> x + 5 end
      ...> fun = fun |> maybe_ast_realise
      ...> 42 |> fun.()
      47

      iex> ast = "fn x -> x + 5 end" |> Code.string_to_quoted!
      ...> fun = ast |> maybe_ast_realise
      ...> 42 |> fun.()
      47

   A map's keys and values are recursively realised:

      iex> ast = %{a: %{a1: 1}, b: %{b21: 21, b22: 22}, c: 3} |> Macro.escape
      iex> ast |> maybe_ast_realise
      %{a: %{a1: 1}, b: %{b21: 21, b22: 22}, c: 3}

   The elements of a tuple are recursively realised:

      iex> ast = [{:x, 2, [1,2,3], %{a: %{a1: 1}, b: %{b21: 21, b22: 22}, c: 3}}] |> Macro.escape
      iex> ast |> maybe_ast_realise
      [{:x, 2, [1,2,3], %{a: %{a1: 1}, b: %{b21: 21, b22: 22}, c: 3}}]

   The elements of a list are recursively realised:

      iex> ast = [{:x,:y,:z}, [1,2,3], %{a: %{a1: 1}, b: %{b21: 21, b22: 22}, c: 3}] |> Macro.escape
      iex> ast |> maybe_ast_realise
      [{:x,:y,:z}, [1,2,3], %{a: %{a1: 1}, b: %{b21: 21, b22: 22}, c: 3}]

  """

  @spec maybe_ast_realise(any) :: any

  def maybe_ast_realise(value)

  def maybe_ast_realise(value)
  when is_atom(value)
  or is_bitstring(value)
  or is_boolean(value)
  or is_function(value)
  or is_number(value)
  or is_pid(value)
  or is_port(value)
  or is_reference(value) do
    value
  end

  # list with maybe quoted elements
  def maybe_ast_realise(value) when is_list(value) do
    value |> Enum.map(fn v -> v |> maybe_ast_realise end)
  end

  # map with maybe quoted keys and/or values
  def maybe_ast_realise(value) when is_map(value) do

    value
    |> Stream.map(fn
      {k,v} when is_atom(k) -> {k, v |> maybe_ast_realise}
      {k,v} -> {k |> maybe_ast_realise, v |> maybe_ast_realise}
    end)
    |> Enum.into(%{})
  end

  # quoted module attribute - leave alone
  def maybe_ast_realise({:@, _, [{attr_name, _, _}]} = value) when is_atom(attr_name) do
    value
  end

  # quoted map
  def maybe_ast_realise({:%{}, _, args} = _value) do

    args
    |> Enum.into(%{})

    |> maybe_ast_realise

  end

  # quoted tuple
  def maybe_ast_realise({:{}, _, args} = _value) do

    args
    |> Enum.map(fn v -> v |> maybe_ast_realise end)
    |> List.to_tuple

  end

  def maybe_ast_realise({_, _, _} = value) do

    case value |> Macro.validate do
      :ok -> value |> Code.eval_quoted([], __ENV__) |> elem(0)
      _ -> value
    end

  end

  # tuple with maybe quoted elements
  def maybe_ast_realise(value) when is_tuple(value) do

    value
    |> Tuple.to_list
    |> Enum.map(fn v -> v |> maybe_ast_realise end)
    |> List.to_tuple

  end

  # default
  def maybe_ast_realise(value) do
    value
  end

  @doc ~S"""
  Takes a maybe quoted value, realises it and, if a `Map`, returns
  `{:ok, map}`.

  If the realised value is a `Keyword`, its is converted to a map and
  `{:ok, map}` returned.

  Anything else returns `:error.`

  The keys and values are recursively realised.

      iex> %{a: 1, b: %{b21: 21, b22: 22}, c: 3} |> maybe_ast_realise_map
      {:ok, %{a: 1, b: %{b21: 21, b22: 22}, c: 3}}

      iex> ast = %{a: 1, b: %{b21: 21, b22: 22}, c: 3} |> Macro.escape
      iex> ast |> maybe_ast_realise_map
      {:ok, %{a: 1, b: %{b21: 21, b22: 22}, c: 3}}

      iex> 42 |> maybe_ast_realise_map
      :error

      iex> ast = {:x, 42} |> Macro.escape
      iex> ast |> maybe_ast_realise_map
      :error
  """

  @spec maybe_ast_realise_map(any) :: {:ok, map} | :error
  def maybe_ast_realise_map(value) do

    realise_value = maybe_ast_realise(value)

    cond do

      is_map(realise_value) -> {:ok, realise_value}

      Keyword.keyword?(realise_value) -> {:ok, realise_value |> Enum.into(%{})}

      true -> :error

    end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it using
  `maybe_ast_realise_map/1`, and if the result is `{:ok, map}`, returns the map,
  else raises a `BadMapError` exception.

      iex> %{a: 1, b: %{b21: 21, b22: 22}, c: 3} |> maybe_ast_realise_map!
      %{a: 1, b: %{b21: 21, b22: 22}, c: 3}

      iex> ast = %{a: 1, b: %{b21: 21, b22: 22}, c: 3} |> Macro.escape
      iex> ast |> maybe_ast_realise_map!
      %{a: 1, b: %{b21: 21, b22: 22}, c: 3}

      iex> 42 |> maybe_ast_realise_map!
      ** (BadMapError) expected a map, got: :error

      iex> ast = {:x, 42} |> Macro.escape
      iex> ast |> maybe_ast_realise_map!
      ** (BadMapError) expected a map, got: :error

  """
  @spec maybe_ast_realise_map!(any) :: map | no_return
  def maybe_ast_realise_map!(value) do

    realised_value = maybe_ast_realise_map(value)

    case realised_value do

      {:ok, map} when is_map(map) -> map

      :error ->

        message =
          "#{inspect __MODULE__}.maybe_ast_realise_map!: expected a map; got: #{inspect realised_value} value #{inspect value}"
        Logger.error message
        raise BadMapError, term: realised_value

    end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it and, if a tuple, returns `{:ok, tuple}`.

  Anything else returns `:error`.

      iex> {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3} |> maybe_ast_realise_tuple
      {:ok, {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3}}

      iex> {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3}
      ...> |> Macro.escape
      ...> |> maybe_ast_realise_tuple
      {:ok, {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3}}

      iex> 42 |> maybe_ast_realise_tuple
      :error

      iex> %{x: 42}
      ...> |> Macro.escape
      ...> |> maybe_ast_realise_tuple
      :error
  """

  @spec maybe_ast_realise_tuple(any) :: {:ok, tuple} | :error
  def maybe_ast_realise_tuple(value) do

    case maybe_ast_realise(value) do

      tuple when is_tuple(tuple) -> {:ok, tuple}

      _ -> :error

    end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it using
  `maybe_ast_realise_tuple/1`, and if the result is `{:ok, tuple}`, returns the tuple,
  else raises an `ArgumentError` exception.

      iex> {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3} |> maybe_ast_realise_tuple!
      {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3}

      iex> {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3}
      ...> |> Macro.escape
      ...> |> maybe_ast_realise_tuple!
      {:one, 1, %{"two1" => 21, :two2 => 22}, "tre", 3}

      iex> 42 |> maybe_ast_realise_tuple!
      ** (ArgumentError) expected a tuple, got: :error

      iex> %{x: 42}
      ...> |> Macro.escape
      ...> |> maybe_ast_realise_tuple!
      ** (ArgumentError) expected a tuple, got: :error
  """

  @spec maybe_ast_realise_tuple!(any) :: tuple | no_return
  def maybe_ast_realise_tuple!(value) do

    realised_value = maybe_ast_realise_tuple(value)

    case realised_value do

      {:ok, tuple} when is_tuple(tuple) -> tuple

      :error ->

        message = "expected a tuple, got: #{inspect realised_value}"
        Logger.error message
        raise ArgumentError, message: message

    end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it and, if a function, returns
  `{:ok, function}`. Anything else returns `:error`.

      iex> fun = fn x -> x end
      ...> result = fun |> maybe_ast_realise_function
      ...> match?({:ok, ^fun}, result)
      true

      iex> quoted_fun = quote(do: fn x -> x end)
      ...> {:ok, fun} = quoted_fun |> maybe_ast_realise_function
      ...> is_function(fun, 1)
      true

      iex> 42 |> maybe_ast_realise_function
      :error

      iex> {:x, 42} |> Macro.escape
      ...> |> maybe_ast_realise_function
      :error
  """

  @spec maybe_ast_realise_function(any) :: {:ok, fun} | :error
  def maybe_ast_realise_function(value) do

    case maybe_ast_realise(value) do

      fun when is_function(fun) -> {:ok, fun}

      _ -> :error

    end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it using
  `maybe_ast_realise_function/1`, and, if `{:ok, function}`, returns the
  function, else raises a `BadFunctionError` exception.

      iex> fun = fn x -> x end
      ...> result = fun |> maybe_ast_realise_function!
      ...> match?(^fun, result)
      true

      iex> quoted_fun = quote(do: fn x -> x end)
      ...> fun = quoted_fun |> maybe_ast_realise_function!
      ...> is_function(fun, 1)
      true

      iex> 42 |> maybe_ast_realise_function!
      ** (BadFunctionError) expected a function, got: :error

      iex> {:x, 42}
      ...> |> Macro.escape
      ...> |> maybe_ast_realise_function!
      ** (BadFunctionError) expected a function, got: :error

  """

  @spec maybe_ast_realise_function!(any) :: fun | no_return
  def maybe_ast_realise_function!(value) do

    realised_value = maybe_ast_realise_function(value)

    case maybe_ast_realise_function(value) do

      {:ok, fun} when is_function(fun) -> fun

      :error ->

        message =
          "#{inspect __MODULE__}.maybe_ast_realise_function!: expected a function; got: #{inspect realised_value} value #{inspect value}"
        Logger.error message
        raise BadFunctionError, term: realised_value

    end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it and, if a *compiled* module, returns `{:ok, module}`.

  Tests whether the module's `__info__` function works to confirm an actual module.

  Anything else returns `:error`.

      iex> mod = (defmodule XYZ1, do: nil) |> elem(1)
      ...> result = mod |> maybe_ast_realise_module
      ...> match?({:ok, ^mod}, result)
      true

      iex> mod = (defmodule XYZ2, do: nil) |> elem(1)
      ...> quoted_mod = mod |> Macro.escape
      ...> result = quoted_mod |> maybe_ast_realise_module
      ...> match?({:ok, ^mod}, result)
      true

      iex> 42 |> maybe_ast_realise_module
      :error

      iex> {:x, 42}
      ...> |> Macro.escape
      ...> |> maybe_ast_realise_module
      :error
  """

  @spec maybe_ast_realise_module(any) :: {:ok, atom} | :error
  def maybe_ast_realise_module(value) do

    case maybe_ast_realise(value) do

      mod when is_atom(mod) ->

        try do
          mod.__info__(:functions)
          {:ok, mod}
        rescue
          _any ->
            :error
        end

      _ -> :error

    end

  end

  @doc ~S"""
  Takes a maybe quoted value, realises it using
  `maybe_ast_realise_module/1`, and, if `{:ok, module}`, returns the
  module, else raises a `ArgumentError` exception.

      iex> mod = (defmodule XYZ3, do: nil) |> elem(1)
      ...> result = mod |> maybe_ast_realise_module!
      ...> match?(^mod, result)
      true

      iex> mod = (defmodule XYZ4, do: nil) |> elem(1)
      ...> quoted_mod = mod |> Macro.escape
      ...> result = quoted_mod |> maybe_ast_realise_module!
      ...> match?(^mod, result)
      true

      iex> :an_atom_but_not_a_module |> maybe_ast_realise_module!
      ** (ArgumentError) expected a module, got: :error

      iex> 42 |> maybe_ast_realise_module!
      ** (ArgumentError) expected a module, got: :error

      iex> {:x, 42}
      ...> |> Macro.escape
      ...> |> maybe_ast_realise_module!
      ** (ArgumentError) expected a module, got: :error
  """

  @spec maybe_ast_realise_module!(any) :: atom | no_return
  def maybe_ast_realise_module!(value) do

    realised_value = maybe_ast_realise_module(value)

    case realised_value do

      {:ok, mod} when is_atom(mod) -> mod

      :error ->

        message =
          "#{inspect __MODULE__}.maybe_ast_realise_module!: expected a module; got: #{inspect value}"
        Logger.error message
        raise ArgumentError, message: "expected a module, got: #{inspect realised_value}"

    end

  end

  @doc ~S"""
  `maybe_ast_escape/1` escapes (`Macro.escape/1`) any value other than a module attribute or an existing ast (i.e. `Macro.validate/1` returns `:ok`)

  ## Examples

      iex> 42 |> maybe_ast_escape
      42

      iex> :two |> maybe_ast_escape
      :two

      iex> %{a: 1} |> maybe_ast_escape
      {:%{}, [], [a: 1]}

      iex> %{a: 1} |> Macro.escape |> maybe_ast_escape
      {:%{}, [], [a: 1]}

      iex> [1, %{b: 2}, {:c, 2, :tre}] |> maybe_ast_escape
      [1, {:%{}, [], [b: 2]}, {:{}, [], [:c, 2, :tre]}]

      iex> [1, %{b: 2}, {:c, 2, :tre}] |> Macro.escape |> maybe_ast_escape
      [1, {:%{}, [], [b: 2]}, {:{}, [], [:c, 2, :tre]}]
  """

  @spec maybe_ast_escape(any) :: Macro.t
  def maybe_ast_escape(value)

  # quoted module attribute - leave alone
  def maybe_ast_escape({:@, _, _} = value) do
    value
  end

  # already a valid ast? if not, escape
  def maybe_ast_escape(value) do
    case value |> Macro.validate do
      :ok -> value
      _ -> value |> Macro.escape
    end
  end

end

