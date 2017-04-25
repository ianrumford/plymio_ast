defmodule Plymio.Ast.Helpers do

  @moduledoc ~S"""
  Miscellaneous Helper Functions
  """

  alias Plymio.Ast.Form, as: PAF

  @type ast :: Macro.t

  @type alias_key :: atom
  @type alias_keys :: alias_key | [alias_key]
  @type alias_value :: nil | alias_keys

  @type aliases_kvs :: [{alias_key, alias_value}]

  @type aliases_tuples :: [{alias_key, alias_key}]
  @type aliases_map :: %{optional(alias_key) => alias_key}

  @type defaults_map :: %{optional(alias_key) => any}

  @type opts :: Keyword.t

  @type dict :: %{optional(alias_key) => any}

  @type maybe_unquotables :: %{optional(atom) => any} | [{atom,any}]
  @type unquotables :: %{optional(atom) => ast}

  @doc false

  @spec ast_edit_var_context_nil(ast) :: ast

  def ast_edit_var_context_nil(ast)

  def ast_edit_var_context_nil({name, ctx, mod}) when is_atom(name) and is_atom(mod)
  and is_list(ctx) and (length(ctx) == 0) do
    {name, [], nil}
  end

  def ast_edit_var_context_nil(ast) do
    ast
  end

  @doc ~S"""
  `aliases_tuples_create/1` takes `Keyword` where the keys are the canonical key names, and their values are zero (nil), one or more aliases for the canonical key.

  A `Keyword` is returned where each key is an alias and its value the canonical key.

  The canonical key also has an entry with the same value.

  ## Examples

      iex> [a: nil, b: [:b1], c: [:c1, :c2, :c3]] |> aliases_tuples_create
      [a: :a, b: :b, b1: :b, c: :c, c1: :c, c2: :c, c3: :c]
  """

  @spec aliases_tuples_create(aliases_kvs) :: aliases_tuples

  def aliases_tuples_create(aliases) do

    aliases
    |> Enum.map(fn

      {k,nil} -> {k,k}

      {k,a} ->

        [k | a |> List.wrap]
        |> Enum.uniq
        |> Enum.map(fn a -> {a,k} end)

    end)
    |> List.flatten

  end

  @doc ~S"""
  `aliases_map_create/1` does the same job as `aliases_tuples_create/1` but returns a map.

  ## Examples

      iex> [a: nil, b: [:b1], c: [:c1, :c2, :c3]] |> aliases_map_create
      %{a: :a, b: :b, b1: :b, c: :c, c1: :c, c2: :c, c3: :c}
  """

  @spec aliases_map_create(aliases_kvs) :: aliases_map

  def aliases_map_create(aliases) do

    aliases
    |> aliases_tuples_create
    |> Enum.into(%{})

  end

  @doc ~S"""
  `struct_kvs_create/2` takes an opts list, together with a defaults map, and returns an opts list where each value is the value of the key in the defaults map (with default `nil`).

  `struct_kvs_create/2` creates an argument suitable for use with `Kernel.defstruct/1`

  ## Examples

      iex> [a: 1, b: :two, c: "tre", d: nil] |> struct_kvs_create(%{a: 42, b: "two"})
      [a: 42, b: "two", c: nil, d: nil]
  """

  @spec struct_kvs_create(opts, defaults_map) :: opts

  def struct_kvs_create(struct_kvs, defaults_map \\ %{})

  def struct_kvs_create(struct_kvs, defaults_map)
  when is_map(defaults_map) and (map_size(defaults_map) == 0) do
    struct_kvs |> Enum.map(fn {k,_v} -> {k,nil} end)
  end

  def struct_kvs_create(struct_kvs, defaults_map)
  when is_map(defaults_map) do
    struct_kvs |> Enum.map(fn {k,_v} -> {k, defaults_map |> Map.get(k)} end)
  end

  @doc ~S"""
  `canon_keys!/2` takes a list of keys together with a lookup dictionary and replaces each key with its canonical value from the dictionary. Unknown keys raise a `KeyError`.

  ## Examples

      iex> [:a, :b, :c] |> canon_keys!(%{a: 1, b: 2, c: 3})
      [1,2,3]

      iex> [:x] |> canon_keys!(%{a: 1, b: 2, c: 3})
      ** (KeyError) key :x not found in: %{a: 1, b: 2, c: 3}
  """

  @spec canon_keys!(alias_keys, dict) :: alias_keys | no_return

  def canon_keys!(keys, dict) when is_map(dict) do
    keys |> Enum.map(fn k -> dict |> Map.fetch!(k) end)
  end

  @doc ~S"""
  `canon_keys/2` takes a list of keys together with a lookup dictionary and replaces each key with its canonical value from the dictionary, returning `{:ok, known_keys_values}`.

  If there are any unknown keys, `{:error, {known_keys_values, unknown_keys}}` is returned.

  ## Examples

      iex> [:a, :b, :c] |> canon_keys(%{a: 1, b: 2, c: 3})
      {:ok, [1,2,3]}

      iex> [:a, :x, :b, :y, :c, :z] |> canon_keys(%{a: 1, b: 2, c: 3})
      {:error, {[1, 2, 3], [:x, :y, :z]}}
  """

  @spec canon_keys(alias_keys, dict) :: {:ok, alias_keys} | {:error, {alias_keys, list}}

  def canon_keys(keys, dict) when is_map(dict) do

    keys
    # split into known and unknown keys
    |> Enum.split_with(fn k -> Map.has_key?(dict, k) end)
    |> case do

         # no unknown keys
         {known_keys, []} ->
           {:ok, known_keys |> canon_keys!(dict)}

         {known_keys, unknown_keys} ->
           {:error, {known_keys |> canon_keys!(dict), unknown_keys}}

       end

  end

  @doc ~S"""

  `maybe_canon_keys/2` takes a list of keys together with a lookup dictionary and, if the key is in the dictionary, replaces it with its value. Unknown keys are passed through unchanged.

  ## Examples

      iex> [:a, :b, :c] |> maybe_canon_keys(%{a: 1, b: 2, c: 3})
      [1, 2, 3]

      iex> [:x, :a] |> maybe_canon_keys(%{a: 1, b: 2, c: 3})
      [:x, 1]
  """

  @spec maybe_canon_keys(alias_keys, dict) :: alias_keys

  def maybe_canon_keys(keys, dict) when is_map(dict) do
    keys
    |> Enum.map(fn k ->
      case dict |> Map.has_key?(k) do
        true -> dict |> Map.fetch!(k)
        _ -> k
      end
    end)
  end

  @doc ~S"""
  `opts_canon_keys!/2` takes an opts list, together with a lookup dictionary and replaces each key with its canonical value from the dictionary. Unknown keys raise a `KeyError`.

  ## Examples

      iex> [a: 1, b: 2, c: 3] |> opts_canon_keys!(%{a: :x, b: :y, c: :z})
      [x: 1, y: 2, z: 3]

      iex> [x: 1, y: 3, z: 3] |> opts_canon_keys!(%{a: 1, b: 2, c: 3})
      ** (KeyError) key :x not found in: %{a: 1, b: 2, c: 3}
  """

  @spec opts_canon_keys!(opts, dict) :: opts | no_return

  def opts_canon_keys!(opts, dict) when is_map(dict) do
    opts |> Enum.map(fn {k,v} -> {dict |> Map.fetch!(k), v} end)
  end

  @doc ~S"""
  `opts_canon_keys/2` takes an opts list, together with a lookup dictionary and replaces each key with its canonical value from the dictionary, returning `{:ok, opts}`.

  If there are any unknown keys, `{:error, {known_opts, unknown_opts}}` is returned.

  ## Examples

      iex> [a: 1, b: 2, c: 3] |> opts_canon_keys(%{a: :x, b: :y, c: :z})
      {:ok, [x: 1, y: 2, z: 3]}

      iex> [a: 11, p: 1, b: 22, q: 2, c: 33, r: 3] |> opts_canon_keys(%{a: :x, b: :y, c: :z})
      {:error, {[x: 11, y: 22, z: 33], [p: 1, q: 2, r: 3]}}
  """

  @spec opts_canon_keys(opts, dict) :: {:ok, opts} | {:error, {opts, opts}}

  def opts_canon_keys(opts, dict) when is_map(dict) do

    opts
    # split into known and unknown keys
    |> Enum.split_with(fn {k,_v} -> Map.has_key?(dict, k) end)
    |> case do

         # no unknown keys
         {known_kvs, []} ->
           {:ok, known_kvs |> opts_canon_keys!(dict)}

         {known_kvs, unknown_kvs} ->
           {:error,
            {known_kvs |> opts_canon_keys!(dict),
            unknown_kvs}}
       end

  end

  @doc ~S"""
  `opts_sort_keys/` takes an opts list, together with a list of sort keys, and returns the opts sorted in the sort keys order. Duplicate keys follow one after another.

  Any keys found but not given in the sort keys follow the sorted keys in the returned opts.

  Any key in the sort list not found in the opts is ignored.

  ## Examples

      iex> [a: 1, b: 2, c: 3, d: 4] |> opts_sort_keys([:c, :a])
      [c: 3, a: 1,  b: 2, d: 4]

      iex> [a: 1, b: 2, c: 3, d: 4] |> opts_sort_keys([:d, :x, :b, :z])
      [d: 4, b: 2, a: 1, c: 3]
  """

  @spec opts_sort_keys(opts, alias_keys) :: opts

  def opts_sort_keys(opts, keys \\ [])

  def opts_sort_keys([], _keys) do
    []
  end

  def opts_sort_keys(opts, []) do
    opts
  end

  def opts_sort_keys(opts, keys) do

    keys
    # add all the opts' keys to the sort order ones
    |> Kernel.++(Keyword.keys(opts))
    |> list_wrap_flat_just_uniq
    |> Enum.flat_map(fn k ->
      opts
      |> Keyword.get_values(k)
      |> Enum.map(fn v -> {k,v} end)
    end)

  end

  @doc ~S"""
  `list_wrap_flat_just/1` wraps a value (if not already a list), flattens and removes `nils` at the *first / top* level.

  ## Examples

      iex> [{:a, 1}, nil, [{:b1, 12}, nil, {:b2, [nil, 22, nil]}], nil, {:c, 3}] |> list_wrap_flat_just
      [a: 1, b1: 12, b2: [nil, 22, nil], c: 3]

      iex> [[[nil, 42, nil]]] |> list_wrap_flat_just
      [42]
  """

  @spec list_wrap_flat_just(any) :: [any]

  def list_wrap_flat_just(value) do
    value
    |> List.wrap
    |> List.flatten
    |> Enum.reject(&is_nil/1)
  end

  @doc ~S"""
  `list_wrap_flat_just_uniq/1` wraps a value (if not already a list), flattens, removes `nils` at
  the *first / top* level, and deletes duplicates (using `Enum.uniq/1`)

  ## Examples

      iex> [{:a, 1}, nil, [{:b1, 12}, nil, {:b2, [nil, 22, nil]}], nil, {:c, 3}, {:a, 1}, {:b1, 12}] |> list_wrap_flat_just_uniq
      [a: 1, b1: 12, b2: [nil, 22, nil], c: 3]

      iex> [nil, [42, [42, 42, nil]], 42] |> list_wrap_flat_just_uniq
      [42]
  """

  @spec list_wrap_flat_just_uniq(any) :: [any]

  def list_wrap_flat_just_uniq(value) do
    value
    |> List.wrap
    |> List.flatten
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq
  end

  @doc ~S"""
  `unquotables_maybe_escape/1` takes a key-value enumerable, validates whether each value is a valid ast (`Macro.validate/1`) and escapes the value (`Macro.escape/1`) if not, returning a `Map`.

  ## Examples

      iex> [a: 1, b: :two, c: "tre"] |> unquotables_maybe_escape
      %{a: 1, b: :two, c: "tre"}

      iex> [a: 1, b: %{b1: 21}, c: {:c, 3, :tre, "tre"}] |> unquotables_maybe_escape
      %{a: 1, b: {:%{}, [], [b1: 21]}, c: {:{}, [], [:c, 3, :tre, "tre"]}}

      iex> [a: 1, b: %{b1: 21}, c: quote(do: {:c, 3, :tre, "tre"})] |> unquotables_maybe_escape
      %{a: 1, b: {:%{}, [], [b1: 21]}, c: {:{}, [], [:c, 3, :tre, "tre"]}}

  """

  @spec unquotables_maybe_escape(maybe_unquotables) :: unquotables

  def unquotables_maybe_escape(maybe_unquotables) do

    maybe_unquotables
    |> Stream.map(fn {k,v} -> {k, v |> PAF.maybe_ast_escape} end)
    |> Enum.into(%{})

  end

end

