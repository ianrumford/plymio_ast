defmodule Plymio.Ast.Signature do

  @moduledoc ~S"""
  Utility Functions for Creating Quoted Function Signatures (a list of Asts).

  ## The Signature DSL

 `signature_create/2` implements a trivially simple [*dsl*](http://elixir-lang.org/getting-started/meta/domain-specific-languages.html) for creating unquotable signatures.

  It always returns a list of unquotable values (asts) expected to be used with
  `Kernel.SpecialForms.unquote_splicing/1`.

  The `signature` passed to `signature_create/2` must be one or more `signature items`.

  Each `signature item` results in one unquotable `argument` and can have one of a number of forms:

  ### Signature Item - item (`Atom`)

  When the item is an atom, and the item is a key in the `unquotables` (see [the signature options](#module-the-signature-options)), the value is returned,
  else the argument will be the the result of `Macro.var(item, nil)`.

      iex> :arg0 |> helper_signature_create_to_string
      "(arg0)"

      iex> [:arg0, :arg1, :arg2] |> helper_signature_create_to_string
      "(arg0, arg1, arg2)"

      iex> unquotables = %{arg1: Macro.var(:arg111, nil), arg3: Macro.var(:arg3, nil)}
      ...> [:arg0, :arg1, :arg2]
      ...> |> helper_signature_create_to_string(unquotables: unquotables)
      "(arg0, arg111, arg2)"

  ### Signature Item - ast (3tuple)

  If the item is already a valid ast, it will be used directly.

      iex> Macro.var(:arg42, nil) |> helper_signature_create_to_string
      "(arg42)"

      iex> unquotables = %{arg1: Macro.var(:arg111, nil), arg3: Macro.var(:arg3, nil)}
      ...> [Macro.var(:arg42, nil), :arg1, :arg2]
      ...> |> helper_signature_create_to_string(unquotables: unquotables)
      "(arg42, arg111, arg2)"

  ### Signature Item - {value, default}

  When the item is a 2tuple, the first element is resolved as before.

  The second element is taken as the default value and is broadly resolved in the same way as the first element:

  When the default is an atom it is first looked up in the `:unquotables` (if supplied) or escaped (if necessary).

      iex> {:arg0, 42} |> helper_signature_create_to_string
      "(arg0 \\\\ 42)"

      iex> {:arg0, :arg0_default} |> helper_signature_create_to_string
      "(arg0 \\\\ :arg0_default)"

      iex> {:arg0, %{a: 1}} |> helper_signature_create_to_string
      "(arg0 \\\\ %{a: 1})"

      iex> unquotables = %{
      ...>   arg1: Macro.var(:arg111, nil), arg3: Macro.var(:arg3, nil),
      ...>   default_x1: :x1, default_y2: %{a: 1}, default_z3: {1, :two, "tre"}}
      ...> [{:arg0, :default_z3}, :arg1, {:arg2, :default_x1}]
      ...> |> helper_signature_create_to_string(unquotables: unquotables)
      "(arg0 \\\\ {1, :two, \"tre\"}, arg111, arg2 \\\\ :x1)"

  When the default is already an ast it is used directly:

      iex> default_ast = %{a: 1} |> Macro.escape
      ...> {:arg0, default_ast} |> helper_signature_create_to_string
      "(arg0 \\\\ %{a: 1})"

  There are a few special (`Atom`) defaults for convenience

      iex> {:arg0, :empty_map} |> helper_signature_create_to_string
      "(arg0 \\\\ %{})"

      iex> {:arg0, :empty_list} |> helper_signature_create_to_string
      "(arg0 \\\\ [])"

      iex> {:arg0, :empty_tuple} |> helper_signature_create_to_string
      "(arg0 \\\\ {})"

  `true`, `false` and `nil` work as expected

      iex> {:arg0, nil} |> helper_signature_create_to_string
      "(arg0 \\\\ nil)"

      iex> {:arg0, true} |> helper_signature_create_to_string
      "(arg0 \\\\ true)"

      iex> {:arg0, false} |> helper_signature_create_to_string
      "(arg0 \\\\ false)"

  ## The Signature Options

  The options that can be given to `signature_create/2` are:

  ### Signature Options - `:unquotables`

  If given, the `:unquotables` option must be a map where the keys are atoms and the values an ast.

  The map is used as a dictionary to look up `signature item` that are atoms.

  > Note, for convenience, any non valid ast values are escaped (`Macro.escape/1`)

  """

  alias Plymio.Ast.Utils, as: PAU
  alias Plymio.Ast.Form, as: PAF

  @type unquotable :: Macro.t

  @type unquotables_map_key :: atom
  @type unquotables_map_value :: unquotable
  @type unquotables_map :: %{optional(unquotables_map_key) => unquotables_map_value}

  @type signature_entity ::
  unquotable |
  unquotables_map_key

  @type signature_default ::
  nil |
  false |
  true |
  :empty_map |
  :empty_list |
  :empty_tuple |
  unquotables_map_key |
  unquotable

  @type signature_item ::
  signature_entity |
  {signature_entity, signature_default}

  @type signature :: signature_item | [signature_item]

  @type signature_option :: {:unquotables, unquotables_map}

  @type signature_options :: [signature_option]

  @type signature_ast :: [unquotable]

  # header
  defp signature_resolve_argument(key, unquotables)

  # lookup
  defp signature_resolve_argument(key, unquotables) when is_atom(key) do
    case unquotables |> Map.has_key?(key)do
      true -> unquotables |> Map.fetch!(key)
      _ -> key |> Macro.var(nil)
    end
  end

  # fun
  defp signature_resolve_argument(key, unquotables) when is_function(key) do
    key.(unquotables)
  end

  # default => escape
  defp signature_resolve_argument(key, _unquotables) do
    key |> Macro.escape
  end

  defp signature_resolve_default(default, unquotables)

  # specials
  defp signature_resolve_default(:empty_map, _unquotables), do: Macro.escape(%{})
  defp signature_resolve_default(:empty_list, _unquotables), do: Macro.escape([])
  defp signature_resolve_default(:empty_tuple, _unquotables), do: Macro.escape({})

  defp signature_resolve_default(default, nil) when is_atom(default) do
    default
  end

  # default is a var?
  defp signature_resolve_default({_,_,_} = default, _unquotables) do
    default
    |> PAF.maybe_ast_escape
  end

  defp signature_resolve_default(default, unquotables) when is_atom(default) do
    case unquotables |> Map.has_key?(default) do
      true -> unquotables |> Map.fetch!(default)
      # leave alone
      _ -> default
    end
  end

  # list
  defp signature_resolve_default(default, unquotables) when is_list(default) do
    default
    |> Enum.map(fn item -> item |> signature_resolve_default(unquotables) end)
  end

  # default = hand off to argument resolution
  defp signature_resolve_default(default, unquotables) do
    signature_resolve_argument(default, unquotables)
  end

  defp signature_create_argument(unquotables, name, opts \\ [])

  defp signature_create_argument(_unquotables, :anon, _opts) do
    quote do: _
  end

  # if a 3tuple assume a pre-made var
  defp signature_create_argument(_unquotables, {_, _, _} = entity, _opts), do: entity

  # atom but no unquotables => var
  defp signature_create_argument(unquotables, entity, _opts)
  when is_atom(entity) and is_nil(unquotables) do
    entity |> Macro.var(nil)
  end

  # atom => an unquotable or var
  defp signature_create_argument(unquotables, entity, opts) when is_atom(entity) do

    #unquotables |> Map.fetch!(entity)

    case unquotables |> Map.has_key?(entity) do

      true -> unquotables |> Map.fetch!(entity)

      # not found.  any remediation fun? else a var
      _ ->

          case opts |> Keyword.get(:fn_missing_unquotable) do
            fun when fun != nil -> fun.(unquotables, entity)
            _ ->
              entity |> Macro.var(nil)
          end

    end

  end

  # arg with a default
  defp signature_create_argument(unquotables, {name, default} = _entity, _opts) do

    argument_var = unquotables |> signature_create_argument(name)

    default_var = default |> signature_resolve_default(unquotables)

    quote do
      unquote(argument_var) \\ unquote(default_var)
    end

  end

  @doc ~S"""

  See above for the explanation of the arguments to `signature_create/2` and more examples.

  The example below repeat some of the above but show both the returned signature ast list and then the result of calling `Macro.to_string/1`.

  > Note `signature_create/2` ensures the the returned signature is a valid ast, its does not attempt to validate its syntax: that is left to the compiler.

  ## Examples

  In these examples, an explicit default (e.g. `arg0 \\ 42`) for the argument is provided:

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil)}
      ...> {:arg1, 42}
      ...> |> helper_signature_create(unquotables: unquotables)
      [{:\\, [], [{:arg1, [], nil}, 42]}]

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil)}
      ...> {:arg1, 42}
      ...> |> helper_signature_create_to_string(unquotables: unquotables)
      "(arg1 \\\\ 42)"

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil)}
      ...> [:arg0, {:arg1, nil}]
      ...> |> helper_signature_create(unquotables: unquotables)
      [{:arg0, [], nil}, {:\\, [], [{:arg1, [], nil}, nil]}]

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil)}
      ...> [:arg0, {:arg1, nil}]
      ...> |> helper_signature_create_to_string(unquotables: unquotables)
      "(arg0, arg1 \\\\ nil)"

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil)}
      ...> [{:arg0, %{a: 1}}, {:arg1, true}]
      ...> |> helper_signature_create(unquotables: unquotables)
      [{:\\, [], [{:arg0, [], nil}, {:%{}, [], [a: 1]}]}, {:\\, [], [{:arg1, [], nil}, true]}]

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil)}
      ...> [{:arg0, %{a: 1}}, {:arg1, true}]
      ...> |> helper_signature_create_to_string(unquotables: unquotables)
      "(arg0 \\\\ %{a: 1}, arg1 \\\\ true)"

  In these examples, the default is a key in the quotables map

      iex> unquotables = %{arg1: Macro.var(:arg1, nil), default: 42}
      ...> {:arg1, :default}
      ...> |> helper_signature_create(unquotables: unquotables)
      [{:\\, [], [{:arg1, [], nil}, 42]}]

      iex> unquotables = %{arg1: Macro.var(:arg1, nil), default: 42}
      ...> {:arg1, :default}
      ...> |> helper_signature_create_to_string(unquotables: unquotables)
      "(arg1 \\\\ 42)"

      iex> unquotables = %{
      ...>   arg1: Macro.var(:arg1, nil),
      ...>   def42: 42,
      ...>   defmap1: Macro.escape(%{x: 99})}
      ...> [{Macro.var(:arg2, nil), :def42}, {:arg1, :defmap1}]
      ...> |> helper_signature_create(unquotables: unquotables)
      [{:\\, [], [{:arg2, [], nil}, 42]}, {:\\, [], [{:arg1, [], nil}, {:%{}, [], [x: 99]}]}]

      iex> unquotables = %{
      ...>   arg1: Macro.var(:arg1, nil),
      ...>   def42: 42,
      ...>   defmap1: Macro.escape(%{x: 99})}
      ...> [{Macro.var(:arg2, nil), :def42}, {:arg1, :defmap1}]
      ...> |> helper_signature_create_to_string(unquotables: unquotables)
      "(arg2 \\\\ 42, arg1 \\\\ %{x: 99})"

  In these examples, a pre-existing ast is supplied

       iex> Macro.var(:arg2, nil) |> helper_signature_create
       [{:arg2, [], nil}]

       iex> {Macro.var(:arg2, nil), 42} |> helper_signature_create
       [{:\\, [], [{:arg2, [], nil}, 42]}]

  In these examples, a range of signature values are used

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil), def42: 42}
      ...> [{:arg0, %{a: 1}}, :arg1, {Macro.var(:arg2, nil), :def42}]
      ...> |> helper_signature_create_to_string(unquotables: unquotables)
      "(arg0 \\\\ %{a: 1}, arg1, arg2 \\\\ 42)"

   In these examples some of the "convenience" values for the signature are used for defaults

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil)}
      ...> {:arg1, :empty_list} |> helper_signature_create(unquotables: unquotables)
      [{:\\, [], [{:arg1, [], nil}, []]}]

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil)}
      ...> {:arg1, :empty_list} |> helper_signature_create_to_string(unquotables: unquotables)
      "(arg1 \\\\ [])"

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil)}
      ...> {:arg1, :empty_map}
      ...> |> helper_signature_create(unquotables: unquotables)
      [{:\\, [], [{:arg1, [], nil}, {:%{}, [], []}]}]

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil)}
      ...> {:arg1, :empty_map}
      ...> |> helper_signature_create_to_string(unquotables: unquotables)
      "(arg1 \\\\ %{})"

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil)}
      ...> {:arg1, :empty_tuple}
      ...> |> helper_signature_create(unquotables: unquotables)
      [{:\\, [], [{:arg1, [], nil}, {:{}, [], []}]}]

      iex> unquotables = %{arg0: Macro.var(:arg0, nil), arg1: Macro.var(:arg1, nil)}
      ...> {:arg1, :empty_tuple}
      ...> |> helper_signature_create_to_string(unquotables: unquotables)
      "(arg1 \\\\ {})"

  """

  @spec signature_create(signature, signature_options) :: signature_ast
  def signature_create(signature, opts \\ []) do

    # unquotables = opts

    unquotables =
      case opts |> Keyword.get(:unquotables) do

        x when is_map(x) ->

         x
         |> Enum.map(fn {k,v} -> {k, v |> PAF.maybe_ast_escape} end)
         |> Enum.into(%{})

        x when is_nil(x) -> nil

      end

    signature
    |> List.wrap
    |> Enum.map(fn arg -> unquotables |> signature_create_argument(arg, opts) end)
    |> PAU.ast_validate!

  end

end

