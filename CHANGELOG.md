# CHANGELOG

## v1.0.0

### 1. Breaking Changes

#### Plymio.Ast.Utility

Renamed Plymio.Ast.Utils to `Plymio.Ast.Utility` as part of the plan to use consistently singular `defmodule` names.

#### Dropped Plymio.Ast.Helpers

`Plymio.Ast.Helpers` has moved into its own package called [plymio_option](https://hex.pm/packages/plymio_option).

#### Dropped Plymio.Ast.Transform

`Plymio.Ast.Transform` is moving into its own package to be called *plymio_ast_transform*.

#### Dropped Plymio.Ast.Signature

`Plymio.Ast.Signature` is moving into its own package to be called *plymio_ast_fun_sig*.

#### Dropped support for mnemonic functions in Plymio.Ast.Utility

Previously it was possible to give as atom for the function to 
`Plymio.Ast.Utility.ast_postwalk/2`.

This feature has been dropped; the functionally was used previously only with
`Plymio.Ast.Transform` and will move into the new *plymio_ast_transform* package.

### 2. New Features

New functions have been introduced to adopt a "standard" API that returns either `{:ok, value}` or {`:error, error}` where `error` is an `Exception`.  The bang functions raise the `error`.

There are many new functions beginning `form_` or `forms_` that use the standard API. Frequently  `error` will be an `ArgumentError`. The new functions usually "shadow" their peer `ast_` and `asts_` functions.

### 3. Old Functions

It is intended to deprecate functions that can error but do not support the  standard API; they are now "frozen".

## v0.2.0

### 1. Bug Fixes

`Plymio.Ast.Form.maybe_ast_escape/1` was not recognizing a quoted List.

`Plymio.Ast.Utility.ast_postwalk/2` and `Plymio.Ast.Utility.ast_prewalk/2`
now have the `is_tuple/1` constraint on the ast (first argument) removed.

### 2. Enhancements

`Plymio.Ast.Utility.asts_enumerate/1` and
`Plymio.Ast.Utility.asts_enumerate/1` now have docs, doctests and typespecs.

`Plymio.Ast.Helpers` now has additional functions and docs, doctests
and typespecs.

## v0.1.0 - 1st Release
