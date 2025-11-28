# View options for fixed node separator characters

Preview character options for use as the fixed node separator in SWIGs.

## Usage

``` r
sep_opts(entity = NULL, table = FALSE)
```

## Arguments

- entity:

  The separator name or an HTML entity of the user's choosing. Defaults
  to `NULL`.

- table:

  Logical to show or hide HTML table display of built-in characters.
  Defaults to `FALSE`.

## Details

By default `sep_opts()` returns a named vector listing the built-in
separators. These built-in HTML entities are attached to nicenames for
convenience. If the user provides the nicename for a built-in entity to
the `entity` argument, the function will return the appropriate HTML
code. If the user provides an HTML entity that does not appear in the
built-in menu, the input will be returned verbatim. Users will typically
access this latter functionality via
[`qd_swig()`](https://jrgant.github.io/quickdag/reference/qd_swig.md).
