## Build `pirc`:

```sh
dune build
```

## Run tests:

```sh
dune runtest
```

## Build and execute:

```sh
cd examples
dune exec pirc <file>
```

Then `pirc` will parse the file and dump the ASTs to the current directory (in pretty-printed format by default).

## Options

- `-ast-dump <pprint|json>`: Dump the AST in pretty-printed or JSON format.

- use `-` to read the source code from `stdin`. E.g.:

```sh
cat examples/1.pir | dune exec pirc -- -
```
