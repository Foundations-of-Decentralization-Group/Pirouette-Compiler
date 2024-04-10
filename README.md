Build `pirc`:

```sh
dune build
```

Run tests:

```sh
dune runtest
```

Build and execute:

```sh
dune exec pirc <file>
```

Then `pirc` will parse the file and print the AST in JSON format. Example code can be found in `examples/`.
