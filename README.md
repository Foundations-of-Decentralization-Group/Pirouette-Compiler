## Install dependencies

```sh
opam install . --deps-only --with-test
```

## Build pirc

```sh
dune build
```

## Run tests

```sh
dune test
```

## Build and execute

```sh
dune exec pirc -- [options] <file>
```

Then `pirc` will parse the file and dump the ASTs to the current terminal (in pretty-printed format by default).

## Options

- `-<pprint|json|dot>`: Dump the AST in pretty-printed or JSON format, or generate a [DOT](https://graphviz.org/doc/info/lang.html) file for AST visualization.

- use `-` to read the source code from `stdin`. E.g.:

```sh
cat examples/1.pir | dune exec pirc -- -
```

- use `-o <file>` to write the output to a file. E.g.:

```sh
dune exec pirc -- -json examples/1.pir -o out
```

## Documentation

```sh
make docs
```

Then open pdf files in `docs` directory.


## Pipeline Testing

Tests are added by placing `<testcase>.pir` and `<testcase>.ans` files in `test/test_src`, then running `dune test` in the root directory. The test runner will compile the `.pir` file, run the compiled program, and compare the output to the `.ans` file. If the output matches the `.ans` file, the test passes.

## Contribute

- [Git Guides](https://github.com/git-guides)

    - Name your branch `<your-names>/<feature>`

    - [Make your PR small and focused](https://artsy.github.io/blog/2021/03/09/strategies-for-small-focused-pull-requests/)
  
- [OCaml Programming Guidelines](https://ocaml.org/docs/guidelines)

    - You can format your code using `dune fmt`

    - [Avoid using `open`](https://ocaml.org/docs/guidelines#opening-modules)
 
- You can enable repo-wide githooks to make sure your code is formatted and can build

    ```sh
    chmod +x .githooks/pre-push
    ```

    ```sh
    git config core.hooksPath .githooks
    ```
