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

if it has been installed by `dune install` you can run it directly:

```sh
pirc [options] <file>
``` 

E.g.:

```sh
cd examples
dune exec pirc -- -ast-dump json ex1.pir
pirc -ast-dump json ex2.pir
```

Then `pirc` will parse the file and dump the ASTs to the current directory (in pretty-printed format by default).

## Options

- `-ast-dump <pprint|json>`: Dump the AST in pretty-printed (*default*) or JSON format.

- `-msg-backend <domain|mpi>`: Specify the backend for parallel execution. `domain` (*default*) for [Domain(thread)-based parallelism](https://ocaml.org/manual/parallelism.html), `mpi` uses the Message Passing Interface, often across multiple machines.

- use `-` to read the source code from `stdin`. E.g.:

```sh
cat examples/ex1.pir | dune exec pirc -- -
```

## Pipeline Testing

Tests are added by placing <testcase>.pir and <testcase>.ans files in test/test_src, then running `dune test` in the root directory. The test runner will compile the `.pir` file, run the compiled program, and compare the output to the `.ans` file. If the output matches the `.ans` file, the test passes.

## Contribute

Install dependencies:

```sh
opam install . --deps-only --with-test --with-dev-setup
```

- [Git Guides](https://github.com/git-guides)

    - Name your branch `<your-names>/<feature>`

    - [Make your PR small and focused](https://artsy.github.io/blog/2021/03/09/strategies-for-small-focused-pull-requests/)
  
- [OCaml Programming Guidelines](https://ocaml.org/docs/guidelines)

    - You can format your code using `dune fmt`

    - [Avoid using `open`](https://ocaml.org/docs/guidelines#opening-modules)
 
- You can enable repo-wide githooks to make sure your code is formatted and can build

    ```sh
    chmod +x .githooks/pre-commit
    ```

    ```sh
    git config core.hooksPath .githooks
    ```
