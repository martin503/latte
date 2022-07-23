# Interpreter for Latte

Author: Marcin Mazur\
All instructions should be executed in project directory and after building interpreter:

```bash
cabal build
```

## Run program

```bash
./run.sh <path-to-latte-file>
```

Example:
```bash
./run.sh examples/good/core001.lat
```

## Code Structure

- `examples` - example programs in Latte.
  - `bad`:
    - `bad`  - examples from original Latte
    - `stx`  - wrong syntax
    - `my`   - tests showing specifics of my language
  - `good`:
    - `core` - examples from original Latte
    - `my`   - tests showing specifics of my language

- `src` - modules implementing Latte.
- `app` - main files to run programs/tests.
- `.devcontainer` - in case using vs code, enables building/using image I used
    during dev (it's NOT my image, so I dont take resposibility for it: <https://github.com/vzarytovskii/haskell-dev-env>).

## Tests

- Grammar

    ```bash
    ./test/bad.sh
    ```

- Good Examples

    ```bash
    ./test/good.sh
    ```

- Bad Examples

    ```bash
    ./test/bad.sh
    ```
