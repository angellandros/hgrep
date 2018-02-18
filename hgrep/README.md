# hgrep
Haskell Globally search a Regular Expression and Print

## Installation
To build this Haskell project first install latest version of [Stack](https://www.haskellstack.org/). Then:
```bash
$ stack setup
$ stack build
```
Now running the binary is possible through `stack exec`:
```
$ stack exec hgrep-exe regex FILE1 [FILE2 [...]]
```

## Usage
Valid regex operations are `()*·|`, ordered by priority. Interpunct `·` (`'\183'` in Haskell) can be omitted, but by the way is more prior than `*`, e.g.
`"a·(b)*"` = `"a·b*"` = `"ab*"` ≠ `"(ab)*"`.

```
$ stack exec hgrep-exe "m(a|i|n)*" app/Main.hs test/Spec.hs
```

## Test
One can run the tests via:
```
$ stack test
```
