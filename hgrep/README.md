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
$ stack exec hrep-exe regex FILE1 [FILE2 [...]]
```

## Usage
Valid regex operations are `()*·|`, ordered by priority. Interpunct `·` can be omitted, but by the way is more prior than `*`, e.g.
`"a·(b)*"` = `"a·b*"` = `"ab*"` ≠ `"(ab)*"`.
