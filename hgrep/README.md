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
A limited version of ranges is also supported, e.g. `[a-z]`, `[a-z0-9]`, `[A-Z]`. Only a combination of `0-9`, `a-z`, or `A-Z` can be used inside the brackets.
```
$ stack exec hgrep-exe "ma[a-zA-Z]*" app/Main.hs
```
Regular dot `.` is also a special literal, matching with every character. So, for example regex `ma..` matches with `"main"`, and `(.a)*` matches with `"banana"`.

## Test
One can run the tests via:
```
$ stack test
```
