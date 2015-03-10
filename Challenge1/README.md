# Challenge 1
C source code minimization

## Dependent binaries
* gcc

## Dependent Haskell packages
* Text.Regex.PCRE
* Data.ByteString.Base64
* Data.Tuple.Select
* Language.C

## Transformations made
1. Preprocessing via GCC
2. Remove comments, includes and spaces from the prettified code
3. Find common keywords and functions and replace them with #define shortcodes