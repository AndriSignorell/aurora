# String Functions in pharos

pharos provides a family of functions for manipulating and inspecting
character strings, built on top of stringi for Unicode-aware behavior.
They fall into two groups:

**Manipulation** – transform, extract, or reshape string content:

|  |  |
|----|----|
| Function | Purpose |
| [`strAbbr()`](https://andrisignorell.github.io/pharos/reference/strAbbr.md) | Abbreviate strings uniquely |
| [`strAlign()`](https://andrisignorell.github.io/pharos/reference/strAlign.md) | Align strings |
| [`strCap()`](https://andrisignorell.github.io/pharos/reference/strCap.md) | Capitalize strings |
| [`strChop()`](https://andrisignorell.github.io/pharos/reference/strChop.md) | Split a string into a number of sections of defined length |
| [`strExtract()`](https://andrisignorell.github.io/pharos/reference/strExtract.md) | Extract first match from strings |
| [`strExtractBetween()`](https://andrisignorell.github.io/pharos/reference/strExtractBetween.md) | Extract substrings between patterns |
| [`strLeft()`](https://andrisignorell.github.io/pharos/reference/strLeftRight.md) / [`strRight()`](https://andrisignorell.github.io/pharos/reference/strLeftRight.md) | Return the left or the right part of a string |
| [`strPad()`](https://andrisignorell.github.io/pharos/reference/strPad.md) | Pad a string with justification |
| [`strRev()`](https://andrisignorell.github.io/pharos/reference/strRev.md) | Reverse strings |
| [`strSpell()`](https://andrisignorell.github.io/pharos/reference/strSpell.md) | Spell strings using phonetic alphabets |
| [`strSplit()`](https://andrisignorell.github.io/pharos/reference/strSplit.md) | Split strings |
| [`strTrim()`](https://andrisignorell.github.io/pharos/reference/strTrim.md) | Remove leading/trailing whitespace from a string |
| [`strTrunc()`](https://andrisignorell.github.io/pharos/reference/strTrunc.md) | Truncate strings and add ellipses if a string is truncated |
| [`strVal()`](https://andrisignorell.github.io/pharos/reference/strVal.md) | Extract numeric values from strings |

**Information** – inspect properties of strings without changing them:

|  |  |
|----|----|
| Function | Purpose |
| [`strCountW()`](https://andrisignorell.github.io/pharos/reference/strCountW.md) | Count words in strings |
| [`strDist()`](https://andrisignorell.github.io/pharos/reference/strDist.md) | Compute distances between strings |
| [`strIsNumeric()`](https://andrisignorell.github.io/pharos/reference/strIsNumeric.md) | Check if character strings represent numeric values |
| [`strLen()`](https://andrisignorell.github.io/pharos/reference/strLen.md) | String length |
| [`strPos()`](https://andrisignorell.github.io/pharos/reference/strPos.md) | Find position of first occurrence of a string |

## See also

[`base::substr()`](https://rdrr.io/r/base/substr.html)
