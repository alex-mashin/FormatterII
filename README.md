# FormatterII

*Version 0.3*

*FormatterII* is a Lua table querying and formatting tool, in other words, a [template engine](https://en.wikipedia.org/wiki/Template_processor).

## Features
*FormatterII* can:
- query table fields fuzzily, using various regular expressions to filter table fields and values, so that:
  - it is not necessary to know table keys up to capitalisation, spaces, hyphens and underscores and
  - item values can be validated before output,
- parse strings for further re-rendering,
- handle missing data gracefully, avoiding rendering unnecessary headings for empty lists,
- avoid dangling commas (or other separators),
- keep track of data supplied but never output,
- be flexibly configured, changing the very syntax of the format string,
- be run in *MediaWiki* *Scribunto* environment, provided that coroutines, *LPEG* and, optionally, regular expression engines from *lrexlib* are available (see [my fork](https://github.com/alex-mashin/php-luasandbox) of *luasandbox* extension for PHP).

*FormatterII*'s syntax of format strings aims to be concise and natural, as similar to the resulting formatted string, and with as little boilerplate code, as possible; yet flexible and powerful.

*FormatterII* is fully declarative. The only way to run arbitrary Lua code is to add a member function to the formatted table and invoke it from the template.

## Requirements
- Lua 5.1, 5.2, 5.3, 5.4 or LuaJIT,
 - `coroutine` table ought to be available,
- Lua [LPEG](http://www.inf.puc-rio.br/~roberto/lpeg/) library,
- [lrexlib](https://github.com/rrthomas/lrexlib) (optional; needed for GNU, Oniguruma, POSIX, PCRE, PCRE2 and Tre regular expressions). Note that PCRE2 regular expressions are not available, *FormatterII* will attempt to use PCRE, and vice versa.

## Usage
- `local formatter = require 'FormatterII'` — require library,
- `formatter.format (format_string, ...)` — format 	`...` values according to `format_string`,
- `local func = formatter.formatter (format_string); local formatted = func (table)` — create a function `func` formatting its argument according to `format_string` and then call it,
- `local config = formatter.config` — get *FormatterII* configuration (mainly, syntax),
- `formatter.config.open, formatter.config.close = '『', '』'` — use Chinese quotation marks for macro delimiters instead of `<<` and `>>`,
- `formatter.config.string = mw.ustring` — replace the standard Lua `string` library with [Scribunto](https://www.mediawiki.org/wiki/Extension:Scribunto)'s `mw.ustring`,
- `formatter.initialise()` — activate changes in *FormatterII* configuration.

## Format string syntax
### Format string
A *format string* is a plain literal text with optional embedded *macros* (`<<…>>`). Each literal chunk between macros is treated as a `printf()`-style format that is passed to [`string.format ()`](http://www.lua.org/manual/5.1/manual.html#pdf-string.format) function; in particular, if the string is a plain literal, it is returned as is.

If a macro returns `nil`, then the whole format string will return `nil` and this will be propagated all the way up. But an *optional macro* (`<<?…>>`) will return an empty string instead of `nil`.

Any format string has a *context*: the outermost macro's context is the first argument to `format()`; and non-empty selectors move context to the selected fields.

### Macro
Macro syntax can be:
- `<<selector|format string 1|format string 2|…|format string n>>` — each value yielded by `selector` will be formatted according to the first `format string` that does not return `nil`. This can be used to define fallbacks for absent values,
- `<<|format string 1|format string 2|…|format string n>>` — no selector means that the value itself rather than its fields will be output,
- `<<selector>>` — no format means that `selector` values will be output as they are,
- `<<>>` — the current value will be output as is,
- `<<,>>` — the default separator (`p.config.separator = ', '`) will be output between the formatted values yielded by the selector of the containing macro,
- `<<,|; >>` — a custom separator (`; ` in this case) will be output, if required,
- `<<?selector…>>` (*optional macro*) — if `selector` yields nothing, the macro will not fail, producing an empty string instead, and not causing the enclosing format string to fail,
- `<<!>>` (*conditional macro*) — if the selector yields nothing, macro containing `<<!>>` will fail, if even the format containing `<<!>>` is constant. If selector yields data, `<<!>>` will display an empty string,
- `<<!1|format string>>` — if `format string` repeats, macro that contains it will fail. This can be used to prevent repetition of values (emulate a unique selector) (likely to be replaced with a different implementation).

### Selector
A selector can be:
- *simple*. The new context will be created by captures. Below are subtypes of simple selectors:
  - `<<>>`, meaning the formatted value itself and not changing the context,
  - `<<key…>>` or `<<'key'…>>` or `<<"key"…>>` — a key to the table. If a key is absent, it will be looked all the way up in the parent tables, to the globals table `_G`. In particular:
    - `<<__unused>>` is a table of values that were never output,
    - `<<..>>` is the parent table.
    - `<<@>>` is the key of the currently selected value in the parent table.
    - `<<@@>>` is the counter of the current row returned by table iterator. It can be used to distinguish table items by some natural order, in which they are expected.
  - `<<dynamic key…>>` — a key to the table that can include macros. The corresponding value will be yielded. If the key is absent, it will be looked all the way up in the parent tables, to the globals table `_G`:
    - if the returned value is userdata containing a compiled LPEG pattern or grammar or a regular expression from lrexlib, it will be used as `lpeg/…/` or `flavoured_regex/…/` selector respectively, i.e., the table will be matched against it. This allows to reuse complex LPEG or regex patterns,
  - *regular expression*. At least, two flavours (standard Lua and LPEG Re) are available, and more can be made available with *[lrexlib](https://github.com/rrthomas/lrexlib)*. Any pair of matching non-space non-alphanumeric characters can delimit a regular expression, except characters that have a special meaning in the selector syntax (`().:*+-,\|@`); and except quotes (`'"`), if regular expression flavour is not specified, the default one is to be used; but a quoted string without a flavour prefix will be treated as a plain table key. Below, `//` are used as an example of regular expression delimiters. The possible flags include `AiDsxXmUu_`, but some of them may be unavailable to a certain flavour. All regular expression flavours support `i` flag for case-insensitive matching and a the non-standard *condense* flag (`_`) meaning that spaces, hyphens and underscores will be ignored:
    - `<</regular expression/flags…>>` — a regular expression of the flavour set by `formatter.config.regex`, by default, `pcre2`, falling back to `pcre`, if available. Quotes `'"` cannot be used to delimit a regular expression of the default flavour,
    - `<<pcre2/regular expression/flags…>>` or `<<pcre2'regular expression'flags…>>` or `<<pcre2"regular expression"flags…>>` — a [Perl-compatible regular expression v2](https://www.pcre.org/current/doc/html/), falling back to `pcre`, if available,
    - `<<pcre/regular expression/flags…>>` or `<<pcre'regular expression'flags…>>` or `<<pcre"regular expression"flags…>>` — a [Rerl-compatible regular expression v1](https://www.pcre.org/original/doc/html/), falling back to `pcre2`, if available,
    - `<<gnu/regular expression/flags…>>` or `<<gnu'regular expression'flags…>>` or `<<gnu"regular expression"flags…>>` — an extended [GNU-compatible regular expression](https://www.gnu.org/software/grep/manual/html_node/Regular-Expressions.html), if available,
    - `<<onig/regular expression/flags…>>` or `<<onig'regular expression'flags…>>` or `<<onig"regular expression"flags…>>` — an [Oniguruma regular expression](https://github.com/kkos/oniguruma/blob/master/doc/RE), if available, subject to features implemented in *lrexlib*,
    - `<<posix/regular expression/flags…>>` or `<<posix'regular expression'flags…>>` or `<<posix"regular expression"flags…>>` — an extended [POSIX regular expression](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html), if available,
    - `<<tre/regular expression/flags…>>` or `<<tre'regular expression'flags…>>` or `<<tre"regular expression"flags…>>` — an extended [TRE regular expression](https://github.com/laurikari/tre), if available, subject to features implemented in *lrexlib*,
    - `<<lua/regular expression/flags…>>` or `<<lua'regular expression'flags…>>` or `<<lua"regular expression"flags…>>` — a [standard Lua regular expression](https://www.lua.org/pil/20.2.html),
    - `<<re/regular expression/flags…>>` or `<<re'regular expression'flags…>>` or `<<re"regular expression"flags…>>` — an LPEG [Re](http://www.inf.puc-rio.br/~roberto/lpeg/re.html) "regular expression", with some additional features:
      - `<` — back assertion (`lpeg.B`),
      - ``{` `}`` — constant capture (`lpeg.Cc`),
      - `{# #}` — argument capture (`lpeg.Carg`),
      - `{flavour/regex/flags}` — the following text will be matched against `flavour` regular expression with `flags`. If match fails, so will the re selector or its part. If match succeeds, position in the string will advance, and all captures from the regular expression will be captured by re as well, but named captures will become anonymous. If `flavour` is omitted, the default flavour from `formatter.config.regex` will be used,
      - optional `i` flag for case insensitive matching;
  - *iterating*:
    - `<<#…>>` for `ipairs()`,
    - `<<$…>>` for `pairs()` but ordered by keys;
  - *function*: `<<func (param1, …, paramn)…>>` will call `func` field of the type `function` of the formatted value, passing to it the formatted value, `param1`, …, `paramn`, and finally, the whole table, and producing the returned value of the function;
- *composite*, ordered by priority, from highest to lowest (order of composition can be changed by parentheses; priorities and operators are configurable via `formatter.config.operators`):
  - `<<selector1 selector2…>>` — an intersection of `selector1` and `selector2`,
  - `<<selector1 . selector2…>>` — `selector2` applied to each values returned by `selector1`,
  - `<<selector1 : selector2…>>` — `selector1` filtered by `selector2`. Useful in the cases, when it is impossible or difficult to integrate that filter into `selector1`, e.g., `: @@ = 2` to show only the second row returned by `selector1`,
  - `<<selector1 * selector2…>>` — a Cartesian product of `selector1` and `selector2`,
  - `<<selector1 + selector2…>>` — values returned by `selector1`, followed by values of `selector2`,
  - `<<selector1 - selector2…>>` — values returned by `selector1`, except values returned by `selector2`,  
  - `<<selector1 , selector2…>>` — values returned by `selector1`, if any; otherwise values of `selector2`.

If a selector (except iterating ones) is preceded with an equal sign, it is applied to table values, not keys. This makes the following syntax possible: `<<key selector = value selector…>>`.

### Configuration
`formatter.config` contains a table of library configuration variables, mainly describing format string syntax.

Default value:

```lua
formatter.config = {
	string		= string,		-- string library to use.
	condense	= '_',			-- "condense" (ignore whitespaces, hyphens and underscores) flag.
	fillers		= '[-_%s]',		-- characters to ignore when the condense flag is used.
	conditional	= '!',			-- conditional macro flag.
	optional	= '?',			-- optional macro flag.
	separator	= ',',			-- separator macro flag.
	default_separator
				= ', ',			-- default separator.
	key			= '@',			-- key selector.
	counter		= '@@',			-- iterator counter.
	self		= '',			-- self selector.
	parent		= '..',			-- parent selector.
	unused		= '__unused',	-- a table of unused items.
	escape		= '\\',			-- escape character.
	open		= '<<',			-- macro start.
	pipe		= '|',			-- separator between selector and format string, or between format string and fallback format string.
	close		= '>>',			-- macro end.
	unique		= '!1',			-- unique selector for unrepeatable formats.
	operators	= {				-- operators over selectors' symbols and priorities.
		{ ['']	= 'intersect' },
		{ ['.']	= 'enter' },
		{ [':'] = 'filter' },
		{ ['*']	= 'cartesian' },
		{ ['+']	= 'union' },
		{ ['-'] = 'except' },
		{ [',']	= 'first' }
	},
	ipairs		= '#',			-- ipairs() selector.
	pairs		= '$',			-- pairs() selector.
	regex		= 'pcre2',		-- the default regular expression flavour.
	regex_jit	= true,			-- load libraries from lrexlib at first use.
	re			= { 'lualibs/re', 'Module:Re' }
								-- path to re Lua library.
}
```

After changing configuration, call `formatter.initialise()`.

### Examples
| Description | Format | Result |
| --- | --- | --- |
| **Constant format** |
| Present value, constant format | `const string` | const string |
| Absent value, constant format | `const string` | const string |
| **Plain format** |
| Present item, plain format | `<<key>>` | value |
| Present item, plain format, prefix | `"key" is "<<key>>"` | "key" is "value" |
| Absent item, plain format | `<<key>>` | nil |
| Absent item, plain format, prefix | `"key" is "<<key>>"` | nil |
| Plain format with escaped special character | `The value is \\|<<key>>\\|` | The value is \|Value\| |
| `<<>>`, present | `Value is "<<>>"` | Value is "Some value" |
| `<<>>`, nil | `Value is <<>>` | nil |
| `<<>>`, present, const format | `Value is <<\|"there is some value">>` | Value is "there is some value" |
| `<<>>`, present, header and footer in macro | `<<\|the value is "<<>>">>` | the value is "Some value" |
| `<<>>`, present, nested header and footer | `They say <<\|the value is "<<>>">>` | They say the value is "Some value" |
| `<<>>`, present, header and footer | `Header - <<>> - Footer` | Header - Some value - Footer |
| `<<>>`, nil, header and footer | `Header - <<>> - Footer` | nil |
| `<<\|>>`, nil, header and footer | `<<\|Header <<>> Footer>>` | nil |
| **Fallbacks** |
| Absent value with fallback | `<<key\|<<>>\|fallback>>` | fallback |
| Absent value with empty fallback | `<<key\|<<>>\|>>` |  |
| Absent value with fallback, short syntax | `<<?key\|fallback>>` | fallback |
| Absent value with empty fallback, short syntax | `<<?key>>` |  |
| Present value with empty fallback | `<<key\|<<>>\|>>` | Value |
| Present value with empty fallback: short form | `<<?key>>` | Value |
| Present value with non-empty fallback | `<<key\|<<>>\|Fallback>>` | Value |
| Present value with non-empty fallback: short form | `<<?key\|Fallback>>` | Value |
| Absent value with fallback, prefix and suffix | `<<key\|Header <<>> footer\|fallback>>` | fallback |
| Absent value with empty fallback, prefix and suffix | `<<key\|Header <<>> footer\|>>` |  |
| Present value with empty fallback, prefix and suffix | `<<key\|Header <<>> footer\|>>` | Header Value footer |
| Present value with non-empty fallback, prefix and suffix | `<<key\|Header <<>> footer\|Fallback>>` | Header Value footer |
| Empty and non-empty | `<<key>>, <<item>>` | nil |
| Optional empty and non-empty | `<<key\|<<>>\|>>, <<item\|<<>>\|>>` | value,  |
| Optional empty and non-empty: short syntax | `<<?key>>, <<?item>>` | value,  |
| At least one; one present | `<<key1 + key2>>` | Value1 |
| At least one; two present | `<<key1 + key2\|<<>><<,>>>>` | Value1, Value2 |
| At least one; absent | `<<key1 + key2>>` | nil |
| At least one; one present; prefix | `<<key1 + key2\|Header <<>>>>` | Header Value1 |
| At least one; absent; prefix | `<<key1 + key2\|Header <<>>>>` | nil |
| At least one; two present; prefix | `<<\|Header: <<key1 + key2\|<<>><<,>>>>>>` | Header: Value1, Value2 |
| **Conditional format** |
| Conditional constant; present | `<<key\|<<!>>const string>>` | const string |
| Conditional constant; absent | `<<key\|<<!>>const string>>` | nil |
| Conditional constant; absent; fallback | `<<key\|<<!>>const string\|fallback>>` | fallback |
| Conditional expression, first option | `<<key = value1\|<<!>>yes\|no>>` | yes |
| Conditional expression, second option | `<<key = value1\|<<!>>yes\|no>>` | no |
| Conditional separator: a and b | `<<\|<<a>>: <<b>>\|<<a>>\|<<b>>>>` | A: B |
| Conditional separator: a, no b | `<<\|<<b>>>>` | nil |
| Conditional separator: a, no b | `<<\|<<a>>: <<b>>\|<<a>>\|<<b>>>>` | A |
| Conditional separator: no a, b | `<<\|<<a>>: <<b>>\|<<a>>\|<<b>>>>` | B |
| Conditional separator: no a and no b | `<<\|<<a>>: <<b>>\|<<a>>\|<<b>>>>` | nil |
| Conditional separator: a and b, short form | `<<?a>><<a * b\|<<!>>: \|>><<?b>>` | A: B |
| Conditional separator: a, short form | `<<?a>><<a * b\|<<!>>: \|>><<?b>>` | A |
| Conditional separator: b, short form | `<<?a>><<a * b\|<<!>>: \|>><<?b>>` | B |
| **Iteration** |
| `@` numeric | `<<1\|<<@>>: key = <<key>>>>` | 1: key = value |
| `<<#>>`, no separator | `<<#>>` | Onetwothree |
| `<<#>>`, default separator | `<<#\|<<>><<,>>>>` | One, two, three |
| `<<#>>`, custom separator | `<<#\|<<>><<,\|; >>>>` | One; two; three |
| `<<$>>`, default separator | `<<$\|<<>><<,>>>>` | one, three, two |
| `<<$>>`, custom separator | `<<$\|<<>><<,\|; >>>>` | one; three; two |
| `<<#>>`, {} | `<<#>>` | nil |
| `<<#\|format>>` | `<<#\|<<>>, >>` | One, two, three,  |
| `<<1\|format>>`, 2D | `<<1\|Numeral: <<numeral>>, ordinal: <<ordinal>>, >>` | Numeral: one, ordinal: first,  |
| `<<#\|format>>`, 2D | `<<#\|Numeral: <<numeral>>, ordinal: <<ordinal>>, >>` | Numeral: one, ordinal: first, Numeral: two, ordinal: second, Numeral: three, ordinal: third,  |
| `<<#\|format>>`, 2D, custom separator | `<<#\|Numeral: <<numeral>>, ordinal: <<ordinal>><<,\|; >>>>` | Numeral: one, ordinal: first; Numeral: two, ordinal: second; Numeral: three, ordinal: third |
| numeric key | `<<1\|some table>>` | some table |
| `@` numeric | `<<1\|<<@>>>>` | 1 |
| `<<#\|format>>`, 2D, header, `<<@>>` | `<<\|One to three: <<#\|<<@>>: Numeral: <<numeral>>, ordinal: <<ordinal>>, >>>>` | One to three: 1: Numeral: one, ordinal: first, 2: Numeral: two, ordinal: second, 3: Numeral: three, ordinal: third,  |
| `<<#.ordinal>>` | `<<#.ordinal\|<<>>, >>` | first, second, third,  |
| `<<#\|format>>`, 2D, header, `{}` | `<<\|One to three: <<#\|Numeral: <<numeral>>, cardinal: <<ordinal>>, >>>>` | nil |
| `<<#\|format>>`, 2D, header, `{}`, fallback | `<<\|One to three: <<#\|Numeral: <<numeral>>, cardinal: <<ordinal>>, >>\|No items>>` | No items |
| **Selectors** |
| Single-quoted key | `<<'key'>>` | Value |
| Double-quoted key | `<<"key">>` | Value |
| Single-quoted key with spaces | `<<'some key'>>` | Some value |
| Dynamic key | `<<key<<which>>>>` | Value |
| /GNU/ | `<<gnu/^key[0-9]+/>>` | Value |
| /ONIG/ | `<<onig/^key[0-9]+/>>` | Value |
| /POSIX/ | `<<posix/^key[0-9]+/>>` | Value |
| /PCRE/ | `<</^key(?<no>\d+)$/>>` | Value |
| /TRE/ | `<<tre/^(key){~1}/>>` | Value |
| /PCRE/i | `<</^key(?<no>\d+)$/i>>` | Value |
| /PCRE/_ | `<</^key(?<no>\d+)$/_>>` | Value |
| /PCRE/i_ | `<</^key(?<no>\d+)$/i_>>` | Value |
| /PCRE/ and @ | `<</^key(?<no>\d+)$/\|<<@>>: <<no>> - <<>>, >>` | key1: 1 - Value1, key3: 3 - Value3, key2: 2 - Value2,  |
| pcre2"PCRE" | `<<pcre2"^key(?<no>\d+)$">>` | Value |
| pcre"PCRE" | `<<pcre"^key(?<no>\d+)$">>` | Value |
| pcre/PCRE/ | `<<pcre2/^key(?<no>\d+)$/>>` | Value |
| Absent PCRE key | `<</^key(?<no>\d+)$/>>` | nil |
| Broken PCRE | `<</^key(?<no>\d+$/>>` | pcre2 regular expression "^key(?<no>\d+$" with flags "" does not compile: missing closing parenthesis (pattern offset: 15) |
| Re key, re// | `<<re/"key" { [0-9]+ }/>>` | Value |
| Re with PCRE | `<<re~"key" {/\d+/}~>>` | Value1 |
| Re key, re'' | `<<re'"key" { [0-9]+ }'>>` | Value |
| Re key, re'', case-insensitive | `<<re'"key" { [0-9]+ }'i>>` | Value |
| Absent re key, re'', case-sensitive | `<<re'"key" { [0-9]+ }'>>` | nil |
| Broken re | `<<re/"key" {: [0-9]+ }/>>` | LPEG Re selector "key" {: [0-9]+ } does not compile: pattern error near ': [0-9]+ }' |
| Re key, named capture | `<<re/"key" {:no: [0-9]+ :}/\|<<no>>: <<>>>>` | 1: Value |
| Absent re key | `<<re/"key" { [0-9]+ }/>>` | nil |
| PCRE // key and <<>> | `<</^key(?<no>\d+)$/\|<<@>>: <<>>, >>` | key1: Value1, key2: Value2,  |
| lua'pattern' | `<<lua'key%d+'>>` | Value |
| lua/pattern/ | `<<lua/key%d+/>>` | Value |
| lua'pattern', case-insensitive | `<<lua'key%d+'i>>` | Value |
| Absent lua'pattern', case-sensitive | `<<lua'key%d+'>>` | nil |
| Unique constraint | `<</^key\d+$/\|value is <<>><<,>><<!1\|<<>>>>>>` | value is Value1, value is Value2 |
| **Nested tables** |
| Nested tables | `<<key.item>>` | Value |
| Nested tables, regexes | `<</^key$/./^item$/>>` | Value |
| Nested tables, outer absent | `<<item.item>>` | nil |
| Nested tables, upper level as fallback | `<<key\|<<item>>, <<desc>>>>` | Value, Description |
| Filter and counter | `<</^\d+$/ = /^\d+\s*(px)?$/ : @@ = 2 \|Height: <<>>>>` | Height: 50px |
| **Functions** |
| Function | `<<even().#>>` | 102030 |
| Function with (parameter) | `<<divisible_by (3).#>>` | 1530 |
| Function with (<<parameter>>) | `<<divisible_by (<<divider>>).#>>` | 1530 |
| **Composition** |
| Composed selectors | `<</^key/ /\d$/>>` | Value1 |
| Composed selectors, parentheses not changing the order of operations | `<<(/^key/ /\d$/)>>` | Value1 |
| Composition of two equal keys | `<<"key" "key">>` | Value |
| key = value | `<<key = Value>>` | Value |
| /PCRE/ = value | `<</^key\d+$/ = Value1>>` | Value1 |
| = /pcre/ | `<<= /^Value\d+$/\|<<>><<,>>>>` | Value1, Value2 |
| /PCRE/ = /pcre/ | `<</^key\d+$/ = /^Value\d+$/>>` | Value1 |
| Union all | `<< ( set1 + set2 ).# \|<<>><<,>>>>` | Value10, Value11, Value20, Value21 |
| Filter | `<</^key/ := /^Mediocre\|Acceptable\|Good\|Excellent$/ \|<<>><<,>>>>` | Good, Excellent, Mediocre |
| Exception | `<</^key/ -= Bad \|<<>><<,>>>>` | Good, Excellent, Mediocre |
| First non-empty: first | `<< /key\d+/, /item\d+/>>` | Value1 |
| First non-empty: second | `<< /item\d+/, /key\d+/>>` | Value1 |
| First non-empty: absent | `<< /item\d+/, /key\d+/>>` | nil |
| Cartesian: non-empty * non-empty | `<< a.# * b.#\|<<@>>: (<<1>>, <<2>>)<<,>>>>` | 1: (Value1, Item1), 2: (Value1, Item2), 3: (Value2, Item1), 4: (Value2, Item2) |
| **Separators** |
| Separator, default | `<<#\|<<@>>: <<key>><<,>>>>` | 1: Value1, 2: Value2, 3: Value3 |
| Separator, explicit | `<<#\|<<@>>: <<key>><<,\|; >>>>` | 1: Value1; 2: Value2; 3: Value3 |
| Separator, dynamic | `<<#\|<<@>>: <<key>><<,\|<<sep>>>>>>` | 1: Value1; 2: Value2; 3: Value3 |
| Separator, header and footer | `<<\|Header <<#\|<<@>>: <<key>><<,>>>> Footer>>` | Header 1: Value1, 2: Value2, 3: Value3 Footer |
| Separator, fallback | `<<\|Header <<#\|<<@>>: <<key>><<,>>>> Footer\|Fallback>>` | Fallback |
| **printf()-style formatting** |
| Float format, limited precision | `<<no\|%.3f>>` | 3.142 |
# Credits
*FormatterII* is written by Alexander Mashin in 2022-2023. *Lua* and *LPEG* are created by Roberto Ierusalimschy. *lrexlib* is written by Reuben Thomas and Shmuel Zeigerman.
