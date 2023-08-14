# FormatterII

*Version 0.1*

*FormatterII* is a Lua table formatting tool.

## Requirements
- Lua 5.1, limited support for Lua 5.2, even more limited for Lua 5.3,
  - `coroutine` table ought to be available,
- Lua [LPEG](http://www.inf.puc-rio.br/~roberto/lpeg/) library,
- [lrexlib](https://github.com/rrthomas/lrexlib) (optional).

## Usage
- `local formatter = require 'FormatterII'` — require library,
- `formatter.format (format_string, ...)` — format ... values according to `format_string`,
- `local func = formatter.format (format_string); local formatted = func (table)` — create a function `func` formatting its argument according to `format_string`,
- `local config = formatter.config` — get *FormatterII* configuration (mainly, syntax),
- `formatter.config.open, formatter.config.close = '『', '』'` — use Chinese quotation marks for macro delimiters instead of `<<` and `>>`,
- `formatter.config.string = mw.ustring` — replace the standard Lua `string` library with [Scribunto](https://www.mediawiki.org/wiki/Extension:Scribunto)'s `mw.ustring`,
- `formatter.initialise()` — activate changes in FormatterII configuration.

## Format string syntax
### Format string
A *format string* is a plain literal text with optional embedded *macros* (`<<…>>`). If a macro returns `nil`, then the whole format string will return `nil` and this will be propagated all the way up. But an *optional macro* (`<<?…>>`) will return an empty string instead of `nil`.

Any format string has a *context*: the outermost macro's context is the first argument to `format()`; and non-empty selectors move context to the selected fields.

### Macro
Macro syntax can be:
- `<<selector|format string 1|format string 2|…|format string n>>` — each selector instance will be formatted according to the first `format string` that does not return `nil`. This can be used to define callbacks for absent values,
- `<<|format string 1|format string 2|…|format string n>>` — no selector means that the value itself rather than its fields will be output,
- `<<selector>>` — no format means that `selector` values will be output as they are,
- `<<>>` — the current value will be output as is,
- `<<,>>` — the default separator (`p.config.separator = ', '`) will be output, if required,
- `<<,|; >>` — a custom separator (`; ` in this case) will be output, if required,
- `<<?selector…>>` (*optional macro*) — if `selector` yields nothing, the macro will not fail, producing an empty string instead, and not causing the enclosing format string to fail,
- `<<!>>` (*conditional macro*) — if the selector yields nothing, macro containing `<<!>>`,
- `<<!1|format string>>` — if `format string` repeats, macro that contains it will fail.

### Selector
A selector can be:
- simple. Lua and Re expression support `i` flag for case-insensitive matching. All regular expression flavours support the non-standard *condense* flag (`_`) meaning that spaces, hypens and underscores will be ignored. The new context will be created by captures. Below, `'…'` means `'…'` or `"…"`; `/…/` means text in any legitimate regular expression delimiters:
  - empty, meaning the formatted value itself and not changing the context,
  - `<<'key'…>>` — a key to the table. If a key is absent, it will be looked all the way up in the parent tables,
  - `<<dynamic key…>>` — a key to the table that can include macros. If a key is absent, it will be looked all the way up in the parent tables,
  - `<</regular extression/…>>` or `<<pcre/regular extression/…>>` or `<<pcre'regular expression'…>>` — a Perl-compatible regular expression, if available,
  - `<<gnu/regular extression/…>>` or `<<gnu'regular expression'…>>` — a GNU-compatible regular expression, if available,
  - `<<onig/regular extression/…>>` or `<<onig'regular expression'…>>` — an Oniguruma regular expression, if available,
  - `<<posix/regular extression/…>>` or `<<posix'regular expression'…>>` — a POSIX regular expression, if available,
  - `<<tre/regular extression/…>>` or `<<tre'regular expression'…>>` — a TRE regular expression, if available,
  - `<<lua/regular extression/…>>` or `<<lua'regular expression'…>>` — a standard Lua regular expression,
  - `<<re/regular extression/…>>` or `<<re'regular expression'…>>` — an LPEG [Re](http://www.inf.puc-rio.br/~roberto/lpeg/re.html) "regular expression", with some additional features:
    - `<` — back assertion (`lpeg.B`),
    - `~>` — fold capture (`lpeg.Cf`),
    - ``{` `}`` — constant capture (`lpeg.Cc`),
    - `{# #}` — argument capture (`lpeg.Carg`);
  - iterating:
    - `<<#…>>` for `ipairs()`,
    - `<<$…>>` for `pairs()`;
  - function: `<<func (param1, …, paramn)…>>` will call `func` field of the type `function` of the formatted value, passing to it the formatted value and `param1`, …, `paramn`, and producing the returned value of the function;
- composite, ordered by priority, from highest to lowest (order of composition can be changed by parentheses):
  - `<<selector1 selector2…>>` — an intersection of `selector1` and `selector2`,
  - `<<selector1.selector2…>>` — `selector2` applied to each value returned by `selector1`,
  - `<<selector1 * selector2…>>` — a Cartesian product of `selector1` and `selector2`,
  - `<<selector1 + selector2…>>` — values returned by `selector1`, followed by values of `selector2`,
  - `<<selector1 , selector2…>>` — values returned by `selector1`, if any; otherwise values of `selector2`.

If a selector (except iterating ones) is preceded with an equal sign, it is applyied to table values, not keys. This makes the folloging syntax possible: `<<key selector = value selector…>>`.

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
	self		= '',			-- self selector.
	parent		= '..',			-- parent selector.
	unused		= '__unused',	-- a table of unused items.
	escape		= '\\',			-- escape character.
	open		= '<<',			-- macro start.
	pipe		= '|',			-- separator between selector and format string, or between format string and fallback format string.
	close		= '>>',			-- macro end.
	unique		= '!1',			-- unique selector for unrepeatable formats.
	operators	= {				-- selector arithmetics.
		enter		= '.',		-- enter field (change context).
		cartesian	= '*',		-- cartesian product.
		union		= '+',		-- union of selectors.
		first		= ','		-- ordered choice of selectors.
	},
	ipairs		= '#',			-- ipairs() selector.
	pairs		= '$',			-- pairs() selector.
	regex		= 'pcre'		-- the default regular expression flavour.
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
| pcre"PCRE" | `<<pcre"^key(?<no>\d+)$">>` | Value |
| pcre/PCRE/ | `<<pcre/^key(?<no>\d+)$/>>` | Value |
| Absent PCRE key | `<</^key(?<no>\d+)$/>>` | nil |
| Broken PCRE | `<</^key(?<no>\d+$/>>` | pcre regular expression "^key(?<no>\d+$" with flags "" does not compile |
| Re key, re// | `<<re/"key" { [0-9]+ }/>>` | Value |
| Re key, re'' | `<<re'"key" { [0-9]+ }'>>` | Value |
| Re key, re'', case-insensitive | `<<re'"key" { [0-9]+ }'i>>` | Value |
| Absent re key, re'', case-sensitive | `<<re'"key" { [0-9]+ }'>>` | nil |
| Broken re | `<<re/"key" {: [0-9]+ }/>>` | LPEG Re selector "key" {: [0-9]+ } does not compile |
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
| First non-empty: first | `<< /key\d+/, /item\d+/>>` | Value1 |
| First non-empty: second | `<< /item\d+/, /key\d+/>>` | Value1 |
| First non-empty: absent | `<< /item\d+/, /key\d+/>>` | nil |
| Cartesian: non-empty * non-empty | `<< a.# * b.# \|<<@\|(<<1>>,<<2>>)>>: <<1>>:<<2>><<,>>>>` | (1,1): Value1:Item1, (1,2): Value1:Item2, (2,1): Value2:Item1, (2,2): Value2:Item2 |
| **Separators** |
| Separator, default | `<<#\|<<@>>: <<key>><<,>>>>` | 1: Value1, 2: Value2, 3: Value3 |
| Separator, explicit | `<<#\|<<@>>: <<key>><<,\|; >>>>` | 1: Value1; 2: Value2; 3: Value3 |
| Separator, dynamic | `<<#\|<<@>>: <<key>><<,\|<<sep>>>>>>` | 1: Value1; 2: Value2; 3: Value3 |
| Separator, header and footer | `<<\|Header <<#\|<<@>>: <<key>><<,>>>> Footer>>` | Header 1: Value1, 2: Value2, 3: Value3 Footer |
| Separator, fallback | `<<\|Header <<#\|<<@>>: <<key>><<,>>>> Footer\|Fallback>>` | Fallback |
# Credits
*FormatterII* is written by Alexander Mashin.
