# FormatterII

*Version 0.1*

FormatterII is a Lua table formatting tool.

## Requirements
- Lua 5.1, limited support for Lua 5.2, even more limited for Lua 5.3,
  - `coroutine` table ought to be available,
- Lua [LPEG](http://www.inf.puc-rio.br/~roberto/lpeg/) library,
- [lrexlib](https://github.com/rrthomas/lrexlib) (optional).

## Usage
- `local formatter = require 'FormatterII'` — require library,
- `formatter.format (table, format_string) — format `table` according to `format_string`,
- `local config = formatter.config` — get FormatterII configuration (mainly, syntax),
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
- `<<!selector|format string>>` (*conditional macro*) — if `selector` yields nothing, macro will fail if even `format string` is constant and does not fail because of `selector`.

### Selector
A selector can be:
- simple (`'…`` means `'…'` or `"…"`; `/…/` means text in any legitimate regular expression delimiters):
  - empty, meaning the formatted value itself and not changing the context,
  - `<<key…>>` — a key to the table,
  - `<</regular extression/…>>` or `<<pcre/regular extression/…>>` or `<<pcre'regular expression'…>>` — a Perl-compatible regular expression, if available,
  - `<<gnu/regular extression/…>>` or `<<gnu'regular expression'…>>` — a GNU-compatible regular expression, if available,
  - `<<onig/regular extression/…>>` or `<<onig'regular expression'…>>` — an Oniguruma regular expression, if available,
  - `<<posix/regular extression/…>>` or `<<posix'regular expression'…>>` — a POSIX regular expression, if available,
  - `<<tre/regular extression/…>>` or `<<tre'regular expression'…>>` — a TRE regular expression, if available,
  - `<<lua/regular extression/…>>` or `<<lua'regular expression'…>>` — a standard Lua regular expression,
  - `<<re/regular extression/…>>` or `<<re'regular expression'…>>` — an LPEG [Re](http://www.inf.puc-rio.br/~roberto/lpeg/re.html) "regular expression", with some additional features:
    - `<` — back assertion (`lpeg.B`),
    - `~>` — fold capture (`lpeg.Cf`),
    - `{`` ``}` — constant capture (`lpeg.Cc`),
    - `{# #}` — argument capture (`lpeg.Carg`);
  - iterating:
    - `<<#…>>` for `ipairs()`,
    - `<<$…>>` for `pairs()`;
  - function: `<<func (param1, …, paramn)…>>` will call `func` field of the type `function` of the formatted value, passing to it the formatted value and `param1`, …, `paramn`, and producing the returned value of the function;
- composite:
  - `<<selector1 selector2…>>` — an intersection of `selector1` and `selector2`,
  - `<<selector1.selector2…>>` — `selector2` applied to each value returned by `selector1`,
  - `<<selector1 * selector2…>>` — a Cartesian product of `selector1` and `selector2`,
  - `<<selector1 + selector2…>>` — values returned by `selector1`, followed by values of `selector2`.

If a selector (except iterating ones) is preceded with an equal sign, it is applyied to table values, not keys. This makes the folloging syntax possible: `<<key selector = value selector…>>`.

### Examples
| Description | Formatted value | Format string | Result |
| --- | --- | --- | --- |
| {}, constant format | { } | `const string` | const string |
| Non-empty item, no format | {key = 'value' } | `"key" is "<<key>>"` | "key" is "value" |
| Empty and non-empty | {key = 'value' } | `<<key>>, <<item>>` | nil |
| Optional empty and non-empty | {key = 'value' } | `<<key>>, <<?item>>` | value,  |
| At least one; present | {key1 = 'Value1' } | `Header <<|<<?key1>><<?key2>>>>` | Header Value1 |
| At least one; absent | { } | `Header <<|<<?key1>><<?key2>>>>` | nil |
| Conditional constant; present | {key = 'Value' } | `<<!key|const string>>` | const string |
| Conditional constant; absent | { } | `<<!key|const string>>` | nil |
| Conditional constant; absent; fallback | { } | `<<!key|const string|fallback>>` | fallback |
| Non-empty item, constant format for item | {key = 'value' } | `"key" is "<<key|fallback>>"` | "key" is "fallback" |
| {}, constant format for item | { } | `"key" is "<<key|fallback>>"` | "key" is "fallback" |
| {}, simple format for item | { } | `"key" is "<<key>>"` | nil |
| nil, simple format for item | nil | `"key" is "<<key>>"` | nil |
| {}, simple format for item, fallback | { } | `"key" is "<<key|<<>>|(there is no key)>>"` | "key" is "(there is no key)" |
| Conditional separator: a and b | {a = 'A', b = 'B' } | `<<|<<a>>: <<b>>|<<a>>|<<b>>>>` | A: B |
| Conditional separator: a, no b | {a = 'A' } | `<<|<<a>>: <<b>>|<<a>>|<<b>>>>` | A |
| Conditional separator: no a, b | {b = 'B' } | `<<|<<a>>: <<b>>|<<a>>|<<b>>>>` | B |
| Conditional separator: no a and no b | { } | `<<|<<a>>: <<b>>|<<a>>|<<b>>>>` | nil |
| Conditional separator: a and b, short form | {a = 'A', b = 'B' } | `<<?a>><<!a * b|: >><<?b>>` | A: B |
| Conditional separator: a, short form | {a = 'A' } | `<<?a>><<!a * b|: >><<?b>>` | A |
| Conditional separator: b, short form | {b = 'B' } | `<<?a>><<!a * b|: >><<?b>>` | B |
| <<>>, non-empty | 'Some value' | `Value is "<<>>"` | Value is "Some value" |
| <<>>, nil | nil | `Value is <<>>` | nil |
| <<>>, non-empty, const format | 'Some value' | `Value is <<|"there is some value">>` | Value is "there is some value" |
| <<>>, non-empty, header and footer in macro | 'Some value' | `<<|the value is "<<>>">>` | the value is "Some value" |
| <<>>, non-empty, nested header and footer | 'Some value' | `Value is (<<|the value is "<<>>">>)` | Value is (the value is "Some value") |
| <<>>, non-empty, header and footer | 'Some value' | `Header - <<>> - Footer` | Header - Some value - Footer |
| <<>>, nil, header and footer | nil | `Header - <<>> - Footer` | nil |
| <<|>>, nil, header and footer | nil | `<<|Header <<>> Footer>>` | nil |
| <<key|format|fallback>>, non-empty | {key = 'Value' } | `<<key|Header - <<>> - Footer|There is no "key">>` | Header - Value - Footer |
| <<key|format|fallback>>, {} | { } | `<<key|Header <<>> Footer|There is no "key">>` | There is no "key" |
| @ numeric | {{key = 'value' } } | `<<1|<<@>>: key = <<key>>>>` | 1: key = value |
| <<#>> | {'One', 'two', 'three' } | `<<#>>` | Onetwothree |
| <<#>>, default separator | {'One', 'two', 'three' } | `<<#|<<>><<,>>>>` | One, two, three |
| <<#>>, custom separator | {'One', 'two', 'three' } | `<<#|<<>><<,|; >>>>` | One; two; three |
| <<$>>, default separator | {key1 = 'one', key3 = 'three', key2 = 'two' } | `<<$|<<>><<,>>>>` | one, three, two |
| <<$>>, custom separator | {key1 = 'one', key3 = 'three', key2 = 'two' } | `<<$|<<>><<,|; >>>>` | one; three; two |
| <<#>>, {} | { } | `<<#>>` | nil |
| <<#|format>> | {'One', 'two', 'three' } | `<<#|<<>>, >>` | One, two, three,  |
| <<1|format>>, 2D | {{numeral = 'one', ordinal = 'first' }, {numeral = 'two', ordinal = 'second' }, {numeral = 'three', ordinal = 'third' } } | `<<1|Numeral: <<numeral>>, ordinal: <<ordinal>>, >>` | Numeral: one, ordinal: first,  |
| <<#|format>>, 2D | {{numeral = 'one', ordinal = 'first' }, {numeral = 'two', ordinal = 'second' }, {numeral = 'three', ordinal = 'third' } } | `<<#|Numeral: <<numeral>>, ordinal: <<ordinal>>, >>` | Numeral: one, ordinal: first, Numeral: two, ordinal: second, Numeral: three, ordinal: third,  |
| <<#|format>>, 2D, custom separator | {{numeral = 'one', ordinal = 'first' }, {numeral = 'two', ordinal = 'second' }, {numeral = 'three', ordinal = 'third' } } | `<<#|Numeral: <<numeral>>, ordinal: <<ordinal>><<,|; >>>>` | Numeral: one, ordinal: first; Numeral: two, ordinal: second; Numeral: three, ordinal: third |
| numeric key | {{key = 'value' } } | `<<1|some table>>` | some table |
| @ numeric | {{key = 'value' } } | `<<1|<<@>>>>` | 1 |
| <<#|format>>, 2D, header, <<@>> | {{numeral = 'one', ordinal = 'first' }, {numeral = 'two', ordinal = 'second' }, {numeral = 'three', ordinal = 'third' } } | `<<|One to three: <<#|<<@>>: Numeral: <<numeral>>, ordinal: <<ordinal>>, >>>>` | One to three: 1: Numeral: one, ordinal: first, 2: Numeral: two, ordinal: second, 3: Numeral: three, ordinal: third,  |
| <<#.ordinal>> | {{numeral = 'one', ordinal = 'first' }, {numeral = 'two', ordinal = 'second' }, {numeral = 'three', ordinal = 'third' } } | `<<#.ordinal|<<>>, >>` | first, second, third,  |
| <<#|format>>, 2D, header, {} | { } | `<<|One to three: <<#|Numeral: <<numeral>>, cardinal: <<ordinal>>, >>>>` | nil |
| Single-quoted key | {key = 'Value' } | `<<'key'>>` | Value |
| Double-quoted key | {key = 'Value' } | `<<"key">>` | Value |
| Single-quoted key with spaces | {some key = 'Some value' } | `<<'some key'>>` | Some value |
| Dynamic key | {which = '1', key1 = 'Value' } | `<<key<<which>>>>` | Value |
| /GNU/ | {key1 = 'Value' } | `<<gnu/^key[0-9]+/>>` | Value |
| /ONIG/ | {key1 = 'Value' } | `<<onig/^key[0-9]+/>>` | Value |
| /POSIX/ | {key1 = 'Value' } | `<<posix/^key[0-9]+/>>` | Value |
| /PCRE/ | {key1 = 'Value' } | `<</^key(?<no>\d+)$/>>` | Value |
| /TRE/ | {key1 = 'Value' } | `<<tre/^(key){~1}/>>` | Value |
| /PCRE/i | {Key1 = 'Value' } | `<</^key(?<no>\d+)$/i>>` | Value |
| /PCRE/_ | {key 1 = 'Value' } | `<</^key(?<no>\d+)$/_>>` | Value |
| /PCRE/i_ | {Key 1 = 'Value' } | `<</^key(?<no>\d+)$/i_>>` | Value |
| /PCRE/ and @ | {key1 = 'Value1', key3 = 'Value3', key2 = 'Value2' } | `<</^key(?<no>\d+)$/|<<@>>: <<no>> - <<>>, >>` | key1: 1 - Value1, key3: 3 - Value3, key2: 2 - Value2,  |
| pcre"PCRE" | {key1 = 'Value' } | `<<pcre"^key(?<no>\d+)$">>` | Value |
| pcre/PCRE/ | {key1 = 'Value' } | `<<pcre/^key(?<no>\d+)$/>>` | Value |
| Absent PCRE key | {item1 = 'Value' } | `<</^key(?<no>\d+)$/>>` | nil |
| Broken PCRE | {item1 = 'Value' } | `<</^key(?<no>\d+$/>>` | pcre regular expression "^key(?<no>\d+$" with flags "" does not compile |
| Re key, re// | {key1 = 'Value' } | `<<re/"key" { [0-9]+ }/>>` | Value |
| Re key, re'' | {key1 = 'Value' } | `<<re'"key" { [0-9]+ }'>>` | Value |
| Re key, re'', case-insensitive | {Key1 = 'Value' } | `<<re'"key" { [0-9]+ }'i>>` | Value |
| Absent re key, re'', case-sensitive | {Key1 = 'Value' } | `<<re'"key" { [0-9]+ }'>>` | nil |
| Broken re | {key1 = 'Value' } | `<<re/"key" {: [0-9]+ }/>>` | LPEG Re selector "key" {: [0-9]+ } does not compile |
| Re key, named capture | {key1 = 'Value' } | `<<re/"key" {:no: [0-9]+ :}/|<<no>>: <<>>>>` | 1: Value |
| Absent re key | {item1 = 'Value' } | `<<re/"key" { [0-9]+ }/>>` | nil |
| PCRE // key and <<>> | {key1 = 'Value1', key2 = 'Value2' } | `<</^key(?<no>\d+)$/|<<@>>: <<>>, >>` | key1: Value1, key2: Value2,  |
| lua'pattern' | {key1 = 'Value' } | `<<lua'key%d+'>>` | Value |
| lua/pattern/ | {key1 = 'Value' } | `<<lua/key%d+/>>` | Value |
| lua'pattern', case-insensitive | {Key1 = 'Value' } | `<<lua'key%d+'i>>` | Value |
| Absent lua'pattern', case-sensitive | {Key1 = 'Value' } | `<<lua'key%d+'>>` | nil |
| Nested tables | {key = {item = 'Value' } } | `<<key.item>>` | Value |
| Nested tables, regexes | {key = {item = 'Value' } } | `<</^key$/./^item$/>>` | Value |
| Nested tables, outer absent | {key = {item = 'Value' } } | `<<item.item>>` | nil |
| Nested tables, upper level as fallback | {key = {item = 'Value' }, desc = 'Description' } | `<<key|<<item>>, <<desc>>>>` | Value, Description |
| Function | {10, 15, 20, 25, 30, even = function … } | `<<even().#>>` | 102030 |
| Function with (parameter) | {10, 15, 20, 25, 30, divisible_by = function … } | `<<divisible_by (3).#>>` | 1530 |
| Function with (<<parameter>>) | {10, 15, 20, 25, 30, divider = 3, divisible_by = function … } | `<<divisible_by (<<divider>>).#>>` | 1530 |
| Composed selectors | {key1 = 'Value1', keyN = 'Unwanted' } | `<</^key/ /\d$/>>` | Value1 |
| Composed selectors, parentheses not changing the order of operations | {key1 = 'Value1', keyN = 'Unwanted' } | `<<(/^key/ /\d$/)>>` | Value1 |
| Composition of two equal keys | {key = 'Value' } | `<<"key" "key">>` | Value |
| key = value | {key = 'Value', clue = 'Value' } | `<<key = Value>>` | Value |
| /PCRE/ = value | {key1 = 'Value1', clue = 'Value2' } | `<</^key\d+$/ = Value1>>` | Value1 |
| = /pcre/ | {key1 = 'Value1', clue = 'Value2' } | `<<= /^Value\d+$/|<<>><<,>>>>` | Value1, Value2 |
| /PCRE/ = /pcre/ | {key1 = 'Value1', clue = 'Value2' } | `<</^key\d+$/ = /^Value\d+$/>>` | Value1 |
| Union all | {set2 = {'Value20', 'Value21' }, set1 = {'Value10', 'Value11' } } | `<< ( set1 + set2 ).# |<<>><<,>>>>` | Value10, Value11, Value20, Value21 |
| Cartesian: non-empty * non-empty | {a = {'Value1', 'Value2' }, b = {'Item1', 'Item2' } } | `<< a.# * b.# |<<@|(<<1>>,<<2>>)>>: <<1>>:<<2>><<,>>>>` | (1,1): Value1:Item1, (1,2): Value1:Item2, (2,1): Value2:Item1, (2,2): Value2:Item2 |
| Separator, default | {{key = 'Value1' }, {key = 'Value2' }, {key = 'Value3' } } | `<<#|<<@>>: <<key>><<,>>>>` | 1: Value1, 2: Value2, 3: Value3 |
| Separator, explicit | {{key = 'Value1' }, {key = 'Value2' }, {key = 'Value3' } } | `<<#|<<@>>: <<key>><<,|; >>>>` | 1: Value1; 2: Value2; 3: Value3 |
| Separator, dynamic | {{key = 'Value1' }, {key = 'Value2' }, {key = 'Value3' }, sep = '; ' } | `<<#|<<@>>: <<key>><<,|<<sep>>>>>>` | 1: Value1; 2: Value2; 3: Value3 |
| Separator, header and footer | {{key = 'Value1' }, {key = 'Value2' }, {key = 'Value3' } } | `<<|Header <<#|<<@>>: <<key>><<,>>>> Footer>>` | Header 1: Value1, 2: Value2, 3: Value3 Footer |
| Separator, fallback | { } | `<<|Header <<#|<<@>>: <<key>><<,>>>> Footer|Fallback>>` | Fallback |

# Credits
FormatterII is written by Alexander Mashin.
