local concat = table.concat

local serialise = require 'FormatterII'.format
local string = require 'FormatterII'.config.string

--[[
Dumping utility for debugging.
@param mixed var Variable to dump.
@param bool inline Inline mode.
@return string Serialise human-readable variable.
--]]
local function dump (var, inline)
	local rep = string.rep
	local shown = {}
	local function helper (var, indent, inline)
		if type (var) == 'string' then
			return "'" .. var .. "'"
		elseif type (var) ~= 'table' or shown [var] then
			return tostring (var)
		else
			shown [var] = true
			local serialised = {}
			for key, value in pairs (var) do
				serialised [#serialised + 1] = (inline and '' or '\n' .. rep ('\t', indent + 1)) .. helper (key, indent + 1, inline) .. ' = ' .. helper (value, indent + 1, inline)
			end
			return (inline and '' or (tostring (var) .. ' ' or 'falsy')) .. '{' .. concat (serialised, inline and ', ' or ',') .. (inline and ' ' or '\n' .. rep ('\t', indent)) .. '}'
		end
	end
	return helper (var, 0, inline)
end

local function escape (str)
	return str:gsub ('`', '``'):gsub ('|', '\\|')
end

local function divisible_by (no, tbl)
	filtered = {}
	for _, n in ipairs (tbl) do
		if tonumber (n) and n % no == 0 then
			filtered [#filtered + 1] = n
		end
	end
	return filtered
end
	
local function even (tbl)
	return divisible_by (2, tbl)
end
	
local cases = {
	--[[{ title = 'Failing',
		{ '`<<|>>`, nil, header and footer, fallback', nil, '<<|Header <<>> Footer|No value>>', 'No value' },
	}, --]]
	{ title = 'Constant format',
		{ 'Present value, constant format', { key = 'Value' }, 'const string', 'const string' },
		{ 'Absent value, constant format', {}, 'const string', 'const string' }
	},
	{ title = 'Plain format',
		{ 'Present item, plain format', { key = 'value' }, '<<key>>', 'value' },
		{ 'Present item, plain format, prefix', { key = 'value' }, '"key" is "<<key>>"', '"key" is "value"' },
		{ 'Absent item, plain format', {}, '<<key>>', 'nil' },
		{ 'Absent item, plain format, prefix', {}, '"key" is "<<key>>"', 'nil' },
		{ 'Plain format with escaped special character', { key = 'Value' }, [[The value is \|<<key>>\|]], 'The value is |Value|' },
		{ '`<<>>`, present' , 'Some value', 'Value is "<<>>"', 'Value is "Some value"' },
		{ '`<<>>`, nil', nil, 'Value is <<>>', 'nil' },
		{ '`<<>>`, present, const format', 'Some value', 'Value is <<|"there is some value">>', 'Value is "there is some value"' },
		{ '`<<>>`, present, header and footer in macro', 'Some value', '<<|the value is "<<>>">>', 'the value is "Some value"' },
		{ '`<<>>`, present, nested header and footer', 'Some value', 'They say <<|the value is "<<>>">>', 'They say the value is "Some value"' },
		{ '`<<>>`, present, header and footer', 'Some value', 'Header - <<>> - Footer', 'Header - Some value - Footer' },
		{ '`<<>>`, nil, header and footer', nil, 'Header - <<>> - Footer', 'nil' },
		{ '`<<|>>`, nil, header and footer', nil, '<<|Header <<>> Footer>>', 'nil' },
	},
	{ title = 'Fallbacks',
		{ 'Absent value with fallback', {}, '<<key|<<>>|fallback>>', 'fallback' },
		{ 'Absent value with empty fallback', {}, '<<key|<<>>|>>', '' },
		{ 'Absent value with fallback, short syntax', {}, '<<?key|fallback>>', 'fallback' },
		{ 'Absent value with empty fallback, short syntax', {}, '<<?key>>', '' },
		{ 'Present value with empty fallback', { key = 'Value' }, '<<key|<<>>|>>', 'Value' },
		{ 'Present value with empty fallback: short form', { key = 'Value' }, '<<?key>>', 'Value' },
		{ 'Present value with non-empty fallback', { key = 'Value' }, '<<key|<<>>|Fallback>>', 'Value' },
		{ 'Present value with non-empty fallback: short form', { key = 'Value' }, '<<?key|Fallback>>', 'Value' },
		{ 'Absent value with fallback, prefix and suffix', {}, '<<key|Header <<>> footer|fallback>>', 'fallback' },
		{ 'Absent value with empty fallback, prefix and suffix', {}, '<<key|Header <<>> footer|>>', '' },
		{ 'Present value with empty fallback, prefix and suffix', { key = 'Value' }, '<<key|Header <<>> footer|>>', 'Header Value footer' },
		{ 'Present value with non-empty fallback, prefix and suffix', { key = 'Value' }, '<<key|Header <<>> footer|Fallback>>', 'Header Value footer' },
		{ 'Empty and non-empty', { key = 'value' }, '<<key>>, <<item>>', 'nil' },
		{ 'Optional empty and non-empty', { key = 'value' }, '<<key|<<>>|>>, <<item|<<>>|>>', 'value, ' },
		{ 'Optional empty and non-empty: short syntax', { key = 'value' }, '<<?key>>, <<?item>>', 'value, ' },		
		{ 'At least one; one present',  { key1 = 'Value1' }, '<<key1 + key2>>',  'Value1' },
		{ 'At least one; two present',  { key1 = 'Value1', key2 = 'Value2' }, '<<key1 + key2|<<>><<,>>>>',  'Value1, Value2' },
		{ 'At least one; absent', {}, '<<key1 + key2>>', 'nil' },
		{ 'At least one; one present; prefix',  { key1 = 'Value1' }, '<<key1 + key2|Header <<>>>>',  'Header Value1' },
		{ 'At least one; absent; prefix', {}, '<<key1 + key2|Header <<>>>>', 'nil' },
		{ 'At least one; two present; prefix',  { key1 = 'Value1', key2 = 'Value2' }, '<<|Header: <<key1 + key2|<<>><<,>>>>>>',  'Header: Value1, Value2' },		
	},
	{ title = 'Conditional format',
		{ 'Conditional constant; present', { key = 'Value' }, '<<key|<<!>>const string>>', 'const string' },
		{ 'Conditional constant; absent', {}, '<<key|<<!>>const string>>', 'nil' },
		{ 'Conditional constant; absent; fallback', {}, '<<key|<<!>>const string|fallback>>', 'fallback' },
		{ 'Conditional expression, first option', { key = 'value1' }, '<<key = value1|<<!>>yes|no>>', 'yes' },
		{ 'Conditional expression, second option', { key = 'value2' }, '<<key = value1|<<!>>yes|no>>', 'no' },
		{ 'Conditional separator: a and b', { a = 'A', b = 'B' }, '<<|<<a>>: <<b>>|<<a>>|<<b>>>>', 'A: B' },
		{ 'Conditional separator: a, no b', { a = 'A' }, '<<|<<b>>>>', 'nil' },
		{ 'Conditional separator: a, no b', { a = 'A' }, '<<|<<a>>: <<b>>|<<a>>|<<b>>>>', 'A' },
		{ 'Conditional separator: no a, b', { b = 'B' }, '<<|<<a>>: <<b>>|<<a>>|<<b>>>>', 'B' },
		{ 'Conditional separator: no a and no b', {}, '<<|<<a>>: <<b>>|<<a>>|<<b>>>>', 'nil' },
		{ 'Conditional separator: a and b, short form', { a = 'A', b = 'B' }, '<<?a>><<a * b|<<!>>: |>><<?b>>', 'A: B' },
		{ 'Conditional separator: a, short form', { a = 'A' }, '<<?a>><<a * b|<<!>>: |>><<?b>>', 'A' },
		{ 'Conditional separator: b, short form', { b = 'B' }, '<<?a>><<a * b|<<!>>: |>><<?b>>', 'B' },
	},
	{ title = 'Iteration', 
		{ '`@` numeric', { { key = 'value' } }, '<<1|<<@>>: key = <<key>>>>', '1: key = value' },
		{ '`<<#>>`, no separator', { 'One', 'two', 'three' }, '<<#>>', 'Onetwothree' },
		{ '`<<#>>`, default separator', { 'One', 'two', 'three' }, '<<#|<<>><<,>>>>', 'One, two, three' },
		{ '`<<#>>`, custom separator', { 'One', 'two', 'three' }, '<<#|<<>><<,|; >>>>', 'One; two; three' },	
		{ '`<<$>>`, default separator', { key1 = 'one', key2 = 'two', key3 = 'three' }, '<<$|<<>><<,>>>>', 'one, three, two' },
		{ '`<<$>>`, custom separator', { key1 = 'one', key2 = 'two', key3 = 'three' }, '<<$|<<>><<,|; >>>>', 'one; three; two' },	
		{ '`<<#>>`, {}', {}, '<<#>>', 'nil' },
		{ '`<<#|format>>`', { 'One', 'two', 'three' }, '<<#|<<>>, >>', 'One, two, three, ' },
		{ '`<<1|format>>`, 2D', {
			{ numeral = 'one', ordinal = 'first' },
			{ numeral = 'two', ordinal = 'second' },
			{ numeral = 'three', ordinal = 'third' }
		}, '<<1|Numeral: <<numeral>>, ordinal: <<ordinal>>, >>', 'Numeral: one, ordinal: first, ' },
		{
			'`<<#|format>>`, 2D',
			{
				{ numeral = 'one', ordinal = 'first' },
				{ numeral = 'two', ordinal = 'second' },
				{ numeral = 'three', ordinal = 'third' }
			},
			'<<#|Numeral: <<numeral>>, ordinal: <<ordinal>>, >>',
			'Numeral: one, ordinal: first, Numeral: two, ordinal: second, Numeral: three, ordinal: third, '
		},
			{
			'`<<#|format>>`, 2D, custom separator',
			{
				{ numeral = 'one', ordinal = 'first' },
				{ numeral = 'two', ordinal = 'second' },
				{ numeral = 'three', ordinal = 'third' }
			},
			'<<#|Numeral: <<numeral>>, ordinal: <<ordinal>><<,|; >>>>',
			'Numeral: one, ordinal: first; Numeral: two, ordinal: second; Numeral: three, ordinal: third'
		},
		{ 'numeric key', { { key = 'value' } }, '<<1|some table>>', 'some table' },
		{ '`@` numeric', { { key = 'value' } }, '<<1|<<@>>>>', '1' },
		{
			'`<<#|format>>`, 2D, header, `<<@>>`',
			{
				{ numeral = 'one', ordinal = 'first' },
				{ numeral = 'two', ordinal = 'second' },
				{ numeral = 'three', ordinal = 'third' }
			},
			'<<|One to three: <<#|<<@>>: Numeral: <<numeral>>, ordinal: <<ordinal>>, >>>>',
			'One to three: 1: Numeral: one, ordinal: first, 2: Numeral: two, ordinal: second, 3: Numeral: three, ordinal: third, '
		},
		{ '`<<#.ordinal>>`', {
			{ numeral = 'one', ordinal = 'first' },
			{ numeral = 'two', ordinal = 'second' },
			{ numeral = 'three', ordinal = 'third' }
		}, '<<#.ordinal|<<>>, >>', 'first, second, third, ' },
		{ '`<<#|format>>`, 2D, header, `{}`', {}, '<<|One to three: <<#|Numeral: <<numeral>>, cardinal: <<ordinal>>, >>>>', 'nil' },
		{ '`<<#|format>>`, 2D, header, `{}`, fallback', {}, '<<|One to three: <<#|Numeral: <<numeral>>, cardinal: <<ordinal>>, >>|No items>>', 'No items' },
	},
	{ title = 'Selectors',
		{ 'Single-quoted key', { key = 'Value' }, "<<'key'>>", 'Value' },
		{ 'Double-quoted key', { key = 'Value' }, '<<"key">>', 'Value' },
		{ 'Single-quoted key with spaces', { ['some key'] = 'Some value' }, "<<'some key'>>", 'Some value' },
		{ 'Dynamic key', { which = '1', key1 = 'Value' }, '<<key<<which>>>>', 'Value' },
		{ '/GNU/', { key1 = 'Value' }, '<<gnu/^key[0-9]+/>>', 'Value' },
		{ '/ONIG/', { key1 = 'Value' }, '<<onig/^key[0-9]+/>>', 'Value' },
		{ '/POSIX/', { key1 = 'Value' }, '<<posix/^key[0-9]+/>>', 'Value' },	
		{ '/PCRE/', { key1 = 'Value' }, '<</^key(?<no>\\d+)$/>>', 'Value' },
		{ '/TRE/', { key1 = 'Value' }, '<<tre/^(key){~1}/>>', 'Value' },
		{ '/PCRE/i', { Key1 = 'Value' }, '<</^key(?<no>\\d+)$/i>>', 'Value' },
		{ '/PCRE/_', { ['key 1'] = 'Value' }, '<</^key(?<no>\\d+)$/_>>', 'Value' },
		{ '/PCRE/i_', { ['Key 1'] = 'Value' }, '<</^key(?<no>\\d+)$/i_>>', 'Value' },
		{ '/PCRE/ and @', { key1 = 'Value1', key2 = 'Value2', key3 = 'Value3' }, '<</^key(?<no>\\d+)$/|<<@>>: <<no>> - <<>>, >>', 'key1: 1 - Value1, key3: 3 - Value3, key2: 2 - Value2, ' },
		{ 'pcre"PCRE"', { key1 = 'Value' }, '<<pcre"^key(?<no>\\d+)$">>', 'Value' },
		{ 'pcre/PCRE/', { key1 = 'Value' }, '<<pcre/^key(?<no>\\d+)$/>>', 'Value' },	
		{ 'Absent PCRE key', { item1 = 'Value' }, '<</^key(?<no>\\d+)$/>>', 'nil' },
		{ 'Broken PCRE', { item1 = 'Value' }, '<</^key(?<no>\\d+$/>>', 'pcre regular expression "^key(?<no>\\d+$" with flags "" does not compile' },
		{ 'Re key, re//', { key1 = 'Value' }, '<<re/"key" { [0-9]+ }/>>', 'Value' },
		{ [[Re key, re'']], { key1 = 'Value' }, [[<<re'"key" { [0-9]+ }'>>]], 'Value' },
		{ [[Re key, re'', case-insensitive]], { Key1 = 'Value' }, [[<<re'"key" { [0-9]+ }'i>>]], 'Value' },
		{ [[Absent re key, re'', case-sensitive]], { Key1 = 'Value' }, [[<<re'"key" { [0-9]+ }'>>]], 'nil' },	
		{ 'Broken re', { key1 = 'Value' }, '<<re/"key" {: [0-9]+ }/>>', 'LPEG Re selector "key" {: [0-9]+ } does not compile' },
		{ 'Re key, named capture', { key1 = 'Value' }, '<<re/"key" {:no: [0-9]+ :}/|<<no>>: <<>>>>', '1: Value' },
		{ 'Absent re key', { item1 = 'Value' }, '<<re/"key" { [0-9]+ }/>>', 'nil' },
		{ 'PCRE // key and <<>>', { key1 = 'Value1', key2 = 'Value2' }, '<</^key(?<no>\\d+)$/|<<@>>: <<>>, >>', 'key1: Value1, key2: Value2, ' },
		{ "lua'pattern'", { key1 = 'Value' }, "<<lua'key%d+'>>", 'Value' },
		{ "lua/pattern/", { key1 = 'Value' }, "<<lua/key%d+/>>", 'Value' },
		{ "lua'pattern', case-insensitive", { Key1 = 'Value' }, "<<lua'key%d+'i>>", 'Value' },
		{ "Absent lua'pattern', case-sensitive", { Key1 = 'Value' }, "<<lua'key%d+'>>", 'nil' },
	},
	{ title = 'Nested tables',
		{ 'Nested tables', { key = { item = 'Value' } }, '<<key.item>>', 'Value' },
		{ 'Nested tables, regexes', { key = { item = 'Value' } }, '<</^key$/./^item$/>>', 'Value' },
		{ 'Nested tables, outer absent', { key = { item = 'Value' } }, '<<item.item>>', 'nil' },
		{ 'Nested tables, upper level as fallback', { key = { item = 'Value' }, desc = 'Description' }, '<<key|<<item>>, <<desc>>>>', 'Value, Description' },
	},
	{ title = 'Functions',
		{ 'Function', { 10, 15, 20, 25, 30, even = even },  '<<even().#>>', '102030' },
		{ 'Function with (parameter)', { 10, 15, 20, 25, 30, divisible_by = divisible_by },  '<<divisible_by (3).#>>', '1530' },
		{ 'Function with (<<parameter>>)', { 10, 15, 20, 25, 30, divider = 3, divisible_by = divisible_by },  '<<divisible_by (<<divider>>).#>>', '1530' },
	}, 
	{ title = 'Composition',
		{ 'Composed selectors', { key1 = 'Value1', keyN = 'Unwanted' }, '<</^key/ /\\d$/>>', 'Value1' },
		{ 'Composed selectors, parentheses not changing the order of operations', { key1 = 'Value1', keyN = 'Unwanted' }, '<<(/^key/ /\\d$/)>>', 'Value1' },
		{ 'Composition of two equal keys', { key = 'Value' }, '<<"key" "key">>', 'Value' },
		{ 'key = value', { key = 'Value', clue = 'Value' }, '<<key = Value>>', 'Value' },
		{ '/PCRE/ = value', { key1 = 'Value1', clue = 'Value2' }, '<</^key\\d+$/ = Value1>>', 'Value1' },
		{ '= /pcre/', { key1 = 'Value1', clue = 'Value2' }, '<<= /^Value\\d+$/|<<>><<,>>>>', 'Value1, Value2' },
		{ '/PCRE/ = /pcre/', { key1 = 'Value1', clue = 'Value2' }, '<</^key\\d+$/ = /^Value\\d+$/>>', 'Value1' },
		{ 'Union all', { set1 = { 'Value10', 'Value11' }, set2 = { 'Value20', 'Value21' } }, '<< ( set1 + set2 ).# |<<>><<,>>>>', 'Value10, Value11, Value20, Value21' },
		{ 'First non-empty: first', { key1 = 'Value1' }, '<< /key\\d+/, /item\\d+/>>', 'Value1' },
		{ 'First non-empty: second', { key1 = 'Value1' }, '<< /item\\d+/, /key\\d+/>>', 'Value1' },
		{ 'First non-empty: absent', { field1 = 'Value1' }, '<< /item\\d+/, /key\\d+/>>', 'nil' },
		{ 'Cartesian: non-empty * non-empty', { a = { 'Value1', 'Value2' }, b = { 'Item1', 'Item2' } }, '<< a.# * b.# |<<@|(<<1>>,<<2>>)>>: <<1>>:<<2>><<,>>>>', '(1,1): Value1:Item1, (1,2): Value1:Item2, (2,1): Value2:Item1, (2,2): Value2:Item2' },
	},
	{ title = 'Separators',
		{ 'Separator, default', { { key = 'Value1' }, { key = 'Value2' }, { key = 'Value3' } }, '<<#|<<@>>: <<key>><<,>>>>', '1: Value1, 2: Value2, 3: Value3' },
		{ 'Separator, explicit', { { key = 'Value1' }, { key = 'Value2' }, { key = 'Value3' } }, '<<#|<<@>>: <<key>><<,|; >>>>', '1: Value1; 2: Value2; 3: Value3' },
		{ 'Separator, dynamic', { { key = 'Value1' }, { key = 'Value2' }, { key = 'Value3' }, sep = '; ' }, '<<#|<<@>>: <<key>><<,|<<sep>>>>>>', '1: Value1; 2: Value2; 3: Value3' },	
		{
			'Separator, header and footer',
			{ { key = 'Value1' }, { key = 'Value2' }, { key = 'Value3' } },
			'<<|Header <<#|<<@>>: <<key>><<,>>>> Footer>>',
			'Header 1: Value1, 2: Value2, 3: Value3 Footer'
		},
		{ 'Separator, fallback', {}, '<<|Header <<#|<<@>>: <<key>><<,>>>> Footer|Fallback>>', 'Fallback' },
	}
}
local tested
tested = { '', '| Status | Description | Format | Expected | Actual |', '| --- | --- | --- | --- | --- |' }
local succeeded, failed = 0, 0
for _, section in ipairs (cases) do
	tested [#tested + 1] = '| ' .. section.title .. ' |'
	for _, case in ipairs (section) do
		local desc, tbl, format, expected = case[1], case[2], case[3], case[4]
		local actual = serialise (format, tbl) or 'nil'
		local status
		if actual == expected then
			status = 'SUCCESS'
			succeeded = succeeded + 1
		else
			status = 'FAILURE'
			failed = failed + 1
		end
		tested [#tested + 1]
			  = '| ' .. status
			.. ' | ' .. desc
			.. ' | `' .. escape (format)
			.. '` | ' .. escape (expected)
			.. ' | ' .. escape (actual)
			.. ' |'
	end
end
tested [1] = 'Succeeded: ' .. tostring (succeeded) .. ', failed: ' .. tostring (failed)
print (concat (tested, '\n'))