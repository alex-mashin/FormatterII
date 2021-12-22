local concat = table.concat

local serialise = require'serialiserii'.serialise
local wrap, dump = require'serialiserii'.wrap, require'serialiserii'.dump

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
	{ '{}, constant format', {}, 'const string', 'const string' },
	{ 'Non-empty item, no format', { key = 'value' }, '"key" is "<<key>>"', '"key" is "value"' },
	{ 'Non-empty item, constant format for item', { key = 'value' }, '"key" is "<<key|fallback>>"', '"key" is "fallback"' },
	{ '{}, constant format for item', {}, '"key" is "<<key|fallback>>"', '"key" is "fallback"' },
	{ '{}, simple format for item', {}, '"key" is "<<key>>"', 'nil' },
	{ 'nil, simple format for item', nil, '"key" is "<<key>>"', 'nil' },
	{ '{}, simple format for item, fallback', {}, '"key" is "<<key|<<>>|(there is no key)>>"', '"key" is "(there is no key)"' },
	{ '<<>>, non-empty' , 'Some value', 'Value is "<<>>"', 'Value is "Some value"' },
	{ '<<>>, nil', nil, 'Value is <<>>', 'nil' },
	{ '<<>>, non-empty, const format', 'Some value', 'Value is <<|"there is some value">>', 'Value is "there is some value"' },
	{ '<<>>, non-empty, header and footer in macro', 'Some value', '<<|the value is "<<>>">>', 'the value is "Some value"' },
	{ '<<>>, non-empty, nested header and footer', 'Some value', 'Value is (<<|the value is "<<>>">>)', 'Value is (the value is "Some value")' },
	{ '<<>>, non-empty, header and footer', 'Some value', 'Header - <<>> - Footer', 'Header - Some value - Footer' },
	{ '<<>>, nil, header and footer', nil, 'Header - <<>> - Footer', 'nil' },
	{ '<<|>>, nil, header and footer', nil, '<<|Header <<>> Footer>>', 'nil' },
	{ '<<key|format|fallback>>, non-empty', { key = 'Value' }, '<<key|Header - <<>> - Footer|There is no "key">>', 'Header - Value - Footer' },
	{ '<<key|format|fallback>>, {}', {}, '<<key|Header <<>> Footer|There is no "key">>', 'There is no "key"' },
	{ '@ numeric', { { key = 'value' } }, '<<1|<<@>>: key = <<key>>>>', '1: key = value' },
	{ '<<#>>', { 'One', 'two', 'three' }, '<<#>>', 'Onetwothree' },
	{ '<<#>>, {}', {}, '<<#>>', 'nil' },
	{ '<<#|format>>', { 'One', 'two', 'three' }, '<<#|<<>>, >>', 'One, two, three, ' },
	{ '<<1|format>>, 2D', {
		{ numeral = 'one', ordinal = 'first' },
		{ numeral = 'two', ordinal = 'second' },
		{ numeral = 'three', ordinal = 'third' }
	}, '<<1|Numeral: <<numeral>>, ordinal: <<ordinal>>, >>', 'Numeral: one, ordinal: first, ' },
	{
		'<<#|format>>, 2D',
		{
			{ numeral = 'one', ordinal = 'first' },
			{ numeral = 'two', ordinal = 'second' },
			{ numeral = 'three', ordinal = 'third' }
		},
		'<<#|Numeral: <<numeral>>, ordinal: <<ordinal>>, >>',
		'Numeral: one, ordinal: first, Numeral: two, ordinal: second, Numeral: three, ordinal: third, '
	},
	{ 'numeric key', { { key = 'value' } }, '<<1|some table>>', 'some table' },
	{ '@ numeric', { { key = 'value' } }, '<<1|<<@>>>>', '1' },
	{
		'<<#|format>>, 2D, header, <<@>>',
		{
			{ numeral = 'one', ordinal = 'first' },
			{ numeral = 'two', ordinal = 'second' },
			{ numeral = 'three', ordinal = 'third' }
		},
		'<<|One to three: <<#|<<@>>: Numeral: <<numeral>>, ordinal: <<ordinal>>, >>>>',
		'One to three: 1: Numeral: one, ordinal: first, 2: Numeral: two, ordinal: second, 3: Numeral: three, ordinal: third, '
	},
	{ '<<#.ordinal>>', {
		{ numeral = 'one', ordinal = 'first' },
		{ numeral = 'two', ordinal = 'second' },
		{ numeral = 'three', ordinal = 'third' }
	}, '<<#.ordinal|<<>>, >>', 'first, second, third, ' },
	{ '<<#|format>>, 2D, header, {}', {}, '<<|One to three: <<#|Numeral: <<numeral>>, cardinal: <<ordinal>>, >>>>', 'nil' },
	{ 'Single-quoted key', { key = 'Value' }, "<<'key'>>", 'Value' },
	{ 'Double-quoted key', { key = 'Value' }, '<<"key">>', 'Value' },
	{ 'Single-quoted key with spaces', { ['some key'] = 'Some value' }, "<<'some key'>>", 'Some value' },
	{ 'Dynamic key', { which = '1', key1 = 'Value' }, '<<key<<which>>>>', 'Value' },
	{ '/PCRE/', { key1 = 'Value' }, '<</^key(?<no>\\d+)$/>>', 'Value' },
	{ '/PCRE/ and @', { key1 = 'Value1', key2 = 'Value2', key3 = 'Value3' }, '<</^key(?<no>\\d+)$/|<<@>>: <<no>> - <<>>, >>', 'key1: 1 - Value1, key3: 3 - Value3, key2: 2 - Value2, ' },
	{ 'pcre"PCRE"', { key1 = 'Value' }, '<<pcre"^key(?<no>\\d+)$">>', 'Value' },
	{ 'pcre/PCRE/', { key1 = 'Value' }, '<<pcre/^key(?<no>\\d+)$/>>', 'Value' },	
	{ 'Absent PCRE key', { item1 = 'Value' }, '<</^key(?<no>\\d+)$/>>', 'nil' },
	{ 'Broken PCRE', { item1 = 'Value' }, '<</^key(?<no>\\d+$/>>', 'Perl-compatible regular expression ^key(?<no>\\d+$ does not compile' },
	{ 'Re key, re//', { key1 = 'Value' }, '<<re/"key" { [0-9]+ }/>>', 'Value' },
	{ [[Re key, re'']], { key1 = 'Value' }, [[<<re'"key" { [0-9]+ }'>>]], 'Value' },	
	{ 'Broken re', { key1 = 'Value' }, '<<re/"key" {: [0-9]+ }/>>', 'LPEG Re selector "key" {: [0-9]+ } does not compile' },
	{ 'Re key, named capture', { key1 = 'Value' }, '<<re/"key" {:no: [0-9]+ :}/|<<no>>: <<>>>>', '1: Value' },
	{ 'Absent re key', { item1 = 'Value' }, '<<re/"key" { [0-9]+ }/>>', 'nil' },
	{ 'PCRE // key and <<>>', { key1 = 'Value1', key2 = 'Value2' }, '<</^key(?<no>\\d+)$/|<<@>>: <<>>, >>', 'key1: Value1, key2: Value2, ' },
	{ "lua'pattern'", { key1 = 'Value' }, "<<lua'key%d+'>>", 'Value' },	
	{ "lua/pattern/", { key1 = 'Value' }, "<<lua/key%d+/>>", 'Value' },	
	{ 'Nested tables', { key = { item = 'Value' } }, '<<key.item>>', 'Value' },
	{ 'Nested tables, regexes', { key = { item = 'Value' } }, '<</^key$/./^item$/>>', 'Value' },
	{ 'Nested tables, outer absent', { key = { item = 'Value' } }, '<<item.item>>', 'nil' },
	{ 'Nested tables, upper level as fallback', { key = { item = 'Value' }, desc = 'Description' }, '<<key|<<item>>, <<desc>>>>', 'Value, Description' },
	{ 'Function', { 10, 15, 20, 25, 30, even = even },  '<<even().#>>', '102030' },
	{ 'Function with (parameter)', { 10, 15, 20, 25, 30, divisible_by = divisible_by },  '<<divisible_by (3).#>>', '1530' },
	{ 'Function with (<<parameter>>)', { 10, 15, 20, 25, 30, divider = 3, divisible_by = divisible_by },  '<<divisible_by (<<divider>>).#>>', '1530' },	
	{ 'Composed selectors', { key1 = 'Value1', keyN = 'Unwanted' }, '<</^key/ /\\d$/>>', 'Value1' },
	{ 'Composed selectors, parentheses not changing the order of operations', { key1 = 'Value1', keyN = 'Unwanted' }, '<<(/^key/ /\\d$/)>>', 'Value1' },
	{ 'Composition of two equal keys', { key = 'Value' }, '<<"key" "key">>', 'Value' },
	{ 'key = value', { key = 'Value', clue = 'Value' }, '<<key = Value>>', 'Value' },
	{ '/PCRE/ = value', { key1 = 'Value', clue = 'Value' }, '<</^key\\d+$/ = Value>>', 'Value' },
	{ 'Separator, default', { { key = 'Value1' }, { key = 'Value2' }, { key = 'Value3' } }, '<<#|<<@>>: <<key>><<,>>>>', '1: Value1, 2: Value2, 3: Value3' },
	{ 'Separator, explicit', { { key = 'Value1' }, { key = 'Value2' }, { key = 'Value3' } }, '<<#|<<@>>: <<key>><<,|; >>>>', '1: Value1; 2: Value2; 3: Value3' },	
}
local tested = { 'Status\tDescription\tFormat\tExpected\tActual' }
local succeeded, failed = 0, 0
for _, case in ipairs (cases) do
	local desc, tbl, format, expected = case[1], case[2], case[3], case[4]
	local actual = serialise (tbl, format)
	local status
	if actual == expected then
		status = 'SUCCESS'
		succeeded = succeeded + 1
	else
		status = 'FAILURE'
		failed = failed + 1
	end
	tested [#tested +1] = status .. '\t' .. desc .. '\t' .. format .. '\t' .. expected .. '\t' .. actual
end
print ('Succeeded: ' .. tostring (succeeded) .. ', failed: ' .. tostring (failed))
print (concat (tested, '\n'))

print '----------------'
print (dump (wrap ({ { key = 'Value' } }, 'top') [1] ['@']))