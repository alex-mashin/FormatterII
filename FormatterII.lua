-- @TODO: order of pairs() in Lua 5.2+.
local p = {
	config = {
		string		= string,	-- string library to use.
		condense	= '_',		-- "condense" (ignore whitespaces, hyphens and underscores) flag.
		fillers		= '[-_%s]',	-- characters to ignore when the condense flag is used.
		conditional	= '!',		-- conditional macro flag.
		optional	= '?',		-- optional macro flag.
		separator	= ',',		-- separator macro flag. 
		key			= '@',		-- key selector.
		escape		= '\\',		-- escape character.
		open		= '<<',		-- macro start.
		pipe		= '|',		-- separator between selector and format string, or between format string and fallback format string.
		close		= '>>',		-- macro end.
		operators	= {			-- selector arithmetics.
			enter		= '.',	-- enter field (change context).
			cartesian	= '*',	-- cartesian product.
			union		= '+',	-- union of selectors.
			first		= ','	-- ordered choice of selectors.
		},
		ipairs		= '#',		-- ipairs() selector.
		pairs		= '$'		-- pairs() selector.
	}
}

--[[
Localising standard functions:
--]]
local unpack = unpack or table.unpack
local concat, sort = table.concat, table.sort
local coroutine = require 'coroutine'
local wrap, yield = coroutine.wrap, coroutine.yield

local function error_msg (msg)
	return msg
end

--[[
Redefined pairs and ipairs.
--]]

--[[
Whether a metamethod exists.
@param string func Function that is supposed to invoke the metamethod.
@param ?string method Metamethod to check.
@return bool True, if the metamethod is supported.
--]]
local function metamethod_supported (func, method)
	return not _G [func] (setmetatable ({ 0 }, { [method or '__' .. func] = function (tbl) end }))
end

--[[
Emulate a metamethod, if it does not exist.

@param string func Function that is supposed to invoke the metamethod.
@param ?string method Metamethod to emulate.
@return function Function redefined to take metamethod into account.
--]]
local function emulate_metamethod (func, method)
	if not metamethod_supported (func, method) then
		local raw = _G [func]
		return function (...)
			local metatable = getmetatable (tbl)
			local metamethod = metatable and metatable [method or '__' .. func]
			-- Do not simplify to return ... and ... or ...:
			if metamethod then
				return metamethod (...)
			else
				return raw (...)
			end
		end
	else
		return _G [func]
	end
end

local pairs, ipairs = emulate_metamethod 'pairs', emulate_metamethod 'ipairs'
	
--[[
Wrap a table so it can fall back to "parent" table for items and return self as item [''].
	
@param mixed var Table or other variable to wrap,
@param ?string name var's name in parent,
@param ?table parent Its parent table,
@param ?string serialised var's serialisation for __tostring.
@return table The wrapped table.
--]]
local function wrap_table (var, name, parent, serialised)
	if type (var) ~= 'table' then
		return var
	end

	local var_serialised = serialised and tostring (serialised) or var and tostring (var) or nil
	local var = type (var) == 'table' and var or { var }
	local len = type (var) == 'table' and #var or 0
	return setmetatable ({}, {
		__index = function (tbl, key)
			if key == '' then
				return var -- already wrapped.
			end
			if key == '..' then
				return parent -- already wrapped.
			end
			
			local item
			if key == '@' then
				item = name
			else
				item = rawget (var, key) or (parent or _G) [key]
			end
			
			if item == nil then
				return nil
			end
			
			return wrap_table (item, key, tbl)
		end,
		__tostring = function (tbl)
			return var_serialised
		end,
		__ipairs = function (tbl)
			return function (tbl, i)
				i = i + 1
				if i <= len then return i, wrap_table (var [i], i, var) end
			end, var, 0
		end,
		__pairs = function (tbl)
			return function (tbl, key)
				local value
				key, value = next (var, key)
				if value ~= nil then return key, wrap_table (value, key, var) end
			end, var, nil
		end
	})
end

--[[
Merge tables.
	
@param table ... Tables to merge.
@return table Merged table.
--]]
local function merge (...)
	local merged = {}
	for _, tbl in ipairs {...} do
		for key, value in pairs (type (tbl) == 'table' and tbl or { tbl }) do
			merged [key] = value
		end
	end
	return merged
end

--[[
Convert to number, if convertible; leave unchanged otherwise.
@param mixed value The value to convert.
@return mixed The converted value.
--]]
local function try_tonumber (value)
	return tonumber (value) or value
end

--[[
String iteration
--]]

--[[
Make a string iterator from a matcher.

@param function matcher A function accepting a string and offset
	and returning match's offset, match's finish and the match itself.
@return function A string iterator.
--]]
local function string_iterator (matcher)
	local sub = p.config.string.sub
	return function (str)
		local len = #tostring (str)
		return wrap (function ()
			local offset, finish, captures = 1, len, nil
			while offset and offset <= len do
				offset, finish, captures = matcher (str, offset)
				if offset then
					local match = sub (str, offset, finish)
					offset = finish + 1
					yield (match, captures)
				end
			end
		end)
	end
end

--[[
Make a comparator function from a plain value.
	
@param mixed value A plain value to compare with.
@return function A comparator function accepting a string and an offset
	and returning an offset and end position of the match and a table of captures.
@return mixed A copy of value, for table_iterator (trivial table[key] case).
--]]
local function exactly (value)
	return string_iterator (function (str, offset)
		if str == value then
			return 1, #tostring (str)
		end
	end), value
end

--[[
Extract a flag from a string of flags.

@param string flags Flags.
@param string flag Flag to search.
@return bool True, if flag is present in flags.
@return string Flags with the flag removed.
--]]
local function cut_flag (flags, flag)
	local string = p.config.string
	local find, gsub = string.find, string.gsub
	if not flags or flags == '' then
		return flags
	end
	local found = false
	local _pos = find (flags, flag, 1, true)
	if _pos then
		found = true
		flags = gsub (flags, flag, '')
	end
	return found, flags
end

--[[
Returns a factory of comparator functions accepting a regular expression.
	
@param string flavour Type of regex: gnu, onig, posix, tre.
@return function (string expr, string flags) -> function ( (string, offset) -> (offset, end, {captures}) )
	or nil, if the library is not available.
--]]
local function regex (flavour)
	local ok, lib = pcall (require, 'rex_' .. flavour)
	if not ok or not lib then
		return nil
	end
	local new_regex = lib.new
	local gsub = p.config.string.gsub
	local fillers = p.config.fillers
	return function (expr, flags)
		local condense, flags = cut_flag (flags, p.config.condense)
		local valid, compiled = pcall (new_regex, expr, flags)
		if not valid then
			return error_msg (
				flavour .. ' regular expression "' .. expr
			 .. '" with flags "' .. (flags or '') .. '" does not compile'
			)
		end
		return string_iterator (function (str, offset)
			local str = condense and gsub (str, fillers, '') or str
			return compiled:tfind (str, offset)
		end)
	end
end

-- LPeg's re module:
local lpeg = require 'lpeg'
local Cp, Ct = lpeg.Cp, lpeg.Ct
local re_lib = require 'lualibs/re'
re_lib.string = p.config.string
local compile_re = re_lib.compile

--[[
Make a comparator function from an LPEG Re selector.
	
@param string expr LPEG Re selector.
@param string flags A string of flags.
@return function A comparator function accepting a string and an offset
	and returning an offset and end position of the match and a table of captures.
--]]
local function re (expr, flags)
	local string = p.config.string
	local sub, gsub = string.sub, string.gsub
	local fillers = p.config.fillers
	local condense, flags = cut_flag (flags, p.config.condense)
	local valid, compiled = pcall (compile_re, expr, {}, flags)
	if not valid then
		return error_msg ('LPEG Re selector ' .. expr .. ' does not compile')
	end
	compiled = Cp() * Ct (compiled) * Cp()
	return string_iterator (function (str, offset)
		local str = condense and gsub (str, fillers, '') or str
		-- @todo: some flags.
		local start, matches, finish = compiled:match (str, offset)
		local captures
		if matches and type (matches [2]) == 'number' then
			captures = { sub(str, offset, matches [2] - 1) }
		else
			captures = matches
		end
		return start, start and finish - 1 or nil, captures
	end)
end

--[[
Generates Lua pattern iterator.
@param string expr Lua regular expression.
@param string flags A string of flags.	
@return function A comparator function accepting a string and an offset
	and returning an offset and end position of the match and a table of captures.
--]]
-- @todo: improve.
local function lua_pattern (expr, flags)
	local string = p.config.string
	local find, gsub, lower, upper = string.find, string.gsub, string.lower, string.upper
	local fillers = p.config.fillers
	local condense, flags = cut_flag (flags, p.config.condense)
	if find (flags or '', 'i', 1, true) then
		expr = gsub (expr, '(%%?)(%a)', function (percent, letter)
			if percent == '%' then
				return '%' .. letter
			else
				return '[' .. upper (letter) .. lower (letter) .. ']'
			end
		end)
	end
	return string_iterator (function (str, offset)
		local str = condense and gsub (str, fillers, '') or str
		return find (str, expr, offset)
	end)
end

--[[
Table iteration
--]]

--[[
Makes a table iterator from a string iterator.

@param bool iterate_value True, if string_iterator will be applied to table values; false, if to keys.
@param function|string string_iterator A function accepting a string to iterate
	and returning an iterator over a string; yielding string value and, optionally, captures;
	or an error message.
@param ?mixed exact_key If set, and iterate_value == false, then it is a special case; simply table[key].
@return function A function that returns an iterator over a table
	yielding key, value, captures.
--]]
local function table_iterator (iterate_value, string_iterator, exact_key)
	-- Special case: error message instead of an iterator.
	if type (string_iterator) == 'string' then
		-- This is an error message:
		return function (tbl)
			return wrap (function () yield (false, string_iterator, nil) end) -- @todo: false?
		end
	end
	
	-- Special case: just table[key]. It's faster than the generic iterator below,
	-- 		and is necessary to enable pseudo-keys like '@'.
	if not iterate_value and exact_key then
		return function (tbl)
			return wrap (function ()
				if type (tbl) == 'table' then
					local value = tbl [exact_key]
					if value then
						yield (exact_key, value)
					end
				end
			end)
		end
	end
	
	-- Generic case: filtering values or filtering keys with a regular expression:
	return function (tbl)
		return wrap (function ()
			for key, value in pairs (tbl or {}) do
				local iterated_string = iterate_value and value or key
				for match, captures in string_iterator (iterated_string) do
					yield (key, value, captures)
				end
			end
		end)
	end
end

--[[
Table iterator that yields the table itself.

@param mixeв var Iterated variable.
@return A function that returns an iterator over a table yielding key, value.
--]]
local function self (var)
	return wrap (function ()
		yield (type (var) == 'table' and var ['@'] or '', var)
	end)
end

--[[
Converts a function to a table iterator yielding its results.
	
@param string name Function index in the table. The function should accept zero or more parameters, then the table.
@param mixed ... Additional parameters to function.
@return A function that returns an iterator over a table
	yielding key, value, captures.
--]]
local function function2table_iterator (name, ...)
	local params = {...}
	return function (tbl)
		local func = tbl [name]
		local resolved = {}
		for _, param in ipairs (params) do
			resolved [#resolved + 1] = type (param) == 'function' and param (tbl) or param -- can be format or a constant string.
		end
		local results = { func and (#resolved > 0 and func (unpack (resolved), tbl) or func (tbl)) or nil } -- cannot be simplified.
		return wrap (function ()
			for _, result in ipairs (results) do
				yield (name, result, tbl)
			end
		end)
	end
end

--[[
Returns a table iterator that yields table items only yielded by the both iterators.
	
@param function iterator1 A function that returns an iterator over a table
	yielding key, value, captures.
@param function iterator2 A function that returns an iterator over a table
	yielding key, value, captures.
@return function A function that returns an iterator over a table
	yielding key, value, captures.
--]]
local function intersect (iterator1, iterator2)
	return function (tbl)
		return wrap (function ()
			local key_set, capture_sets = {}, {}
			for key, value, captures in iterator1 (tbl) do
				key_set [key] = true
				capture_sets [key] = captures
			end
			for key, value, captures in iterator2 (tbl) do
				if key_set [key] then
					captures = merge (capture_sets [key], captures)
					yield (key, value, captures)
				end
			end
		end)
	end
end

--[[
Returns a table iterator that yields items yielded by the second iterator over items yielded by the first one.
	
@param function iterator1 A function that returns an iterator over a table
	yielding key, value, captures.
@param function iterator2 A function that returns an iterator over a table
	yielding key, value, captures.
@return function A function that returns an iterator over a table
	yielding key, value, captures.
--]]
local function enter (iterator1, iterator2)
	return function (tbl)
		return wrap (function ()
			for key1, value1, captures1 in iterator1 (tbl) do
				if type (value1) == 'table' then
					for key2, value2, captures2 in iterator2 (value1) do
						-- @todo: somehow include key1, value1 and match1?
						yield (key2, value2, merge (captures1, captures2))
					end
				end
			end
		end)
	end
end

--[[
Returns a table iterator that yields items from a cartesian product of two iterators.
	
@param function iterator1 A function that returns an iterator over a table
	yielding key, value, captures.
@param function iterator2 A function that returns an iterator over a table
	yielding key, value, captures.
@return function A function that returns an iterator over a table
	yielding key, value, captures.
--]]
local function cartesian (iterator1, iterator2)
	return function (tbl)
		return wrap (function ()
			for key1, value1, captures1 in iterator1 (tbl) do
				for key2, value2, captures2 in iterator2 (tbl) do
					yield ({ key1, key2 }, { value1, value2 }, merge (captures1, captures2))
				end
			end
		end)
	end
end

--[[
Returns a table iterator that yields items yielded by the first and then the second iterator.
	
@param function iterator1 A function that returns an iterator over a table
	yielding key, value, captures.
@param function iterator2 A function that returns an iterator over a table
	yielding key, value, captures.
@return function A function that returns an iterator over a table
	yielding key, value, captures.
--]]
local function union_all (iterator1, iterator2)
	return function (tbl)
		return wrap (function ()
			for key1, value1, captures1 in iterator1 (tbl) do
				yield (key1, value1, captures1)
			end
			for key2, value2, captures2 in iterator2 (tbl) do
				yield (key2, value2, captures2)
			end
		end)
	end
end

--[[
Returns a table iterator that yields items yielded by the first iterator, if any,
	and then the second iteratorm if the first yields nothing.
	
@param function iterator1 A function that returns an iterator over a table
	yielding key, value, captures.
@param function iterator2 A function that returns an iterator over a table
	yielding key, value, captures.
@return function A function that returns an iterator over a table
	yielding key, value, captures.
--]]
local function first (iterator1, iterator2)
	return function (tbl)
		return wrap (function ()
			local yielded = false
			for key1, value1, captures1 in iterator1 (tbl) do
				yielded = true
				yield (key1, value1, captures1)
			end
			if not yielded then
				for key2, value2, captures2 in iterator2 (tbl) do
					yield (key2, value2, captures2)
				end
			end
		end)
	end
end

--[[
Implementation of # (ipairs) and * (pairs) selectors.
	
@param function func ipairs or pairs.
@return false (not nil) for absent values.
--]]
local function iterator (func)
	return function (tbl)
		return wrap (function ()
			local numbered = {}
			for key, item in func (tbl or {}) do
				-- @todo: add parent name?
				yield (key, item)
			end
		end)
	end
end

--[[
Formatting tables
--]]

local empty = ''

--[[
Returns a formatter function made from chunks (strings or macros).
	
@param string|function ... Strings and macros.
@return function The formatter function that accepts a table and returns a table with a string for the loop body and a string for the separator.
--]]
local function chunks2func (chunks)
	return function (tbl)
		local expanded_chunks = {}
		for _, chunk in ipairs (chunks) do
			-- Do not simplify to ... and ... or ...
			if type (chunk) == 'table' then
				local macro, role = unpack (chunk)
				-- Run macro:
				local expanded = macro (tbl)
				if expanded == nil and role == empty then
					-- Propagate emptyness up:
					return nil
				else
					expanded_chunks [#expanded_chunks + 1] = expanded
				end
			else
				-- Record literal:
				expanded_chunks [#expanded_chunks + 1] = chunk
			end
		end
		return #expanded_chunks > 0 and concat (expanded_chunks) or nil
	end
end	

--[[
Returns a table of two formatter functions made from chunks (strings or macros): for the loop body and for the separator.
	
@param string|function ... Strings and macros.
@return table { The formatter function that accepts a table and returns a table with a string for the loop body, the same for the separator }.
--]]
-- @TODO: get a named table?
local function make_formatter (...)
	local separator = p.config.separator
	local chunks, separators = {}, {}
	for _, chunk in ipairs {...} do
		if type (chunk) == 'table' and chunk [2] == separator then
			separators[#separators + 1] = chunk
		else
			chunks [#chunks + 1] = chunk
		end
	end
	return { chunks2func (chunks), chunks2func (separators) }
end

--[[
The trivial and basic formatter; essentially, tostring().
	
@param mixed value Value to format.
@return string|nil Formatted value.
--]]
local function plain (value)
	return value and tostring (value) or nil
end

--[[
Default format for macro role.

@param string role
@return table  { The formatter function that accepts a table and returns a table with a string for the loop body, the same for the separator }.
--]]
local function default_format (role)
	return role == p.config.separator and make_formatter ', ' or { plain }
end

--[[
	Creates a macro function from a selector function and several fornat functions.
	@param string role Macro's role: necessary, optional, conditional, separator or key.
	@param function selector Function that accepts a table and
		returns an iterator yielding key, value, match, captures.
	@param function ... Formatter functions.
	@return table { Function implementing the macro, Macro's role: empty, optional, separator or key }.
--]]
local function make_macro (role, selector, ...)
	local formats = {...}
	if #formats == 0 then
		formats = { default_format (role) }
	end
	local conditional = p.config.conditional
	return { function (tbl)
		-- Return the first successful (non-nil) format:
		for i, formatter_pair in ipairs (formats) do
			local formatter, separator = unpack (formatter_pair)
			local formatted = {}
			for key, value, captures in selector (tbl) do
				local extended_value = captures and wrap_table (merge (type (value) == 'table' and value or {}, captures), key, tbl, value) or value
				formatted [#formatted + 1] = formatter (extended_value)
			end
			if #formatted == 0 and not (role == conditional and i == 1) then
				-- Even nil can be formatted by a constant fallback format:
				local formatted_nil = formatter ()
				if formatted_nil then
					formatted = { formatted_nil }
				end
			end
			if #formatted > 0 then
				return concat (formatted, separator and separator (tbl) or '')
			end
		end
		return nil -- all formats have failed.
	end, role }
end

--[[
	LPEG localisations and utilities
--]]
local P, S, V = lpeg.P, lpeg.S, lpeg.V
local C, Cg, Cb, Cmt, Cf, Cc = lpeg.C, lpeg.Cg, lpeg.Cb, lpeg.Cmt, lpeg.Cf, lpeg.Cc
local locale = lpeg.locale()
local any, never, space, alnum = P(1), P(false), locale.space, locale.alnum

--[[
	Returns an LPEG rule accepting any symbol except the arguments.
	@param LPEG rule|string ... LPEG rules to exclude.
	@return LPEG rule.
--]]
local function any_except (...)
	local rule, escape = any, P (p.config.escape)
	for _, forbidden in ipairs {...} do
		rule = escape * forbidden + rule - forbidden
	end
	return rule
end

--[[
	Returns a rule for a quoted string (the opening and closing quotes should be the same).
	@param LPEG rule|string quote The quote symbol(s).
	@return LPEG rule.
--]]
local function quoted (quote)
	local open = Cg( quote, 'open' )
	local close = ( C( quote ) * Cb'open' ):Cmt ( function (_, __, open, close) return open == close end )
	return open * C ( ( any - close ) ^ 1 ) * close
end

--[[
Return the sum of the values of a numbered table, wrapped in lpeg.P().

@param table.
@return userdata LPEG grammar.
--]]
local function sumP (tbl)
	sort (tbl, function (a, b)
		return #a > #b
	end)
	local sum = never
	for _, tbl in ipairs (tbl) do
		sum = sum + P (tbl)
	end
	return sum
end

--[[
Return the sum of the keys of a table, wrapped in lpeg.P(). Keys are ordered from longest to shortest.

@param table.
@return userdata LPEG grammar.
--]]
local function key_sum (tbl)
	local keys = {}
	for key, _ in pairs (tbl) do
		keys [#keys + 1] = key
	end
	return sumP (keys)
end

--[[
Return the sum of the values of a table, wrapped in lpeg.P(). Values are ordered from longest to shortest.

@param table.
@return userdata LPEG grammar.
--]]
local function value_sum (tbl)
	local values = {}
	for _, value in pairs (tbl) do
		values [#values + 1] = value
	end
	return sumP (values)
end

local equals, parentheses, quotes, comma = '=', { '(', ')' }, S[['"]], ','

--[[
Rule for regular expression delimiter.

@return userdata LPEG
--]]
local function regex_delim ()
	return any
		- (alnum + space + quotes + parentheses [1] + parentheses[2])
		- value_sum (p.config.operators)
		- p.config.condense - p.config.escape - p.config.pipe - p.config.key
end

--[[
Rule for flags for regular expressions.

@return userdata LPEG.
--]]
local function regex_flag ()
	return S'AiDsxXmUu' + p.config.condense
end

--[[
Return a rule for quoted selectors.

@return userdata LPEG for quoted selectors.
--]]
local function quoted_selector ()
	-- '', "", re'', re"", pcre'', pcre"", lua'', lua"", //, re//, pcre//, lua//:
	local selectors = {
		[quotes] = {
			re		= re,
			lua		= lua_pattern,
			gnu	 	= regex 'gnu',
			onig	= regex 'onig',
			posix	= regex 'posix',
			pcre	= regex 'pcre',
			tre 	= regex 'tre',
			['']	= exactly
		},
		[regex_delim ()] = {
			re		= re,
			lua		= lua_pattern,
			gnu	 	= regex 'gnu',
			onig	= regex 'onig',
			posix	= regex 'posix',
			pcre	= regex 'pcre',
			tre 	= regex 'tre',
			['']	= regex 'pcre'
		}
	}
	local quoted_selectors = never
	local flag = regex_flag()
	for delimiter, prefixes in pairs (selectors) do
		for prefix, selector in pairs (prefixes) do
			quoted_selectors = quoted_selectors + prefix * quoted (delimiter) * C ( flag ^ 0 ) / selector
		end
	end
	return quoted_selectors
end

--[[
	Define the format string grammar
--]]

--[[
	Generate a grammar rule for selector 'atithmetics' takinig into account priorities.
	@param LPEG rule operand An LPEG rule for the elementary operand.
	@return function An LPEG rule that parses an 'arithmetic' expression over iterators.
--]]
local function priorities (atomic)
	-- Selector 'arithmetics' indexed by priority:
	-- @todo: move to p.config
	local operators_by_priority = {
		{ [empty]							= intersect	},
		{ [p.config.operators.enter]		= enter },
		{ [p.config.operators.cartesian]	= cartesian },
		{ [p.config.operators.union]		= union_all },
		{ [p.config.operators.first]		= first }
	}
	local operand = atomic
	for priority, operators in ipairs (operators_by_priority) do
		operand = ( operand * space ^ 0 * Cg ( C ( key_sum (operators) ) * space ^ 0 * operand ) ^ 0 ):Cf ( function (selector1, operator, selector2)
			return operators [operator] (selector1, selector2)
		end )
	end
	return operand
end

--[[
Create metagrammar for the formatter string.

@return userdata LPEG grammar.
--]]
local function make_meta_grammar()
	return P{V'format' * -1,
		
		format		= ( V'macro' + V'literal' ) ^ 0 / make_formatter,

		-- << selector |format|fallback|format|...|last fallback format>>
		macro		= P(p.config.open) * V'role' * space ^ 0 * V'selector'
					* ( p.config.pipe * V'format' ) ^ 0 * p.config.close / make_macro,
		
		-- Optional macro role prefix (! for conditional; ? for optional; , for separator:
		role		= C ( P (p.config.conditional) + P (p.config.optional) + P (p.config.separator) + P (empty) ),

		-- Literal for the right half (formats) of <<|>>:
		literal		= C ( any_except (p.config.open, p.config.pipe, p.config.close) ^ 1 ),
			
		-- Complex table selector (operations space for intersect, . for nesting, * for cartesian product, + for union:
		selector	= priorities (V'simple') + Cc ( self ),
		
		-- Atomic table selectors for the left half of <<|>>:
		simple		= parentheses [1] * V'selector' * parentheses [2]
					+ V'ipairs' + V'pairs' + V'quoted' + V'func' + V'unquoted' + V'dynamic',
		
		-- # (ipairs() iterator):
		ipairs		= P (p.config.ipairs) * Cc (iterator (ipairs)),
		
		-- $ (pairs() iterator):
		pairs		= P (p.config.pairs) * Cc (iterator (pairs)),
		
		-- Optional equal sign, for selectors, filtering values:
		equals		= ( equals * Cc (true) + Cc (false) ) * space ^ 0,
		
		-- [=] 'key' | "key" | /pcre/ | pcre'' | pcre"" | pcre// | re'' | re"" | re// | lua'' | lua"" | lua//:
		quoted		= V'equals' * quoted_selector () / table_iterator,
		
		-- function ():
		func		= V'word' * space ^ 0 * parentheses [1] * space ^ 0
					* ( V'parameter' * ( space ^ 0 * comma * V'parameter' ) ^ 0 ) ^ -1
					* space ^ 0 * parentheses [2] / function2table_iterator,

		parameter	= Ct ( ( V'macro' + V'word' ) ^ 1 ) / chunks2func + quoted'"' + quoted"'",
		
		-- unquoted static iterator for values. Simple and faster than dynamic.
		unquoted	= V'equals' * ( V'word' / try_tonumber / exactly )
					* #( P (space) + value_sum (p.config.operators) + p.config.separator + p.config.pipe + p.config.close ) / table_iterator,
		
		-- Single item selector with dynamic key. Empty selector means the variable itself rather than one of its items.
		-- Returns false (not nil) for absent value:
		dynamic		= V'equals' * ( V'macro' + V'word' ) ^ 1 / function (value, ...)
			local formatter = chunks2func {...}
			return function (tbl)
				local filter = try_tonumber (formatter (tbl))
				return table_iterator (value, exactly (filter)) (tbl)
			end
		end,
		
		-- A word with no spaces or tags:
		word		= C( any_except (
			equals, quotes, regex_delim(), parentheses [1], parentheses [2], space,
			p.config.open, p.config.pipe, p.config.close, value_sum (p.config.operators), p.config.separator
		) ^ 1 ),
	}
end

--[[
Rebuild the metagrammar according to p.p.config.
--]]
function p.initialise ()
	p.meta = make_meta_grammar()
end

--[[
Serialise the table s according to format and p.p.config.

@param table table The table to serialise.
@param string format The format string.
@return string The formatted table.
--]]
function p.format (tbl, format)
	if not p.meta then
		p.initialise ()
	end
	local compiled = p.meta:match (format)
	return compiled and tostring (compiled [1] (wrap_table (tbl))) or format .. ' does not compile'
end

return p