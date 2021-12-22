-- @TODO: mergers.
--[[
	Table utilities
--]]

local concat, sort = table.concat, table.sort

local rep = string.rep
local function dump (var)
	local shown = {}
	local function helper (var, indent)
		if type (var) == 'string' then
			return "'" .. var .. "'"
		elseif type (var) ~= 'table' or shown [var] then
			return tostring (var)
		else
			shown [var] = true
			local serialised = {}
			for key, value in pairs (var) do
				serialised [#serialised + 1] = '\n' .. rep ('\t', indent + 1) .. helper (key, indent + 1) .. ' = ' .. helper (value, indent + 1)
			end
			return (tostring (var) or 'falsy') .. ' {' .. concat (serialised, ',') .. '\n' .. rep ('\t', indent) .. '}'
		end
	end
	return helper (var, 0)
end

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
local coroutine = require 'coroutine'
local wrap, yield = coroutine.wrap, coroutine.yield
local sub = string.sub

--[[
	Make a string iterator from a matcher.

	@param function matcher A function accepting a string and offset
		and returning match's offset, match's finish and the match itself.
	@return function A string iterator.
--]]
local function string_iterator (matcher)
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

-- Perl compatible regular expressions:
local new_pcre = require 'rex_pcre'.new
--[[
	Make a comparator function from a Perl-compatible regular expression.
	
	@param string expr Perl-compatible regular expression.
	@return function A comparator function accepting a string and an offset
		and returning an offset and end position of the match and a table of captures.
--]]
local function pcre (expr)
	local valid, compiled = pcall (new_pcre, expr)
	if not valid then
		return error_msg ('Perl-compatible regular expression ' .. expr .. ' does not compile')
	end
	return string_iterator (function (str, offset)
		return compiled:tfind (str, offset)
	end)
end

-- LPeg's re module:
local lpeg = require 'lpeg'
local Cp, Ct = lpeg.Cp, lpeg.Ct
local new_re = require 'lualibs/re'.compile
--[[
	Make a comparator function from an LPEG Re selector.
	
	@param string expr LPEG Re selector.
	@return function A comparator function accepting a string and an offset
		and returning an offset and end position of the match and a table of captures.
--]]
local function re (expr)
	local valid, compiled = pcall (new_re, expr)
	if not valid then
		return error_msg ('LPEG Re selector ' .. expr .. ' does not compile')
	end
	compiled = Cp() * Ct (compiled) * Cp()
	return string_iterator (function (str, offset)
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
	@return function A comparator function accepting a string and an offset
		and returning an offset and end position of the match and a table of captures.
--]]
-- @todo: improve.
local find = (mw and mw.ustring or string).find
local function lua_pattern (expr)
	return string_iterator (function (str, offset)
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
				if tbl then
					yield (exact_key, tbl [exact_key])
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
--]]
local function self (tbl)
	return wrap (function ()
		yield (type (tbl) == 'table' and tbl ['@'] or '', tbl)
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
	Implementation of # (ipairs) and * (pairs) selectors.
	@param function func ipairs or pairs.
	@return false (not nil) for absent values.
--]]
local function iterator (func)
	return function (tbl)
		return wrap (function ()
			local numbered = {}
			for key, item in func (tbl or {}) do
				yield (key, item)
			end
		end)
	end
end

--[[
	Formatting tables
--]]

--[[
	Returns a formatter function made from chunks (strings or macros).
	@param string|function ... Strings and macros.
	@return ?function The formatter function that accepts a table and returns a string.
--]]
local function chunks2func (...)
	local chunks = {...}
	return function (tbl)
		local formatted = {}
		for i, chunk in ipairs (chunks) do
			-- Do not simplify to ... and ... or ...
			if type (chunk) == 'function' then
				formatted[i] = chunk (tbl)
			else
				formatted[i] = chunk
			end
			if formatted[i] == nil then
				-- Propagate emptyness up:
				return nil
			end
		end
		return concat (formatted)
	end
end
	
--[[
	Returns a formatter function made from chunks (strings or macros).
	@param table loop A table of strings and macros for the macro body.
	@param table separator A macro for the separator.	
	@return table Two formatter functions that accept a table and return a string:
		{ for the macro body, for the separator }.
--]]
local function make_formatter (loops, separator)
	return { chunks2func (unpack (loops)), separator and chunks2func (separator) }
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
	Creates a macro function from a selector function and several fornat functions.
	@param function selector Function that accepts a table and
		returns an iterator yielding key, value, match, captures.
	@param function ... Formatter functions.
	@return function Function implementing the macro.
--]]
local function make_macro (selector, ...)
	local formats = {...}
	return function (tbl)
		-- Return the first successful (non-nil) format:
		for _, format_pair in ipairs (formats) do
			local format, separator = unpack (format_pair)
			local formatted = {}
			for key, value, captures in selector (tbl) do
				local extended_value = captures and wrap_table (merge (type (value) == 'table' and value or {}, captures), key, tbl, value) or value
				formatted [#formatted + 1] = format (extended_value)
			end
			if #formatted == 0 then
				-- Even nil can be formatted by a constant fallback format:
				local formatted_nil = format ()
				if formatted_nil then
					formatted = { formatted_nil }
				end
			end
			if #formatted > 0 then
				return concat (formatted, separator and separator (tbl) or '')
			end
		end
		return nil -- all formats have failed.
	end
end

--[[
	LPEG localisations and utilities
--]]
local P, S, V = lpeg.P, lpeg.S, lpeg.V
local C, Cg, Cb, Cmt, Cf, Cc = lpeg.C, lpeg.Cg, lpeg.Cb, lpeg.Cmt, lpeg.Cf, lpeg.Cc
local locale = lpeg.locale()

local empty, any, never, escape, space, alnum = '', P(1), P(false), P'\\', locale.space, locale.alnum

--[[
	Returns an LPEG rule accepting any symbol except the arguments.
	@param LPEG rule|string ... LPEG rules to exclude.
	@return LPEG rule.
--]]
local function any_except (...)
	local rule = any
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

local open, pipe, close, equals = '<<', '|', '>>', '='
local parentheses = { '(', ')' }
local dot, comma, at, underscore = '.', ',', '@', '_'

-- Return the sum of the keys of a table, wrapped in lpeg.P(). Keys are ordered from longest to shortest:
local function key_sum (tbl)
	local keys = {}
	for key, _ in pairs (tbl) do
		keys [#keys + 1] = key
	end
	sort (keys, function (a, b)
		return #a > #b
	end)
	local sum = never
	for _, key in ipairs (keys) do
		sum = sum + P (key)
	end
	return sum
end

local quotes = S[['"]]
-- Rule for regular expression delimiter:
local regex_delim = any - alnum - underscore - space - escape - quotes - pipe - parentheses [1] - parentheses[2] - dot - at

-- '', "", re'', re"", pcre'', pcre"", lua'', lua"", //, re//, pcre//, lua//:
local selectors = {
	[quotes] = {
		re		= re,
		pcre	= pcre,
		lua		= lua_pattern,
		['']	= exactly
	},
	[regex_delim] = {
		re		= re,
		pcre	= pcre,
		lua		= lua_pattern,
		['']	= pcre
	}
}
local quoted_selectors = never
for delimiter, prefixes in pairs (selectors) do
	for prefix, selector in pairs (prefixes) do
		quoted_selectors = quoted_selectors + prefix * quoted (delimiter) / selector
	end
end

--[[
	Define the format string grammar
--]]

--[[
	Generate a grammar rule for selector 'atithmetics' of certain priority.
	@param LPEG rule operand An LPEG rule for the operands.
	@param table operators An associative table of functions indexed by operators.
	@return function The resulting operation as a function returning a table iterator.
--]]
local function priority (operand, operators)
	return ( operand * space ^ 0 * Cg ( C ( key_sum (operators) ) * space ^ 0 * operand ) ^ 0 ):Cf ( function (selector1, operator, selector2)
		return operators [operator] (selector1, selector2)
	end )
end

local ipairs_token, pairs_token = P'#', P'*'

local meta = P{V'format' * -1,
	
	format		= Ct ( ( V'macro' + V'literal' ) ^ 0 ) * V'separator' ^ 0 / make_formatter,

	-- << selector |format|fallback|format|...|last fallback format>>:
	macro		= P(open) * -P(comma) * V'selector' * ( ( pipe * V'format' ) ^ 1 + Cc { plain } ) * close / make_macro,
	
	-- <<,>> | <<,selector>> | <<,selector|>>:
	separator	= P(open) * comma * Cc (self) * ( ( pipe * V'format' ) ^ 1 + Cc { chunks2func ', ' } ) * close / make_macro,
	
	-- Literal for the right half (formats) of <<|>>:
	literal		= C ( any_except (open, pipe, close) ^ 1 ),
		
	selector	= V'priority1' + empty * Cc ( self ),
	
	-- Selector 'arithmetics':
	priority1	= priority (V'simple', {
		[dot]	= enter,
		[empty]	= intersect
	}),
	
	-- Possible table selectors for the left half of <<|>>:
	simple		= parentheses [1] * V'selector' * parentheses [2] + V'ipairs' + V'pairs' + V'quoted' + V'func' + V'unquoted' + V'dynamic',
	
	-- # (ipairs() iterator):
	ipairs		= ipairs_token * Cc (iterator (ipairs)),
	
	-- * (pairs() iterator):
	pairs		= pairs_token * Cc (iterator (pairs)),
	
	-- Optional equal sign:
	equals		= ( equals * Cc (true) + Cc (false) ) * space ^ 0,
	
	-- [=] 'key'|"key"|/pcre/|pcre''|pcre""|pcre//|re''|re""|re//|lua''|lua""|lua//:
	quoted		= V'equals' * quoted_selectors / table_iterator,
	
	-- function ():
	func		= V'word' * space ^ 0 * parentheses [1] * space ^ 0
				* ( V'parameter' * ( space ^ 0 * comma * V'parameter' ) ^ 0 ) ^ -1
				* space ^ 0 * parentheses [2] / function2table_iterator,

	parameter	= ( V'macro' + V'word' ) ^ 1 / chunks2func + quoted'"' + quoted"'",
	
	-- unquoted static iterator for values. Simple and faster than dynamic.
	unquoted	= V'equals' * ( V'word' / try_tonumber / exactly ) * #( P (space) + dot + comma + pipe + close ) / table_iterator,
	
	-- Single item selector with dynamic key. Empty selector means the variable itself rather than one of its items.
	-- Returns false (not nil) for absent value:
	dynamic		= V'equals' * ( V'macro' + V'word' ) ^ 1 / function (value, ...)
		local formatter = chunks2func (...)
		return function (tbl)
			local filter = try_tonumber (formatter (tbl))
			return table_iterator (value, exactly (filter)) (tbl)
		end
	end,
	
	-- A word with no spaces but with <<>> tags:
	word		= C( any_except (open, pipe, close, equals, quotes, regex_delim, parentheses [1], parentheses [2], dot, comma, space) ^ 1 ),
}

local p = { wrap = wrap_table, dump = dump }

function p.serialise (tbl, format)
	local compiled = meta:match (format)
	return compiled and tostring (compiled [1] (wrap_table (tbl))) or format .. ' does not compile'
end

return p