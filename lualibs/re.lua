--[[
	Based on the code by Roberto Ierusalimschy:
	https://github.com/lua/lpeg/blob/master/re.lua
	
	Modified initially for traditio.wiki and then for SummaryII by Alexander Mashin:
		* <		-- back assertion	(lpeg.B),
		* {` `}	-- constant capture	(lpeg.Cc),
		* {# #}	-- argument capture	(lpeg.Carg),
		* {/ /} -- a regular expression match,
		* case_insensitive parametre for compile().
--]]

--
-- Copyright 2007-2023, Lua.org & PUC-Rio  (see 'lpeg.html' for license)
-- written by Roberto Ierusalimschy
--

-- imported functions and modules
local tonumber, type, print, error = tonumber, type, print, error
local setmetatable = setmetatable
local __found, m = pcall (require, 'lpeg')
-- In MediaWiki, lpeg has to be made available as a global.
m = __found and m or lpeg

-- 'm' will be used to parse expressions, and 'mm' will be used to
-- create expressions; that is, 're' runs on 'm', creating patterns
-- on 'mm'
local mm = m

-- pattern's metatable
local mt = getmetatable(mm.P(0)) or {
	-- In MediaWiki, no metatable for userdata.
	__unm = function (a) return -lpeg.P(a) end
  , __len = function (a) return #lpeg.P(a) end
  , __add = function (a, b) return lpeg.P(a) + lpeg.P(b) end
  , __mul = function (a, b) return lpeg.P(a) * lpeg.P(b) end
  , __div = function (a, b) return lpeg.P(a) / b end
  , __mod = function (a, b) return lpeg.P(a) % b end
  , __pow = function (a, b) return lpeg.P(a) ^ b end
}

local version = _VERSION

-- We need re here.
local re = {
	string = string,
	rex_flavour = 'pcre2'
}

-- Injecting regular expressions:
local function load_library (library)
	if _G [library] then
		return _G [library]
	end
	local ok, lib = pcall (require, library)
	if not ok or not lib then
		return nil
	end
	return lib
end

local regex_flavours = (function ()
	local flavours = m.P (false)
	local loaded_flavours = {}
	for _, flavour in ipairs { 'posix', 'pcre2', 'pcre', 'onig', 'tre' } do
		local lib = load_library ('rex_' .. flavour)
		if lib then
			flavours = flavours + m.C (flavour) * m.Cc (lib.new)
			loaded_flavours [flavour] = lib.new
		end
	end
	local default = re.rex_flavour
	-- Default rex flavour:
	if loaded_flavours [default] then
		flavours = flavours + m.Cc (default) * m.Cc (loaded_flavours [default])
	end
	return flavours
end) ()

-- No more global accesses after this point
local pcall = pcall
_ENV = { pcall = pcall, unpack = table.unpack }

-- @TODO: multibyte characters.
local any, start = m.P (1), m.P (true)

-- Pre-defined names
local Predef = { nl = m.P"\n" }

local mem
local fmem
local gmem

local function updatelocale ()
  mm.locale(Predef)
  Predef.a = Predef.alpha
  Predef.c = Predef.cntrl
  Predef.d = Predef.digit
  Predef.g = Predef.graph
  Predef.l = Predef.lower
  Predef.p = Predef.punct
  Predef.s = Predef.space
  Predef.u = Predef.upper
  Predef.w = Predef.alnum
  Predef.x = Predef.xdigit
  Predef.A = any - Predef.a
  Predef.C = any - Predef.c
  Predef.D = any - Predef.d
  Predef.G = any - Predef.g
  Predef.L = any - Predef.l
  Predef.P = any - Predef.p
  Predef.S = any - Predef.s
  Predef.U = any - Predef.u
  Predef.W = any - Predef.w
  Predef.X = any - Predef.x
  mem = {}	-- restart memoization
  fmem = {}
  gmem = {}
  local mt = {__mode = "v"}
  setmetatable(mem, mt)
  setmetatable(fmem, mt)
  setmetatable(gmem, mt)
end

updatelocale()

local I = m.P(function (s,i) print(i, s:sub(1, i-1)); return i end)

local function patt_error (s, i)
  local msg = (#s < i + 20) and s:sub(i)
							 or s:sub(i,i+20) .. "..."
  msg = ("pattern error near '%s'"):format(msg)
  error(msg, 2)
end

local function mult (p, n)
  local np = mm.P(true)
  while n >= 1 do
	if n%2 >= 1 then np = np * p end
	p = p * p
	n = n/2
  end
  return np
end

local function equalcap (s, i, c)
  if type(c) ~= "string" then return nil end
  local e = #c + i
  if s:sub(i, e - 1) == c then return e else return nil end
end

local S = (Predef.space + "--" * (any - Predef.nl)^0)^0

local name = m.R("AZ", "az", "__") * m.R("AZ", "az", "__", "09")^0

local arrow = S * "<-"

local seq_follow = m.P"/" + ")" + "}" + ":}" + "~}" + "|}" + "`}" + (name * arrow) + -1

name = m.C(name)

-- a defined name only have meaning in a given environment
local Def = name * m.Carg(1)

local function getdef (id, defs)
  local c = defs and defs[id]
  if not c then error("undefined name: " .. id) end
  return c
end

-- match a name and return a group of its corresponding definition
-- and 'f' (to be folded in 'Suffix')
local function defwithfunc (f)
  return m.Cg(Def / getdef * m.Cc(f))
end

local num = m.C(m.R"09"^1) * S / tonumber

local String = "'" * m.C((any - "'")^0) * "'" +
			   '"' * m.C((any - '"')^0) * '"'

local defined = "%" * Def / function (c, Defs)
	local cat =  Defs and Defs [c] or Predef [c]
	if not cat then error ("name '" .. c .. "' undefined") end
	return cat
end
	
local function adddef (t, k, exp)
  if t[k] then
	error("'"..k.."' already defined as a rule")
  else
	t[k] = exp
  end
  return t
end

local function firstdef (n, r) return adddef({n}, n, r) end

local function NT (n, b)
  if not b then
	error("rule '"..n.."' used outside a grammar")
  else return mm.V(n)
  end
end

local function do_nothing (...)
	return ...
end

local string = re.string
local gmatch, upper, lower = string.gmatch, string.upper, string.lower
local set = mm.S

local function metagrammar (case_insensitive)

	local string2range = case_insensitive and function (str)
		local uppercase, lowercase = upper (str), lower (str)
		return uppercase ~= lowercase and mm.R (uppercase) + mm.R (lowercase) or mm.R (str)
	end or mm.R

	local Range = mm.Cs(any * (m.P'-' / '') * (any - ']')) / string2range

	local add_capital = case_insensitive and function (char)
		local uppercase, lowercase = upper (char), lower (char)
		return uppercase ~= lowercase and set (uppercase .. lowercase) or char
	end or do_nothing
	
	local item = (defined + Range + any / add_capital ) / m.P
	
	local Class = '[' * (m.C (m.P'^' ^ -1))	-- optional complement symbol.
				* m.Cf (item * (item - ']') ^ 0, mt.__add) /
					function (c, p) return c == '^' and any - p or p end
				* ']'
	
	local string2pattern = case_insensitive and function (str)
		local pattern = start
		for char in gmatch (str, '.') do
			local uppercase, lowercase = upper (char), lower (char)
			local char = uppercase ~= lowercase and set (uppercase .. lowercase) or char
			pattern = pattern * char
		end
		return pattern
	end or mm.P
	
	local exp = m.P{ "Exp",
	  Exp = S * ( m.V"Grammar"
			-- % on userdata does nt seem to work in this context, even with a metatable.
			-- Therefore, falling back to lpeg.Cf.  	
			--	+ m.V"Seq" * ("/" * S * m.V"Seq" % mt.__add)^0 );
			+ m.Cf(m.V"Seq" * ("/" * S * m.V"Seq")^0, mt.__add) );
		-- % on userdata does nt seem to work in this context, even with a metatable.
		-- Therefore, falling back to lpeg.Cf.  
	  -- Seq = (m.Cc(m.P"") * (m.V"Prefix" % mt.__mul)^0)
	  Seq = m.Cf(m.Cc(m.P"") * m.V"Prefix"^0 , mt.__mul)
			* (#seq_follow + patt_error);
	  Prefix = "&" * S * m.V"Prefix" / mt.__len
			 + "!" * S * m.V"Prefix" / mt.__unm
			 -- < -- back assertion. Added for traditio.wiki by Alexander Mashin:
			 + "<" * S * m.V"Prefix" / mm.B	 	
			 + m.V"Suffix";
		-- % on userdata does nt seem to work in this context, even with a metatable.
		-- Therefore, falling back to lpeg.Cf. 
		-- Suffix = m.V"Primary" * S *
		Suffix = m.Cf(m.V"Primary" * S *
			  ( ( m.P"+" * m.Cc(1, mt.__pow)
				+ m.P"*" * m.Cc(0, mt.__pow)
				+ m.P"?" * m.Cc(-1, mt.__pow)
				+ "^" * ( m.Cg(num * m.Cc(mult))
						+ m.Cg(m.C(m.S"+-" * m.R"09"^1) * m.Cc(mt.__pow))
						)
				+ "->" * S * ( m.Cg((String + num) * m.Cc(mt.__div))
							 + m.P"{}" * m.Cc(nil, m.Ct)
							 + defwithfunc(mt.__div)
							 )
				+ "=>" * S * defwithfunc(mm.Cmt)
				+ ">>" * S * defwithfunc(mt.__mod)
				+ "~>" * S * defwithfunc(mm.Cf)
		-- % on userdata does nt seem to work in thiscontext, even with a metatable.
		-- Therefore, falling back to lpeg.Cf.			 
		-- ) % function (a,b,f) return f(a,b) end * S
		--)^0;
			) * S
		)^0, function (a,b,f) return f(a,b) end );
	  Primary = "(" * m.V"Exp" * ")"
				+ String / string2pattern
				+ Class
				+ defined
				+ "{:" * (name * ":" + m.Cc(nil)) * m.V"Exp" * ":}" /
						 function (n, p) return mm.Cg(p, n) end
				+ "=" * name / function (n) return mm.Cmt(mm.Cb(n), equalcap) end
				+ m.P"{}" / mm.Cp
				+ "{~" * m.V"Exp" * "~}" / mm.Cs
				+ "{|" * m.V"Exp" * "|}" / mm.Ct
				-- Inject regular expressions:
				+ '{' * S * regex_flavours * S
					* '/' * mm.C (( any - '/' + '\\/') ^ 1) * '/'
					* mm.C (mm.S 'imsxUX' ^ 0)
					* S * '}' / function (flavour, compiler, regex, flags)
						local absolute_start = false
						if string.sub (regex, 1, 1) == '^' then
							-- Anchoring to string's absolute beginning is required:
							absolute_start = true
						else
							-- Force anchoring to current position:
							regex = '^' .. regex
						end
						local valid, result = pcall (compiler, regex, flags)
							if not valid or not result then
								error (flavour .. ' regular expression /' .. regex .. '/' .. (flags or '') .. ' does not compile: ' .. (result or '?'))
							end
						return mm.Cmt ( m.P (true), function (s, p)
							if absolute_start and p > 1 then
								-- Not the beginning of the string, so already failed:
								return false
							end
							local remainder = string.sub (s, p)
							local start, finish, captures = compiled:tfind (remainder)
							if not start then
								return false
							end
							if #captures == 0 then
								captures = { string.sub (remainder, 1, finish) }
							end
							return p + finish, unpack (captures))
						end)
					end
				+ "{" * m.V"Exp" * "}" / mm.C
				-- {` `} -- constant capture. Inserted for traditio.wiki by Alexander Mashin:
				+ "{`" * Predef.space^0 * m.C((any - "`")^1) * S * "`}" / mm.Cc
				-- {# #} == argument capture. Inserted for traditio.wiki by Alexander Mashin:
				+ "{#" * S * Def * "#}" / getdef
				+ m.P"." * m.Cc(any)
				+ (name * -arrow + "<" * name * ">") * m.Cb("G") / NT;
	  Definition = name * arrow * m.V"Exp";
	  Grammar = m.Cg(m.Cc(true), "G") *
				-- % on userdata does nt seem to work in thiscontext, even with a metatable.
				-- Therefore, falling back to lpeg.Cf.	  
				-- ((m.V"Definition" / firstdef) * (m.V"Definition" % adddef)^0) / mm.P
				m.Cf(m.V"Definition" / firstdef * m.Cg(m.V"Definition")^0, adddef) / mm.P
	}

	return S * m.Cg(m.Cc(false), "G") * exp / mm.P * (-any + patt_error)
end


local function compile (p, defs, case_insensitive)
	if mm.type(p) == "pattern" then return p end   -- already compiled
	local cp = metagrammar (case_insensitive):match (p, 1, defs)
	if not cp then error("incorrect pattern", 3) end
	return cp
end

local function match (s, p, i)
  local cp = mem[p]
  if not cp then
	cp = compile(p)
	mem[p] = cp
  end
  return cp:match(s, i or 1)
end

local function find (s, p, i)
  local cp = fmem[p]
  if not cp then
	cp = compile(p) / 0
	cp = mm.P{ mm.Cp() * cp * mm.Cp() + 1 * mm.V(1) }
	fmem[p] = cp
  end
  local i, e = cp:match(s, i or 1)
  if i then return i, e - 1
  else return i
  end
end

local function gsub (s, p, rep)
  local g = gmem[p] or {}   -- ensure gmem[p] is not collected while here
  gmem[p] = g
  local cp = g[rep]
  if not cp then
	cp = compile(p)
	cp = mm.Cs((cp / rep + 1)^0)
	g[rep] = cp
  end
  return cp:match(s)
end

-- exported names. re has been declare above.
re.compile, re.match, re.find, re.gsub, re.updatelocale = compile, match, find, gsub, updatelocale

if version == "Lua 5.1" then _G.re = re end

return re
