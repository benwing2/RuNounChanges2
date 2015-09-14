local export = {}

local m_links = require("Module:links")

local u = mw.ustring.char
local notes_ranges = {
  -- First three represent symbols in ISO-8859-1
  -- Including ÷ (U+00F7) × (U+00D7) § (U+00B7) ¤ (U+00A4)
  {0xA1,0xBF},
  {0xD7,0xD7}, -- ×
  {0xF7,0xF7}, -- ÷
  -- Next two are "General Punctuation" minus non-spacing chars
  -- First one includes † (U+2020) ‡ (U+2021) • (U+2022)  ※ (U+203B) ⁕ (U+2055)
  {0x2010,0x2027},
  {0x2030,0x205E},
  -- Next one is "Superscripts and Subscripts" and "Currency Symbols"
  {0x2070,0x20CF},
  -- Next one is a whole series of symbol ranges
  {0x2100,0x2B5F},
  -- Next one is "Supplemental Punctuation"
  {0x2E00,0x2E3F}
}

local unicode_ranges = {}
for _, range in ipairs(notes_ranges) do
  table.insert(unicode_ranges, u(range[1]) .. "-" .. u(range[2]))
end
local unicode_range_str = table.concat(unicode_ranges, "")
local notes_re = "[%*%~%@%#%$%%%^%&%+0-9_ " .. unicode_range_str .. "]*"

local function manipulate_entry(entries, f)
	entries = mw.text.split(mw.ustring.gsub(entries, "^%s*(.-)%s*$", "%1"), "%s*,%s*")
	
	local sep = ""
	local ret = ""
	
	for _, entry in ipairs(entries) do
		ret = ret .. sep .. (entry == "-" and "—" or entry == "" and "" or f(entry))
		sep = ", "
	end
	
	return ret
end

local function gather_args(frame)
	local args = frame.args
	local lang = args["lang"]
	
	if not args[1] then
		args = frame:getParent().args
	end
	
	if not lang then
		lang = args[1]
		local n = 1
		while args[n] do
			args[n] = args[n + 1]
			n = n + 1
		end
	end
	
	return lang, args
end

function export.get_notes(entry)
	local notes
	entry, notes = mw.ustring.match(entry, "^(.-)(" .. notes_re .. ")$")
	
	if notes ~= "" then
		notes = "<sup>" .. mw.ustring.gsub(notes, "_", " ") .. "</sup>"
	end
	
	return entry, notes
end

function export.get_initial_notes(entry)
	local notes
	notes, entry = mw.ustring.match(entry, "^(" .. notes_re .. ")(.*)$")

	if notes ~= "" then
		notes = "<sup>" .. mw.ustring.gsub(notes, "_", " ") .. "</sup>"
	end

	return notes, entry
end

function export.linkify_entry(lang, entries, allow_self_link)
	if type(lang) == "table" then
		local args
		lang, args = gather_args(lang)
		entries = args[1]
		allow_self_link = (args["allowSelfLink"] or "") ~= ""
	end
	lang = require("Module:languages").getByCode(lang)
	
	local function f(entry)
		local e, notes = export.get_notes(entry)
		return m_links.language_link(e, nil, lang, nil, allow_self_link) .. notes
	end
	
	return manipulate_entry(m_links.remove_links(entries), f)
end

function export.translit_entry(lang, entries)
	if type(lang) == "table" then
		local args
		lang, args = gather_args(lang)
		entries = args[1]
	end
	lang = require("Module:languages").getByCode(lang)

	local function f(entry)
		local e, notes = export.get_notes(entry)
		return lang:transliterate(e) .. notes
	end
	
	return manipulate_entry(m_links.remove_links(entries), f)
end

function export.format_entry(lang, entries)
	if type(lang) == "table" then
		local args
		lang, args = gather_args(lang)
		entries = args[1]
	end
	
	return manipulate_entry(m_links.remove_links(entries), function(entry) local e, n = export.get_notes(entry); return e .. n end)
end

function export.first_entry(lang, entries)
	if type(lang) == "table" then
		local args
		lang, args = gather_args(lang)
		entries = args[1]
	end

	local entry = mw.text.split(mw.ustring.gsub(entries, "^%s*(.-)%s*$", "%1"), "%s*,%s*")[1]
	
	local e, notes = export.get_notes(entry)
	return e .. notes
end

return export
