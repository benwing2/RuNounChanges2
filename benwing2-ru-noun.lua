--[=[
	This module contains functions for creating inflection tables for Russian
	nouns.

	Form of arguments: One of the following:
		1. LEMMA|DECL|BARE|PLSTEM (all arguments optional)
		2. ACCENT|LEMMA|DECL|BARE|PLSTEM (all arguments optional)
		3. multiple sets of arguments separated by the literal word "or"

	Arguments:
		ACCENT: Accent pattern (a b c d e f b' d' f' f''). For compatibility,
		   can also be a number, 1 through 6 equivalent to a through f and
		   4* and 6* are equivalent to d' and f', except that for
		   3rd-declension feminine nouns, 2 maps to b' instead of b, and 6
		   maps to f'' instead of f. Multiple values can be specified,
		   separated by commas. If omitted, defaults to a or b depending on
		   the position of stress on the lemma or explicitly-specified
		   declension.
		LEMMA: Lemma form (i.e. nom sg or nom pl), with appropriately-placed
		   stress; or the stem, if an explicit declension is specified
		   (in this case, the declension usually looks like an ending, and
		   the stem is the portion of the lemma minus the ending). In the
		   first argument set (i.e. first set of arguments separated
		   by "or"), defaults to page name; in later sets, defaults to lemma
		   of previous set. A plural form can be given, and causes argument
		   n= to default to n=p (plurale tantum). Normally, an accent is
		   required if multisyllabic, and unaccented monosyllables with
		   automatically be stressed; prefix with * to override both behaviors.
		DECL: Declension field. Normally omitted to autodetect based on the
		   lemma form; see below.
		BARE: Present for compatibility; don't use this in new template calls.
		   Irregular nom sg or gen pl form (specifically, the form used for
		   cases with no suffix or with a nonsyllabic suffix -ь/-й/-ъ). If
		   present, LEMMA for masculine nouns should omit the extra vowel
		   normally present in the nom sg. In new template calls, use the *
		   or (2) special cases in the declension field (see below), or failing
		   that, use an explicit override nom_sg= or gen_pl=.
		PLSTEM: special plural stem (defaults to stem of lemma)

	Additional named arguments:
		a: animacy (a = animate, i = inanimate, b = both, otherwise inanimate)
		n: number restriction (p = plural only, s = singular only, b = both;
		   defaults to both unless the lemma is plural, in which case it
		   defaults to plural only)
		CASE_NUM or par/loc/voc: override (or multiple values separated by
		   commas) for particular form; forms auto-linked; can have raw links
		pltail: Specify something (usually a * or similar) to attach to
		   the end of plural forms when there's more than one. Used in
		   conjunction with notes= to indicate that alternative plural forms
		   are obsolete, poetic, etc.
		pltailall: Same as pltail= but added to added to all forms even if
		   there's only one.
		sgtail, sgtailall: Same as pltail=, pltailall= but for the singular.
		CASE_NUM_tail: Similar to pltailall but restricted to a single form.
		suffix: Add a suffix such as ся to all forms.

	Case abbreviations:
		nom: nominative
		gen: genitive
		dat: dative
		acc: accusative
		ins: instrumental
		pre: prepositional
		par: partitive
		loc: locative
		voc: vocative

	Number abbreviations:
		sg: singular
		pl: plural

	Declension field:
		One of the following for regular nouns:
			(blank)
			GENDER
			-PLVARIANT
			GENDER-PLVARIANT
			DECLTYPE
			DECLTYPE/DECLTYPE
			(also, can append various special-case markers to any of the above)
		Or one of the following for adjectival nouns:
			+
			+ь
			+short, +mixed or +proper
			+DECLTYPE
		GENDER if present is m, f, n or 3f; for regular nouns, required if the
			lemma ends in -ь or is plural, ignored otherwise. 3f is the same
			as f but in the case of a plural lemma in -и, detects a
			third-declension feminine with singular in -ь rather than a
			first-declension feminine with singular in -а or -я.
		PLVARIANT is one way of specifying declensions with irregular plurals;
			the only currently allowed value is -ья and will select a
			mixed declension that has some normal declension in its singular
			and the -ья plural declension. See also special case (1).
		DECLTYPE is an explicit declension type. Normally you shouldn't use
			this, and should instead let the declension type be autodetected
			based on the ending, supplying the appropriate hint if needed
			(gender for regular nouns, +ь for adjectives). If provided, the
			declension type is usually the same as the ending, and if present,
			the lemma field should be just the stem, without the ending.

			Possibilities for regular nouns are (blank) or # for hard-consonant
			declension, а, я, о, е or ё, е́, й, ья, ье or ьё, ь-m, ь-f,
			ин, ёнок or онок or енок, ёночек or оночек or еночек, мя,
			-а or #-а, ь-я, й-я, о-и or о-ы, -ья or #-ья, $ (invariable).
			Old-style (pre-reform) declensions use ъ instead of (blank), ъ-а
			instead of -а, ъ-ья instead of -ья, and инъ, ёнокъ/онокъ/енокъ,
			ёночекъ/оночекъ/еночекъ instead of the same without terminating ъ.
			The declensions can also be written with an accent on them; this
			chooses the same declension (except for е vs. е́), but causes
			ACCENT to default to pattern b instead of a.

			For adjectival nouns, you should normally supply just + and let
			the ending determine the declension; supply +ь in the case of a
			possessive adjectival noun in -ий, which have an extra -ь- in
			most endings compared with normal adjectival nouns in -ий, but
			which can't be distinguished based on the nominative singular.
			You can also supply +short, +mixed or +proper, which constrains
			the declension appropriately but still autodetects the
			gender-specific and stress-specific variant. If you do supply
			a specific declension type, as with regular nouns you need to
			omit the ending from the lemma field and supply just the stem.
			Possibilities are +ый, +ое, +ая, +ій, +ее, +яя, +ой, +о́е, +а́я,
			+ьій, +ье, +ья, +-short or +#-short (masc), +о-short,
			+о-stressed-short or +о́-short, +а-short, +а-stressed-short or
			+а́-short, and similar for -mixed and -proper (except there aren't
			any stressed mixed declensions).
		DECLTYPE/DECLTYPE is used for nouns with one declension in the
			singular and a different one in the plural, for cases that
			PLVARIANT and special case (1) below don't cover.
		Special-case markers:
			(1) for Zaliznyak-style alternate nominative plural ending:
				-а or -я for masculine, -и or -ы for neuter
			(2) for Zaliznyak-style alternate genitive plural ending:
				-ъ/none for masculine, -ей for feminine, -ов(ъ) for neuter,
				-ей for plural variant -ья
			* for reducibles (nom sg or gen pl has an extra vowel before the
				final consonant as compared with the stem found in other cases)
			;ё for Zaliznyak-style alternation between last е in stem and ё

TODO:

1. Change {{temp|ru-decl-noun-pl}} and {{temp|ru-decl-noun-unc}} to use
   'manual' instead of '*' as the decl class.
   [IMPLEMENTED IN WIKTIONARY.]
2. Find places with '-' as the decl class and remove or change to #.
  [IMPLEMENTED IN WIKTIONARY.]
3. Find places with '*' as the decl class and change to $. There is at
  least one. [IMPLEMENTED IN WIKTIONARY; NEED TO REMOVE * AS ALIAS.
  NEED TO TEST THAT OMITTING A MANUAL FORM LEAVES THE FORM AS A BIG DASH.]
4. Implement skipping entirely an empty first stress argument in master,
  and fix cases that use it, in preparation for switching. [IMPLEMENTED IN
  WIKTIONARY.]
5. Require stem to be specified instead of defaulting to page,
  and fix errors that result. [IMPLEMENTED IN WIKTIONARY. UNDID THIS AND
  IMPLEMENTED THE OPPOSITE, TO ALLOW FOR DEFAULTING IN CASE OF PAGES WITH
  Ё IN THEM AND MONOSYLLABLES.]
6. Change '-' to mean invariable to '$' in ru-adjective.lua. Change
   the stem from empty to "-" and change the endings to be empty. Change any
   templates to use '$'. Test that omitting a manual form leaves the form
   as a big dash. [IMPLEMENTED. DON'T THINK THERE ARE ANY TEMPLATES USING IT.
   NEED TO TEST OMITTING A FORM.]
7. Add proper support for Zaliznyak b', f''. [IMPLEMENTED. NEED TO TEST.]
7a. In Module:table-tools, support + as a footnote along with §¶ªº†‡°№!@#$%^
   and anything in the range U+00A1-U+00BF,U+00D7,U+00F7,U+2010-U+2027,
   U+2030-U+205E,U+2070-U+20CF,U+2100-U+2B5F,U+2E00-U+2E3F [IMPLEMENTED.]
7b. FIXME: Consider putting a triangle △ (U+25B3) or the smaller variant
   ▵ (U+25B5) next to each irregular form.
7c. Mixed and proper-noun adjectives have built-in notes. We need to
   handle those notes with an "internal_notes" section similar to what is used
   in the adjective module. [IMPLEMENTED. NEED TO TEST.]
7d. Adjective detection code here needs to work the same as for the
   adjective module, in particular in the handling of short, stressed-short,
   mixed, proper, stressed-proper. [IMPLEMENTED. NEED TO TEST.]
7e. FIXME: Change calls to ru-adj11 to use the new proper name support in
   ru-adjective.
7f. FIXME: Add words ребёночек, щеночек, сапожок, зубок. Fix decls of
   сяжок (3*b // 3*d(2)), глазок, рожок, знамя.
7g. Change this module to use Zaliznyak-style accent patterns internally
   instead of numbered ones. [IMPLEMENTED.]
7h. Change stress pattern categories to use Zaliznyak-style accent
   patterns. Do this when supporting b' and f'' and changing module
   internally to use Zaliznyak-style accent patterns. [IMPLEMENTED. NEED TO
   TEST.]
7i. [FIXME: Consider removing slash patterns and instead handling them by
   allowing additional declension flags 'sg' and 'pl'. This simplifies the
   various special cases caused by slash declensions. It would also be
   possible to remove the special plural stem, which would get rid of more
   special cases. On the other hand, it makes it more complicated to support
   plural variant -ья with all singular types, and the category code that
   displays things like "Russian nominals with singular -X and plural -Y"
   also gets more complicated, and there's something convenient and intuitive
   about plural stems, and slash declensions are also convenient and at least
   somewhat intuitive. One possibility is to externally allow slash
   declensions and special plural stems and rewrite them internally to
   separate stems with 'sg' and 'pl' declension flags; but there are still
   the two coding issues mentioned above.]
7j. Consider simplifying plural-variant code to only allow -ья as a
   plural variant [and maybe even change that to be something like (1')].
   [IMPLEMENTED REDUCTION OF PLURAL VARIANTS TO -ья; PLURAL-VARIANT CODE
   STILL COMPLEX, THOUGH. NEED TO TEST.]
7k. Remove code that recognizes gender for adjectival nouns; not
   needed. [IMPLEMENTED.]
7l. FIXME: Create categories for use with the category code.
7m. FIXME: Integrate stress categories with those in Vitalik's module.
7n. FIXME: Remove boolean recognize_plurals; this should always be true.
   Do in conjunction with merging multiple-words/manual-translit branches.
7o. Automatically superscript *, numbers and similar things at the
   beginning of a note. Also do this in adjective module. [IMPLEMENTED.
   NEED TO TEST.]
7p. Eliminate мя-1; it's only one noun, and can use slash declension +
   plural stem. [IMPLEMENTED. REPLACED ENTRY FOR знамя AS FOLLOWS:
   {{ru-noun-table|зна́|мя/о||знамён}}]
7q. Consider eliminating о-ья and replacing it with slash declension
   о/-ья like we do for feminine, masculine soft, etc. nouns. [IMPLEMENTED.
   NEED TO TEST.]
7r. Implement check for bare argument specified when neither nominative
   singular nor genitive plural makes use of bare. [IMPLEMENTED. TRACKING
   UNDER "pointless-bare". ELIMINATED MOST ADJECTIVES HERE, A COUPLE LEFT
   ARE IN -Я AND WILL DISAPPEAR AS SOON AS WE PUSH THE NEW MODULE.]
7s. ADJECTIVE MODULE: Add categories for short-adjective accent
   patterns. [IMPLEMENTED.]
7t. Rename "unreduce" to "dereduce". [IMPLEMENTED.]
7u. FIXME: Change stress-pattern detection and overriding to happen inside of
   looping over the two parts of a slash decl. Requires that the loop over
   the two parts happen outside of the loop over stress patterns. Requires
   that the category code get split into two parts, one to handle combined
   singular/plural categories that goes outside the two loops, and one to
   handle everything else that goes inside the two loops.
7v. FIXME: Make the check for multiple stress patterns (categorizing/tracking)
   smarter, to keep a list of them and check at the end, so we handle
   multiple stress patterns specified through different arg sets.
7w. FIXME: override_matches_suffix() had a free variable reference to ARGS
   in it, which should have triggered an error whenever there was a nom_sg or
   nom_pl override but didn't. Is there an error causing this never to be
   called? Check.
7x. With pluralia tantum adjectival nouns, we don't know the gender.
   By default we assume masculine (or feminine for old-style -ія nouns) and
   currently this goes into the category, but shouldn't. [IMPLEMENTED.]
7y. Consider renaming "stem set" to "arg set". I considered "decl spec/
   declension spec" but that proved too obscure. [IMPLEMENTED.]
7z. FIXME: Implement smart code to check properly whether an explicit bare is
   a reducible by looking to see if it's one more syllable than the stem.
   We should probably do this test only when args.reducible not set, otherwise
   assume reducible.
8. [Get error "Unable to dereduce" with strange noun ва́йя, what should
  happen?] [WILL NOT FIX; USE AN OVERRIDE]
9. Implement ins_sg stem for 8* feminine words like люво́вь with reducible
  stem любв- in gen/dat/pre sg and throughout the plural (I think),
  but ins sg любо́вью. [IMPLEMENTED. TESTED. CHANGE NOUNS OF THIS
  SORT TO USE THE NEW WAY. любо́вь, нелюбо́вь, вошь, це́рковь, ложь, рожь.]
10. pltailall/sgtailall. [IMPLEMENTED. PLTAILALL IMPLEMENTED IN WIKTIONARY.]
11. More sophisticated handling of user-requested plural variant vs. special
  case (1) vs. plural-detected variant. [IMPLEMENTED. NEED TO TEST FURTHER.]
12. Dereducing masculine plural. [IMPLEMENTED. TESTED.]
13. Solution to ambiguous plural involving gender spec "3f". [IMPLEMENTED;
   NEED TO TEST.]
14. N*d(2) in masc nouns will have different dereduced form in
   nom sg vs. gen pl, e.g. nom sg рожо́к vs. gen pl ро́жек; essentially, need
   to reduce then dereduce again, and store a separate nom_sg bare and
   gen_pl bare. With saпожо́к, the pl stem is probably сапо́ж-; this implies
   we need to restress an unstressed reduced stem according to the way we
   do it for the normal stem with b/d(')/f(') stress patterns. The nouns
   that appear to work this way are all 3*d(2): зубо́к, сапожо́к (3*d(2) // 3*b),
   рожо́к, сяжо́к (3*b // 3*d(2)), глазо́к. [IMPLEMENTED. TESTED. NEED TO CHANGE
   THE WIKTIONARY NOUNS TO INCORPORATE THESE AND ADD сапожо́к.]
15. Bug in -я nouns with bare specified; gen pl should not have -ь ending. Old
   templates did not add this ending when bare occurred. [IMPLEMENTED IN
   WIKTIONARY. SHOULD REMOVE THE TRACKING CODE.]
16. Remove barepl, make pl= be 5th argument. [IMPLEMENTED IN WIKTIONARY.]
17. [Add accent pattern for ь-stem numbers. Wikitiki handled that through
   overriding the ins_sg. I thought there would be complications with the
   nom_sg in multi-syllabic words but no.] [INSTEAD, DISTINGUISHED b from b',
   f' from f''. CAN USE PLAIN b.]
18. Eliminate complicated defaulting code for second and further arg sets.
   Should simply default to same values as the first arg set does, without
   the first arg set serving as defaults for the remainder, except that
   the stem itself can default to the previous arg set. [IMPLEMENTED IN
   WIKTIONARY.]
19. Fixes for stem-multi-syllabic words with ending stress in gen pl but
   non-syllabic gen pl, with stress transferring onto final syllable even if
   stem is otherwise stressed on an earlier syllable (e.g. голова́ in
   accent pattern f, nom pl го́ловы, gen pl голо́в). Currently these are handled
   by overriding "bare" but I want to make bare predictable mostly, just
   specifying that the noun is reducible should be enough. [IMPLEMENTED
   IN WIKTIONARY. SHOULD REMOVE THE TRACKING CODE.]
20. If stress omitted, it should default to a or b depending on whether accent
   is on stem or ending, not always a. [IMPLEMENTED. TESTED.]
21. Should recognize plural in the auto-detection code when the gender is set.
   [IMPLEMENTED. TESTED.]
22. Issue an error unless allow_unaccented is given (using a * at the beginning
   of the stem). [IMPLEMENTED. FIXED WIKTIONARY ENTRIES.]
23. Make it so that the plural-specifying decl classes -а, -ья, and new -ы, -и
   still auto-detect the class and convert the resulting auto-detected class
   to one with the plural variant. It's useful then to have explicit names for
   the plural-variant classes -а, -ья. I propose #-а, #-ья, which are aliases;
   the canonical name is still -a, -ья so that you can still say something like
   ин/-ья. We should similarly have # has the alias for -.  The classes
   would look like (where * means to construct a slash class)

   Orig        -а          -ья          -ы         -и
   (blank)     -а          -ья          (blank)    (blank)
   ъ           ъ-а         ъ-ья         ъ          ъ
   ь-m         *           *            *          ь-m
   а           *           *            а          а
   я           *           *            *          я
   о           о           *            о-и        о-и
   е           *           *            *          *
   ь-f         *           *            *          ь-f
  [IMPLEMENTED. THEN REMOVED MOST PLURAL VARIANTS, LEAVING ONLY -ья AND
  (1) AND (2). NEED TO TEST -ья, ALTHOUGH PRESUMABLY THEY GOT TESTEED
  THROUGH THE TEST PAGES AND THROUGH BEING RUN ON ALL THE EXISTING DECLED
  RUSSIAN NOUNS IN WIKTIONARY.]
24. Support adjective declensions. Autodetection should happen by putting +
   in decl field to indicate it's an adjective. Adjective decl types should
   begin with a +. (Formerly a * but that is used for reducibles.)]
   [IMPLEMENTED. TESTED.]
24a. Implement autodetection of plural adjective declensions given gender,
   and conversion to singular declension, with n=pl set. [IMPLEMENTED. TESTED.]
25. Implement (1) as an alias for certain irregular plurals, for
    compatibility with Zaliznyak. [IMPLEMENTED. APPEARS TO WORK BUT SHOULD
    CHECK.]
26. Add ability to specify manual translation. [IMPLEMENTED IN GITHUB
   MANUAL-TRANSLIT BRANCH FOR NOUNS, NOT YET FOR ADJECTIVES, NOT TESTED,
   ALMOST CERTAINLY HAS ERRORS]
27. Support multiple words and new ru-decl-noun-multi. [IMPLEMENTED IN
   MULTIPLE-WORDS BRANCH, NOT TESTED.]
28. FIXME: In multiple-words branch, fix ru-decl-noun-multi so it recognizes
   things like *, (1), (2) and ; without the need for a separator. Consider
   using semicolon as a separator, since we already use it to separate ё
   from a previous declension. Maybe use $ or ~ for an invariable word; don't
   use semicolon.
29. FIXME: In multiple-words branch, with normal ru-noun-table, allow -
    as a joiner, now that $ is used for invariable.
30. [Eventually: Even with decl type explicitly given, the full stem with
    ending should be included.] [MAY NEVER IMPLEMENT]

]=]--

local m_utilities = require("Module:utilities")
local ut = require("Module:utils")
local m_links = require("Module:links")
local com = require("Module:ru-common")
local m_ru_adj = require("Module:ru-adjective")
local m_ru_translit = require("Module:ru-translit")
local strutils = require("Module:string utilities")
local m_table_tools = require("Module:table tools")
local m_debug = require("Module:debug")

local export = {}

local lang = require("Module:languages").getByCode("ru")

local u = mw.ustring.char
local rfind = mw.ustring.find
local rsubn = mw.ustring.gsub
local rmatch = mw.ustring.match
local rsplit = mw.text.split
local ulower = mw.ustring.lower
local usub = mw.ustring.sub

local AC = u(0x0301) -- acute =  ́
local CFLEX = u(0x0302) -- circumflex =  ̂

-- version of rsubn() that discards all but the first return value
local function rsub(term, foo, bar)
	local retval = rsubn(term, foo, bar)
	return retval
end

-- version of rsubn() that returns a 2nd argument boolean indicating whether
-- a substitution was made.
local function rsubb(term, foo, bar)
	local retval, nsubs = rsubn(term, foo, bar)
	return retval, nsubs > 0
end

-- version of rfind() that lowercases its string first, for case-insensitive matching
local function rlfind(term, foo)
	return rfind(ulower(term), foo)
end

local function track(page)
	m_debug.track("ru-noun/" .. page)
	return true
end

-- Clone parent's args while also assigning nil to empty strings.
local function clone_args(frame)
	local args = {}
	for pname, param in pairs(frame:getParent().args) do
		if param == "" then args[pname] = nil
		else args[pname] = param
		end
	end
	return args
end

-- Old-style declensions.
local declensions_old = {}
-- New-style declensions; computed automatically from the old-style ones,
-- for the most part.
local declensions = {}
-- Internal notes for old-style declensions. Only currently used for
-- adjectival nouns.
local internal_notes_table_old = {}
-- Same for new-style declensions.
local internal_notes_table = {}
-- Category and type information corresponding to declensions: These may
-- contain the following fields: 'singular', 'plural', 'decl', 'hard', 'g',
-- 'suffix', 'gensg', 'irregpl', 'cant_reduce', 'ignore_reduce', 'stem_suffix'.
--
-- 'singular' is used to construct a category of the form
-- "Russian nominals SINGULAR". If omitted, a category is constructed of the
-- form "Russian nominals ending in -ENDING", where ENDING is the actual
-- nom sg ending shorn of its acute accents; or "Russian nominals ending
-- in suffix -ENDING", if 'suffix' is true. The value of SINGULAR can be
-- one of the following: a single string, a list of strings, or a function,
-- which is passed one argument (the value of ENDING that would be used to
-- auto-initialize the category), and should return a single string or list
-- of strings. Such a category is only constructed if 'gensg' is true.
--
-- 'plural' is analogous but used to construct a category of the form
-- "Russian nominals with PLURAL", and if omitted, a category is constructed
-- of the form "Russian nominals with plural -ENDING", based on the actual
-- nom pl ending shorn of its acute accents. Currently no plural category
-- is actually constructed.
--
-- In addition, a category may normally constructed from the combination of
-- 'singular' and 'plural', appropriately defaulted; e.g. if both are present,
-- the combined category will be "Russian nominals SINGULAR with PLURAL" and
-- if both are missing, the combined category will be
-- "Russian nominals ending in -SGENDING with plural -PLENDING" (or
-- "Russian nominals ending in suffix -SGENDING with plural -PLENDING" if
-- 'suffix' is true). Note that if either singular or plural or both
-- specifies a list, looping will occur over all combinations. Such a
-- category is constructed only if 'irregpl' or 'suffix' is true or if the
-- declension class is a slash class.
--
-- 'decl' is "1st", "2nd", "3rd" or "invariable"; 'hard' is "hard", "soft"
-- or "none"; 'g' is "m", "f", "n" or "none"; these are all traditional
-- declension categories.
--
-- If 'suffix' is true, the declension type includes a long suffix
-- added to the string that itself undergoes reducibility and such, and so
-- reducibility cannot occur in the stem minus the suffix. Categoriess will
-- be created for the suffix.
--
-- In addition to the above categories, additional more specific categories
-- are constructed based on the final letter of the stem, e.g.
-- "Russian velar-stem 1st-declension hard nominals". See calls to
-- com.get_stem_trailing_letter_type(). 'stem_suffix', if present, is added to
-- the end of the stem when get_stem_trailing_letter_type() is called.
-- This is the only place that 'stem_suffix' is used. This is for use with
-- the '-ья' and '-ье' declension types, so that the trailing letter is
-- 'ь' and not whatever precedes it.
--
-- 'enable_categories' is a special hack for testing, which disables all
-- category insertion if false. Delete this as soon as we've verified the
-- working of the category code and created all the necessary categories.
local enable_categories = false
-- Whether to recognize plural stem forms given the gender.
local recognize_plurals = true
-- Category/type info corresponding to old-style declensions; see above.
local declensions_old_cat = {}
-- Category/type info corresponding to new-style declensions. Computed
-- automatically from the old-style ones, for the most part. Same format
-- as the old-style ones.
local declensions_cat = {}
-- Table listing aliases of old-style declension classes.
local declensions_old_aliases = {}
-- Table listing aliases of new-style declension classes; computed
-- automatically from the old-style ones.
local declensions_aliases = {}
local nonsyllabic_suffixes = {}
local sibilant_suffixes = {}
local stress_patterns = {}
-- Set of patterns with ending-stressed genitive plural.
local ending_stressed_gen_pl_patterns = {}
-- Set of patterns with ending-stressed prepositional singular.
local ending_stressed_pre_sg_patterns = {}
-- Set of patterns with ending-stressed dative singular.
local ending_stressed_dat_sg_patterns = {}
-- Set of patterns with all singular forms ending-stressed.
local ending_stressed_sg_patterns = {}
-- Set of patterns with all plural forms ending-stressed.
local ending_stressed_pl_patterns = {}
-- List of all cases, excluding loc/par/voc.
local decl_cases
-- List of all cases, including loc/par/voc.
local cases
-- Type of trailing letter, for tracking purposes
local trailing_letter_type

-- If enabled, compare this module with new version of module to make
-- sure all declensions are the same. Eventually consider removing this;
-- but useful as new code is created.
local test_new_ru_noun_module = false

-- Forward functions

local determine_decl
local handle_forms_and_overrides
local make_table
local detect_adj_type
local detect_stress_pattern
local override_stress_pattern
local determine_stress_variant
local is_reducible
local is_dereducible
local add_bare_suffix
local attach_stressed
local do_stress_pattern
local canonicalize_override

--------------------------------------------------------------------------
--                     Tracking and categorization                      --
--------------------------------------------------------------------------

-- Best guess as to whether a bare value is actually a reducible/dereducible
-- or just a reaccented stem or whatever. FIXME: Maybe we should actually
-- check the number of vowels and see if it is one more or one less.
local function bare_is_reducible(stem, bare)
	if not bare or bare == stem then
		return false
	else
		local ustem = com.make_unstressed(stem)
		local ubare = com.make_unstressed(bare)
		if ustem == ubare or ustem .. "ь" == ubare or ustem .. "ъ" == ubare or ustem .. "й" == ubare then
			return false
		end
	end
	return true
end

-- FIXME! Move below the main code

-- FIXME!! Consider deleting most of this tracking code once we've enabled
-- all the categories. Note that some of the tracking categories aren't
-- completely redundant; e.g. we have tracking pages that combine decl and
-- stress classes, such as "а/a" or "о-и/d'", which are more or less
-- equivalent to stem/gender/stress categories, but we also have the same
-- prefixed by "reducible-stem/" for reducible stems.
local function tracking_code(stress, decl_class, real_decl_class, args)
	assert(decl_class)
	assert(real_decl_class)
	local hint_types = com.get_stem_trailing_letter_type(args.stem)
	if real_decl_class == decl_class then
		real_decl_class = nil
	end
	local function dotrack(prefix)
		track(stress)
		track(decl_class)
		track(decl_class .. "/" .. stress)
		if real_decl_class then
			track(real_decl_class)
			track(real_decl_class .. "/" .. stress)
		end
		for _, hint_type in ipairs(hint_types) do
			track(hint_type)
			track(decl_class .. "/" .. hint_type)
			if real_decl_class then
				track(real_decl_class .. "/" .. hint_type)
			end
		end
		if args.pl ~= args.stem then
			track("irreg-pl")
		end
	end
	dotrack("")
	if bare_is_reducible(args.stem, args.bare) then
		track("reducible-stem")
		dotrack("reducible-stem/")
	end
	if rlfind(args.stem, "и́?н$") and (decl_class == "" or decl_class == "#") then
		track("irregular-in")
	end
	if rlfind(args.stem, "[еёо]́?нок$") and (decl_class == "" or decl_class == "#") then
		track("irregular-onok")
	end
	if args.pltail then
		track("pltail")
	end
	if args.sgtail then
		track("sgtail")
	end
	if args.pltailall then
		track("pltailall")
	end
	if args.sgtailall then
		track("sgtailall")
	end
	if args.alt_gen_pl then
		track("alt-gen-pl")
	end
	for _, case in ipairs(cases) do
		if args[case] then
			track("irreg/" .. case)
			-- questionable use: dotrack("irreg/" .. case .. "/")
		end
	end
end

local gender_to_full = {m="masculine", f="feminine", n="neuter"}

-- Insert the category CAT (a string) into list CATEGORIES. String will
-- have "Russian " prepended and ~ substituted for the part of speech --
-- currently always "nominals".
local function insert_category(categories, cat)
	if enable_categories then
		table.insert(categories, "Russian " .. rsub(cat, "~", "nominals"))
	end
end

-- Insert categories into ARGS.CATEGORIES corresponding to the specified
-- stress and declension classes and to the form of the stem (e.g. velar,
-- sibilant, etc.).
local function categorize(stress, decl_class, args)
	local function cat_to_list(cat)
		if not cat then
			return {}
		elseif type(cat) == "string" then
			return {cat}
		else
			assert(type(cat) == "table")
			return cat
		end
	end

	-- Insert category CAT into the list of categories in ARGS.
	-- CAT may be nil, a single string or a list of strings. We call
	-- insert_category() on each string. The strings will have "Russian "
	-- prepended and "~" replaced with the part of speech (currently always
	-- "nominals").
	local function insert_cat(cat)
		for _, c in ipairs(cat_to_list(cat)) do
			insert_category(args.categories, c)
		end
	end

	-- "Resolve" the category spec CATSPEC into the sort of category spec
	-- accepted by insert_cat(), i.e. nil, a single string or a list of
	-- strings. CATSPEC may be any of these or a function, which takes one
	-- argument (SUFFIX) and returns another CATSPEC.
	local function resolve_cat(catspec, suffix)
		if type(catspec) == "function" then
			return resolve_cat(catspec(suffix), suffix)
		else
			return catspec
		end
	end

	-- Check whether an override for nom_sg or nom_pl still contains the
	-- normal suffix (which should already have accents removed) in at least
	-- one of its entries. If no override then of course we return true.
	local function override_matches_suffix(args, case, suffix)
		assert(suffix == com.remove_accents(suffix))
		local override = canonicalize_override(args, case)
		if not override then
			return true
		end
		for _, x in ipairs(override) do
			local entry, notes = m_table_tools.get_notes(x)
			entry = com.remove_accents(m_links.remove_links(entry))
			if rlfind(entry, suffix .. "$") then
				return true
			end
		end
		return false
	end

	assert(decl_class)
	local decl_cats = args.old and declensions_old_cat or declensions_cat

	local sgdecl, pldecl
	if rfind(decl_class, "/") then
		local indiv_decl_classes = rsplit(decl_class, "/")
		sgdecl, pldecl = indiv_decl_classes[1], indiv_decl_classes[2]
	else
		sgdecl, pldecl = decl_class, decl_class
	end
	local sgdc = decl_cats[sgdecl]
	local pldc = decl_cats[pldecl]
	assert(sgdc)
	assert(pldc)

	local sghint_types = com.get_stem_trailing_letter_type(
		args.stem .. (sgdc.stem_suffix or ""))

	-- insert English version of Zaliznyak stem type
	if sgdc.decl == "invariable" then
		insert_cat("invariable ~")
	else
		local stem_type =
			sgdc.decl == "3rd" and "3rd-declension" or
			ut.contains(sghint_types, "velar") and "velar-stem" or
			ut.contains(sghint_types, "sibilant") and "sibilant-stem" or
			ut.contains(sghint_types, "c") and "ц-stem" or
			ut.contains(sghint_types, "i") and "i-stem" or
			ut.contains(sghint_types, "vowel") and "vowel-stem" or
			ut.contains(sghint_types, "soft-cons") and "vowel-stem" or
			ut.contains(sghint_types, "palatal") and "vowel-stem" or
			sgdc.hard == "soft" and "soft-stem" or
			"hard-stem"
		if sgdc.adj then
			-- Don't include gender for pluralia tantum because it's mostly
			-- indeterminate (certainly when specified using a plural lemma,
			-- which will be usually; technically it's partly or completely
			-- determinate in certain old-style adjectives that distinguish
			-- masculine from feminine/neuter, but this is too rare a case
			-- to worry about)
			local gendertext = args.n == "p" and "plural-only" or gender_to_full[sgdc.g]
			if sgdc.possadj then
				insert_cat(sgdc.decl .. " possessive " .. gendertext .. " adjectival ~")
			elseif stem_type == "soft-stem" or stem_type == "vowel-stem" then
				insert_cat(stem_type .. " " .. gendertext .. " adjectival ~")
			else
				insert_cat(stem_type .. " " .. gendertext .. " accent-" .. stress .. " adjectival ~")
			end
		else
			-- NOTE: There are 8 Zaliznyak-style stem types and 3 genders, but
			-- we don't create a category for masculine-type 3rd-declension
			-- nominals (there is such a noun, путь, but it mostly behaves
			-- like a feminine noun), so there are 23.
			insert_cat(stem_type .. " " .. gender_to_full[sgdc.g] .. "-type ~")
			-- NOTE: Here we are creating categories for the combination of
			-- stem, gender and accent. There are 10 accent patterns and 23
			-- combinations of stem and gender, which potentially makes for
			-- 10*23 = 230 such categories, which is a lot. Not all such
			-- categories should actually exist; there were maybe 75 former
			-- declension templates, each of which was essentially categorized
			-- by the same three variables, but some of which dealt with
			-- ancillary issues like irregular plurals; this amounts to 67
			-- actual stem/gender/accent categories, although there are more
			-- of them in Zaliznyak (FIXME, how many? See generate_cats.py).
			insert_cat(stem_type .. " " .. gender_to_full[sgdc.g] .. "-type accent-" .. stress .. " ~")
		end
		insert_cat("~ with accent pattern " .. stress)
	end
	local sgsuffix = args.suffixes.nom_sg
	if sgsuffix then
		assert(#sgsuffix == 1) -- If this ever fails, then implement a loop
		sgsuffix = com.remove_accents(sgsuffix[1])
		-- If we are a plurale tantum or if nom_sg is overridden and has
		-- an unusual suffix, then don't create category for sg suffix
		if args.n == "p" or not override_matches_suffix(args, "nom_sg", sgsuffix) then
			sgsuffix = nil
		end
	end
	local plsuffix = args.suffixes.nom_pl
	if plsuffix then
		assert(#plsuffix == 1) -- If this ever fails, then implement a loop
		plsuffix = com.remove_accents(plsuffix[1])
		-- If we are a singulare tantum or if nom_pl is overridden and has
		-- an unusual suffix, then don't create category for pl suffix
		if args.n == "s" or not override_matches_suffix(args, "nom_pl", plsuffix) then
			plsuffix = nil
		end
	end
	local sgcat = sgsuffix and (resolve_cat(sgdc.singular, sgsuffix) or "ending in " .. (sgdc.suffix and "suffix " or "") .. "-" .. sgsuffix)
	local plcat = plsuffix and (resolve_cat(pldc.plural, suffix) or "plural -" .. plsuffix)
	if sgcat and sgdc.gensg then
		for _, cat in ipairs(cat_to_list(sgcat)) do
			insert_cat("~ " .. cat)
		end
	end
	if sgcat and plcat and (sgdc.suffix or sgdc.irregpl or
			rfind(decl_class, "/")) then
		for _, scat in ipairs(cat_to_list(sgcat)) do
			for _, pcat in ipairs(cat_to_list(plcat)) do
				insert_cat("~ " .. scat .. " with " .. pcat)
			end
		end
	end

	if args.pl ~= args.stem then
		insert_cat("~ with irregular plural")
	end
	if bare_is_reducible(args.stem, args.bare) then
		insert_cat("~ with reducible stem")
	end
	if args.alt_gen_pl then
		insert_cat("~ with alternate genitive plural")
	end
	if sgdc.adj then
		insert_cat("adjectival ~")
	end
	for _, case in ipairs(cases) do
		if args[case] then
			local engcase = rsub(case, "^([a-z]*)", {
				nom="nominative", gen="genitive", dat="dative",
				acc="accusative", ins="instrumental", pre="prepositional",
				par="partitive", loc="locative", voc="vocative"
			})
			engcase = rsub(engcase, "(_[a-z]*)$", {
				_sg=" singular", _pl=" plural"
			})
			if case == "loc" or case == "voc" or case == "par" then
				insert_cat("~ with " .. engcase)
			elseif not args.manual then
				insert_cat("~ with irregular " .. engcase)
			end
		end
	end
end

--------------------------------------------------------------------------
--                              Main code                               --
--------------------------------------------------------------------------

local numbered_to_zaliznyak_stress_pattern = {
	["1"] = "a",
	["2"] = "b",
	["3"] = "c",
	["4"] = "d",
	["4*"] = "d'",
	["5"] = "e",
	["6"] = "f",
	["6*"] = "f'",
}

local function arg1_is_stress(arg1)
	if not arg1 then return false end
	for _, arg in ipairs(rsplit(arg1, ",")) do
		if not (rfind(arg, "^[a-f]'?'?$") or rfind(arg, "^[1-6]%*?$")) then
			return false
		end
	end
	return true
end

function export.do_generate_forms(args, old)
	local SUBPAGENAME = mw.title.getCurrentTitle().subpageText
	old = old or args.old
	args.old = old
	args.suffix = args.suffix or ""

	-- Gather arguments into an array of ARG_SET objects, containing
	-- (potentially) elements 1, 2, 3, 4, 5, corresponding to accent pattern,
	-- stem, declension type, bare stem, pl stem and coming from consecutive
	-- numbered parameters. Sets of declension parameters are separated by the
	-- word "or".
	local arg_sets = {}
	-- Find maximum-numbered arg, allowing for holes
	local max_arg = 0
	for k, v in pairs(args) do
		if type(k) == "number" and k > max_arg then
			max_arg = k
		end
	end
	-- Now gather the arguments.
	local offset = 0
	local arg_set = {}
	for i=1,(max_arg + 1) do
		if args[i] == "or" or i == max_arg + 1 then
			local setnum = #arg_sets + 1
			table.insert(arg_sets, arg_set)
			arg_set = {}
			offset = i
		else
			-- If the first argument isn't stress, that means all arguments
			-- have been shifted to the left one. We want to shift them
			-- back to the right one, so we change the offset so that we
			-- get the same effect of skipping a slot in the arg set.
			if i - offset == 1 and not arg1_is_stress(args[i]) then
				offset = offset - 1
			end
			if i - offset > 5 then
				error("Too many arguments for argument set: arg " .. i .. " = " .. (args[i] or "(blank)"))
			end
			arg_set[i - offset] = args[i]
		end
	end

	-- Initialize non-stem-specific arguments.
	args.a = args.a and string.sub(args.a, 1, 1) or "i"
	args.n = args.n and string.sub(args.n, 1, 1)
	args.forms = {}
	args.categories = {}
	local function insert_cat(cat)
		insert_category(args.categories, cat)
	end
	args.internal_notes = {}
	-- Superscript footnote marker at beginning of note, similarly to what's
	-- done at end of forms.
	if args.notes then
		local notes, entry = m_table_tools.get_initial_notes(args.notes)
		args.notes = notes .. entry
	end

	local decls = old and declensions_old or declensions
	local decl_cats = old and declensions_old_cat or declensions_cat
	local intable = old and internal_notes_table_old or internal_notes_table

	if #arg_sets > 1 then
		track("multiple-stems")
		insert_cat("~ with multiple stems")
	end

	-- Default lemma defaults to previous lemma, first one to page name.
	local default_lemma = SUBPAGENAME

	for _, arg_set in ipairs(arg_sets) do
		local stress_arg = arg_set[1]
		local decl_class = arg_set[3] or ""
		local bare = arg_set[4]
		local pl = arg_set[5]
		if decl_class == "manual" then
			decl_class = "$"
			args.manual = true
			if #arg_sets > 1 then
				error("Can't specify multiple argument sets when manual")
			end
			if bare or pl then
				error("Can't specify optional stem parameters when manual")
			end
		end
		decl_class, args.jo_special = rsubb(decl_class, "([^/%a])ё$", "%1")
		if not args.jo_special then
			decl_class, args.jo_special = rsubb(decl_class, "([^/%a])ё([^/%a])", "%1%2")
		end
		decl_class, args.want_sc1 = rsubb(decl_class, "%(1%)", "")
		decl_class, args.alt_gen_pl = rsubb(decl_class, "%(2%)", "")
		decl_class, args.reducible = rsubb(decl_class, "%*", "")
		decl_class = rsub(decl_class, ";", "")
		local lemma = args.manual and "-" or arg_set[2] or default_lemma
		if not lemma then
			error("Lemma in first argument set must be specified")
		end
		default_lemma = lemma
		local stem, was_accented, was_plural, was_autodetected
		if rfind(decl_class, "^%+") then
			stem, decl_class, was_accented, was_plural, was_autodetected =
				detect_adj_type(lemma, decl_class, old)
		else
			stem, decl_class, was_accented, was_plural, was_autodetected =
				determine_decl(lemma, decl_class, args)
		end
		if was_plural then
			args.n = args.n or "p"
		end
		stem, args.allow_unaccented = rsubb(stem, "^%*", "")
		if not args.allow_unaccented and not stress_arg and was_autodetected and com.needs_accents(lemma) then
			-- If user gave the full word and expects us to determine the
			-- declension and stress, the word should have an accent on the
			-- stem or ending. We have a separate check farther below for
			-- an accent on a multisyllabic stem, after stripping off any
			-- ending; but this way we get an error if the user e.g. writes
			-- "гора" without an accent rather than assuming it's stem
			-- stressed, as would otherwise happen.
			error("Lemma must have an accent in it: " .. lemma)
		end
		if not stress_arg then
			stress_arg = {detect_stress_pattern(stem, decl_class, decl_cats, args.reducible, was_plural, was_accented)}
		else
			-- validate/canonicalize stress arg, override in certain cases
			-- and convert to list
			stress_arg = rsplit(stress_arg, ",")
			for i=1,#stress_arg do
				local stress = stress_arg[i]
				stress = override_stress_pattern(decl_class, stress)
				stress = numbered_to_zaliznyak_stress_pattern[stress] or stress
				if not stress_patterns[stress] then
					error("Unrecognized accent pattern " .. stress)
				end
				stress_arg[i] = stress
			end
		end

		-- convert decl type to list
		local sub_decl_classes
		if rfind(decl_class, "/") then
			track("mixed-decl")
			insert_cat("~ with mixed declension")
			local indiv_decl_classes = rsplit(decl_class, "/")
			-- Should have been caught in canonicalize_decl()
			assert(#indiv_decl_classes == 2)
			sub_decl_classes = {{indiv_decl_classes[1], "sg"}, {indiv_decl_classes[2], "pl"}}
		else
			sub_decl_classes = {{decl_class}}
		end

		if #stress_arg > 1 then
			track("multiple-accent-patterns")
			insert_cat("~ with multiple accent patterns")
		end

		local original_stem = stem
		local original_bare = bare
		local original_pl = pl

		-- Loop over accent patterns in case more than one given.
		for _, stress in ipairs(stress_arg) do
			args.suffixes = {}

			stem = original_stem
			bare = original_bare
			local stem_for_bare
			pl = original_pl

			local stem_was_unstressed = com.is_unstressed(stem)

			if stem_was_unstressed and args.jo_special then
				stem = rsub(stem, "([еЕ])([^еЕ]*)$",
					function(e, rest)
						return (e == "Е" and "Ё" or "ё") .. rest
					end
				)
				stem_for_bare = stem
			end

			local function restress_stem(stem, stress, stem_unstressed)
				-- If the user has indicated they purposely are leaving the
				-- word unstressed by putting a * at the beginning of the main
				-- stem, leave it unstressed. This might indicate lack of
				-- knowledge of the stress or a truly unaccented word
				-- (e.g. an unaccented suffix).
				if args.allow_unaccented then
					return stem
				end
				-- it's safe to accent monosyllabic stems
				if com.is_monosyllabic(stem) then
					stem = com.make_ending_stressed(stem)
				-- For those patterns that are ending-stressed in the singular
				-- nominative (and hence are likely to be expressed without an
				-- accent on the stem) it's safe to put a particular accent on
				-- the stem depending on the stress type. Otherwise, give an
				-- error if no accent.
				elseif stem_unstressed then
					if rfind(stress, "^f") then
						stem = com.make_beginning_stressed(stem)
					elseif (rfind(stress, "^[bd]") or
						args.n == "p" and ending_stressed_pl_patterns[stress]) then
						stem = com.make_ending_stressed(stem)
					elseif com.needs_accents(stem) then
						error("Stem " .. stem .. " requires an accent")
					end
				end
				return stem
			end

			stem = restress_stem(stem, stress, stem_was_unstressed)

			-- Leave pl unaccented if user wants this; see restress_stem().
			if pl and not args.allow_unaccented then
				if com.is_monosyllabic(pl) then
					pl = com.make_ending_stressed(pl)
				end
				-- I think this is safe.
				if com.needs_accents(pl) then
					if ending_stressed_pl_patterns[stress] then
						pl = com.make_ending_stressed(pl)
					elseif not args.allow_unaccented then
						error("Plural stem " .. pl .. " requires an accent")
					end
				end
			end

			local sgdecl = sub_decl_classes[1][1]
			local sgdc = decl_cats[sgdecl]
			local resolved_bare = bare
			-- Handle (de)reducibles
			-- FIXME! We are dereducing based on the singular declension.
			-- In a slash declension things can get weird and we don't
			-- handle that. We are also computing the bare value from the
			-- singular stem, and again things can get weird with a plural
			-- stem. Note that we don't compute a bare value unless we have
			-- to (either (de)reducible or stress pattern f/f'/f'' combined
			-- with ё special case); the remaining times we generate the bare
			-- value directly from the plural stem.
			if bare then
				-- FIXME: Tracking code eventually to remove; track cases
				-- where bare is explicitly specified to see how many could
				-- be predicted
				if stem == bare then
					track("explicit-bare-same-as-stem")
				elseif com.make_unstressed(stem) == com.make_unstressed(bare) then
					track("explicit-bare-different-stress")
					track("explicit-bare-different-stress-from-stem")
				elseif rfind(decl_class, "^ь%-") and (stem .. "ь") == bare then
					track("explicit-bare-same-as-nom-sg")
				elseif rfind(decl_class, "^ь%-") and com.make_unstressed(stem .. "ь") == com.make_unstressed(bare) then
					track("explicit-bare-different-stress")
					track("explicit-bare-different-stress-from-nom-sg")
				else
					if is_reducible(sgdc) then
						local autostem = export.reduce_nom_sg_stem(bare, sgdecl)
						if not autostem then
							track("error-reducible")
						elseif autostem == stem then
							track("predictable-reducible")
						elseif com.make_unstressed(autostem) == com.make_unstressed(stem) then
							track("predictable-reducible-but-for-stress")
						else
							track("unpredictable-reducible")
						end
					elseif is_dereducible(sgdc) then
						local autobare = export.dereduce_nom_sg_stem(stem, sgdc, stress, old)
						if not autobare then
							track("error-dereducible")
						elseif autobare == bare then
							track("predictable-dereducible")
						elseif com.make_unstressed(autobare) == com.make_unstressed(bare) then
							track("predictable-dereducible-but-for-stress")
						else
							track("unpredictable-dereducible")
						end
					else
						track("bare-without-reducibility")
					end
				end
			elseif args.reducible and not sgdc.ignore_reduce then
				-- Zaliznyak treats all nouns in -ье and -ья as being
				-- reducible. We handle this automatically and don't require
				-- the user to specify this, but ignore it if so for
				-- compatibility.
				if is_reducible(sgdc) then
					-- If we derived the stem from a nom pl form, then
					-- it's already reduced, and we need to dereduce it to
					-- get a bare form; otherwise the stem comes from the
					-- nom sg and we need to reduce it to get the real stem.
					if was_plural then
						resolved_bare = export.dereduce_nom_sg_stem(stem, sgdc,
							stress, old, "error")
					else
						resolved_bare = stem
						stem = export.reduce_nom_sg_stem(stem, sgdecl, "error")
						-- Stem will be unstressed if stress was on elided
						-- vowel; restress stem the way we did above. (This is
						-- needed in at least one word, сапожо́к 3*d(2), with
						-- plural stem probably сапо́жк- and gen pl probably
						-- сапо́жек.)
						stem = restress_stem(stem, stress, com.is_unstressed(stem))
						if stress ~= "a" and stress ~= "b" and args.alt_gen_pl and not pl then
							-- Nouns like рожо́к, глазо́к of type 3*d(2) have
							-- gen pl's ро́жек, гла́зок; to handle this,
							-- dereduce the reduced stem and store in a
							-- special place.
							args.gen_pl_bare = export.dereduce_nom_sg_stem(stem,
								sgdc, stress, old, "error")
						end
					end
				elseif is_dereducible(sgdc) then
					resolved_bare = export.dereduce_nom_sg_stem(stem, sgdc,
						stress, old, "error")
				else
					error("Declension class " .. sgdecl .. " not (de)reducible")
				end
			elseif stem_for_bare and stem ~= stem_for_bare then
				resolved_bare = add_bare_suffix(stem_for_bare, old, sgdc, false)
			end

			-- Leave unaccented if user wants this; see restress_stem().
			if resolved_bare and not args.allow_unaccented then
				if com.is_monosyllabic(resolved_bare) then
					resolved_bare = com.make_ending_stressed(resolved_bare)
				elseif com.needs_accents(resolved_bare) then
					error("Resolved bare stem " .. resolved_bare .. " requires an accent")
				end
			end

			args.stem = stem
			args.bare = resolved_bare
			args.ustem = com.make_unstressed_once(stem)
			args.pl = pl or stem
			args.upl = com.make_unstressed_once(args.pl)
			-- Special hack for любо́вь and other reducible 3rd-fem nouns,
			-- which have the full stem in the ins sg
			args.ins_sg_stem = sgdecl == "ь-f" and args.reducible and resolved_bare

			-- Loop over declension classes (we may have two of them, one for
			-- singular and one for plural, in the case of a mixed declension
			-- class of the form SGDECL/PLDECL).
			for _,decl_class_spec in ipairs(sub_decl_classes) do
				local orig_decl_class = decl_class_spec[1]
				local number = decl_class_spec[2]
				local real_decl_class =
					determine_stress_variant(orig_decl_class, stress)
				-- sanity checking; errors should have been caught in
				-- canonicalize_decl()
				assert(decl_cats[real_decl_class])
				assert(decls[real_decl_class])
				tracking_code(stress, orig_decl_class, real_decl_class, args)
				do_stress_pattern(stress, args, decls[real_decl_class], number)

				-- handle internal notes
				local internal_note = intable[real_decl_class]
				if internal_note then
					ut.insert_if_not(args.internal_notes, internal_note)
				end
			end

			-- check for pointless bare (4th argument)
			local function is_nonsyllabic(suff)
				return suff and #suff == 1 and nonsyllabic_suffixes[suff[1]]
			end
			if bare and not is_nonsyllabic(args.suffixes.nom_sg) and not is_nonsyllabic(args.suffixes.gen_pl) then
				track("pointless-bare")
				track("pointless-bare/" .. decl_class)
			end

			categorize(stress, decl_class, args)
		end
	end

	handle_forms_and_overrides(args)

	-- Test code to compare existing module to new one.
	if test_new_ru_noun_module then
		local m_new_ru_noun = require("Module:User:Benwing2/ru-noun")
		local newargs = clone_args(frame)
		newargs = m_new_ru_noun.do_generate_forms(newargs, old)
		for _, case in ipairs(cases) do
			local is_pl = rfind(case, "_pl")
			if args.n == "s" and is_pl or args.n == "p" and not is_pl then
				-- Don't need to check cases that won't be displayed.
			elseif not ut.equals(args[case], newargs[case]) then
				local monosyl_accent_diff = false
				-- Differences only in monosyllabic accents. Enable if we
				-- change the algorithm for these.
				--if args[case] and newargs[case] and #args[case] == 1 and #newargs[case] == 1 then
				--	local val1 = args[case][1]
				--	local val2 = newargs[case][1]
				--	if com.is_monosyllabic(val1) and com.is_monosyllabic(val2) and com.remove_accents(val1) == com.remove_accents(val2) then
				--		monosyl_accent_diff = true
				--	end
				--end
				if monosyl_accent_diff then
					track("monosyl-accent-diff")
				else
					-- Uncomment this to display the particular case and
					-- differing forms.
					--error(case .. " " .. (args[case] and table.concat(args[case], ",") or "nil") .. " " .. (newargs[case] and table.concat(newargs[case], ",") or "nil"))
					track("different-decl")
				end
				break
			end
		end
	end

	return args
end

-- Implementation of main entry point
local function do_show(frame, old)
	local args = clone_args(frame)
	local args = export.do_generate_forms(args, old)
	return make_table(args) .. m_utilities.format_categories(args.categories, lang)
end

-- The main entry point for modern declension tables.
function export.show(frame)
	return do_show(frame, false)
end

-- The main entry point for old declension tables.
function export.show_old(frame)
	return do_show(frame, true)
end

local function get_form(forms)
	local canon_forms = {}
	for _, form in forms do
		local entry, notes = m_table_tools.get_notes(form)
		ut.insert_if_not(canon_forms, m_links.remove_links(entry))
	end
	return table.concat(canon_forms, ",")
end

-- The entry point for 'ru-noun-forms' to generate all noun forms.
function export.generate_forms(frame)
	local args = clone_args(frame)
	local args = export.do_generate_forms(args, false)
	local ins_text = {}
	for _, case in ipairs(cases) do
		if args.forms[case] then
			table.insert(ins_text, case .. "=" .. get_form(args.forms[case]))
		end
	end
	return table.concat(ins_text, "|")
end

-- The entry point for 'ru-noun-form' to generate a particular noun form.
function export.generate_form(frame)
	local args = clone_args(frame)
	if not args.form then
		error("Must specify desired form using form=")
	end
	local form = args.form
	if not ut.contains(old_cases, form) then
		error("Unrecognized form " .. form)
	end
	local args = export.do_generate_forms(args, false)
	if not args.forms[form] then
		return ""
	else
		return get_form(args.forms[form])
	end
end

local stem_expl = {
	["velar-stem"] = "a velar (-к, -г or –x)",
	["sibilant-stem"] = "a sibilant (-ш, -ж, -ч or -щ)",
	["ц-stem"] = "-ц",
	["i-stem"] = "-и (old-style -і)",
	["vowel-stem"] = "a vowel other than -и or -і, or -й or -ь",
	["soft-stem"] = "a soft consonant",
	["hard-stem"] = "a hard consonant",
}

local zaliznyak_stem_type = {
	["velar-stem"] = "3",
	["sibilant-stem"] = "4",
	["ц-stem"] = "5",
	["i-stem"] = "7",
	["vowel-stem"] = "6",
	["soft-stem"] = "2",
	["hard-stem"] = "1",
	["3rd-declension"] = "8",
}

local stem_gender_endings = {
    masculine = {
		["hard-stem"]      = {"a hard consonant (-ъ old-style)", "-ы"},
		["ц-stem"]         = {"-ц (-цъ old-style)", "-ы"},
		["velar-stem"]     = {"a velar (plus -ъ old-style)", "-и"},
		["sibilant-stem"]  = {"a sibilant (plus -ъ old-style)", "-и"},
		["soft-stem"]      = {"-ь", "-и"},
		["i-stem"]         = {"-й", "-и"},
		["vowel-stem"]     = {"-й", "-и"},
		["3rd-declension"] = {"-ь", "-и"},
	},
    feminine = {
		["hard-stem"]      = {"-а", "-ы"},
		["ц-stem"]         = {"-а", "-ы"},
		["velar-stem"]     = {"-а", "-и"},
		["sibilant-stem"]  = {"-а", "-и"},
		["soft-stem"]      = {"-я", "-и"},
		["i-stem"]         = {"-я", "-и"},
		["vowel-stem"]     = {"-я", "-и"},
		["3rd-declension"] = {"-ь", "-и"},
	},
    neuter = {
		["hard-stem"]      = {"-о", "-а"},
		["ц-stem"]         = {"-е", "-а"},
		["velar-stem"]     = {"-о", "-а"},
		["sibilant-stem"]  = {"-е", "-а"},
		["soft-stem"]      = {"-е", "-я"},
		["i-stem"]         = {"-е", "-я"},
		["vowel-stem"]     = {"-е", "-я"},
		["3rd-declension"] = {"-мя", "-мена"},
	},
}

-- Implementation of template 'runouncatboiler'.
function export.catboiler(frame)
	local SUBPAGENAME = mw.title.getCurrentTitle().subpageText
	local args = clone_args(frame)

	local cats = {}
	insert_category(cats, "~")

	local function get_stem_gender_text(stem, gender)
		if not stem_gender_endings[gender] then
			error("Invalid gender " .. gender)
		end
		local endings = stem_gender_endings[gender][stem]
		if not endings then
			error("Invalid stem type " .. stem)
		end
		local sgending, plending = endings[1], endings[2]
		local stemtext =
			stem == "3rd-declension" and "" or
			" The stem ends in " .. stem_expl[stem] .. " and is Zaliznyak's type " .. zaliznyak_stem_type[stem] .. "."
		local decltext =
			stem == "3rd-declension" and "" or
			" This is traditionally considered to belong to the " .. (gender == "feminine" and "1st" or "2nd") .. " declension."
		return stem .. ", usually " .. gender .. " ~, normally ending in nominative singular " .. sgending .. " and nominative plural " .. plending .. "." .. stemtext .. decltext
	end

	local maintext
	if args[1] == "stemgenderstress" then
		local stem, gender, stress = rmatch(SUBPAGENAME, "^Russian (.-) (.-)%-type accent%-(.-) ")
		if not stem then
			error("Invalid category name, should be e.g. \"Russian velar-stem masculine-type accent-a nominals\"")
		end
		local stem_gender_text = get_stem_gender_text(stem, gender)
		local accent_text = " This nominal is stressed according to accent pattern " .. stress .. "."
		maintext = stem_gender_text .. accent_text
		insert_category(cats, "~ by stem type, gender and accent pattern")
	elseif args[1] == "stemgender" then
		if rfind(SUBPAGENAME, "invariable") then
			maintext = "invariable (indeclinable) ~, which normally have the same form for all cases and numbers."
		else
			local stem, gender = rmatch(SUBPAGENAME, "^Russian (.-) (.-)%-type")
			if not stem then
				error("Invalid category name, should be e.g. \"Russian velar-stem masculine-type nominals\"")
			end
			maintext = get_stem_gender_text(stem, gender)
		end
		insert_category(cats, "~ by stem type and gender")
	elseif args[1] == "adj" then
		local stem, gender, stress = rmatch(SUBPAGENAME, "^Russian (.*) (.-) accent%-(.-) adjectival")
		if not stem then
			stem, gender = rmatch(SUBPAGENAME, "^Russian (.*) (.-) adjectival")
		end
		if not stem then
			error("Invalid category name, should be e.g. \"Russian velar-stem masculine accent-a adjectival nominals\"")
		end
		local stemtext
		if rfind(stem, "possessive") then
			possessive = "possessive "
			stem = rsub(stem, " possessive", "")
			stemtext = ""
		else
			if not stem_expl[stem] then
				error("Invalid stem type " .. stem)
			end
			possessive = ""
			stemtext = " The stem ends in " .. stem_expl[stem] .. " and is Zaliznyak's type " .. zaliznyak_stem_type[stem] .. "."
		end
		local stresstext = stress == "a" and
			"This nominal is stressed according to accent pattern a (stress on the stem)." or
			stress == "b" and
			"This nominal is stressed according to accent pattern b (stress on the ending)." or
			"All nominals of this class are stressed according to accent pattern a (stress on the stem)."
		maintext = stem .. " " .. gender .. " ~, with " .. possessive .. "adjectival endings, ending in nominative singular " .. args[2] .. " and nominative plural " .. args[3] .. "." .. stemtext .. " " .. stresstext
		insert_category(cats, "~ by stem type, gender and accent pattern")
	elseif args[1] == "sg" then
		maintext = "~ ending in nominative singular " .. args[2] .. "."
		insert_category(cats, "~ by singular ending")
	elseif args[1] == "pl" then
		maintext = "~ ending in nominative plural " .. args[2] .. "."
		insert_category(cats, "~ by plural ending")
	elseif args[1] == "sgpl" then
		maintext = "~ ending in nominative singular " .. args[2] .. " and nominative plural " .. args[3] .. "."
		insert_category(cats, "~ by singular and plural ending")
	elseif args[1] == "stress" then
		maintext = "~ with accent pattern " .. args[2] .. "."
		insert_category(cats, "~ by accent pattern")
	elseif args[1] == "extracase" then
		maintext = "~ with a separate " .. args[2] .. " singular case."
		insert_category(cats, "~ by case form")
	elseif args[1] == "irregcase" then
		maintext = "~ with an irregular " .. args[2] .. " case."
		insert_category(cats, "~ by case form")
	else
		maintext = "~ " .. args[1]
	end

	return "This category contains Russian " .. rsub(maintext, "~", "nominals")
		.. "\n" ..
		mw.getCurrentFrame():expandTemplate{title="ru-categoryTOC", args={}}
		.. m_utilities.format_categories(cats, lang)
end

--------------------------------------------------------------------------
--                   Autodetection and lemma munging                    --
--------------------------------------------------------------------------

-- Attempt to detect the type of the lemma based on its ending, separating
-- off the stem and the ending. GENDER must be present with -ь and plural
-- stems, and is otherwise ignored. Return up to three values: The stem
-- (lemma minus ending), the singular lemma ending, and if the lemma was
-- plural, the plural lemma ending. If the lemma was singular, the singular
-- lemma ending will contain any user-given accents; likewise, if the
-- lemma was plural, the plural ending will contain such accents.
local function detect_lemma_type(lemma, gender, anim)
	local base, ending = rmatch(lemma, "^(.*)([еЕ]́)$") -- accented
	if base then
		return base, ulower(ending)
	end
	base = rmatch(lemma, "^(.*[" .. com.sib_c .. "])[еЕ]$") -- unaccented
	if base then
		return base, "о"
	end
	base, ending = rmatch(lemma, "^(.*)([ёоЁО]́?[нН][оО][кК][ъЪ]?)$")
	if base then
		return base, ulower(ending)
	end
	base, ending = rmatch(lemma, "^(.*)([ёоЁО]́?[нН][оО][чЧ][еЕ][кК][ъЪ]?)$")
	if base then
		return base, ulower(ending)
	end
	base, ending = rmatch(lemma, "^(.*[аяАЯ]́?[нН])([иИ]́?[нН][ъЪ]?)$")
	-- Need to check the animacy to avoid nouns like маиганин, цианин,
	-- меланин, соланин, etc.
	if base and anim == "a" then
		return base, ulower(ending)
	end
	base, ending = rmatch(lemma, "^(.*)([мМ][яЯ]́?)$")
	if base then
		return base, ending
	end
	--recognize plural endings
	if recognize_plurals then
		if gender == "n" then
			base, ending = rmatch(lemma, "^(.*)([ьЬ][яЯ]́?)$")
			if base then
				-- Don't do this; о/-ья is too rare
				-- error("Ambiguous plural lemma " .. lemma .. " in -ья, singular could be -о or -ье/-ьё; specify the singular")
				return base, "ье", ending
			end
			base, ending = rmatch(lemma, "^(.*)([аяАЯ]́?)$")
			if base then
				return base, rfind(ending, "[аА]") and "о" or "е", ending
			end
			base, ending = rmatch(lemma, "^(.*)([ыиЫИ]́?)$")
			if base then
				if rfind(ending, "[ыЫ]") or rfind(base, "[" .. com.sib .. com.velar .. "]$") then
					return base, "о-и", ending
				else
					-- FIXME, should we return a slash declension?
					error("No neuter declension е-и available; use a slash declension")
				end
			end
		end
		if gender == "f" then
			base, ending = rmatch(lemma, "^(.*)([ьЬ][иИ]́?)$")
			if base then
				return base, "ья", ending
			end
		end
		if gender == "m" then
			base, ending = rmatch(lemma, "^(.*)([ьЬ][яЯ]́?)$")
			if base then
				return base, "-ья", ending
			end
			base, ending = rmatch(lemma, "^(.*)([аА]́?)$")
			if base then
				return base, "-а", ending
			end
			base, ending = rmatch(lemma, "^(.*)([яЯ]́?)$")
			if base then
				return base, "ь-я", ending
			end
		end
		if gender == "m" or gender == "f" then
			base, ending = rmatch(lemma, "^(.*[" .. com.sib .. com.velar .. "])([иИ]́?)$")
			if not base then
				base, ending = rmatch(lemma, "^(.*)([ыЫ]́?)$")
			end
			if base then
				return base, gender == "m" and "" or "а", ending
			end
			base, ending = rmatch(lemma, "^(.*[" .. com.vowel .. "]́?)([иИ]́?)$")
			if base then
				return base, gender == "m" and "й" or "я", ending
			end
			base, ending = rmatch(lemma, "^(.*)([иИ]́?)$")
			if base then
				return base, gender == "m" and "ь-m" or "я", ending
			end
		end
		if gender == "3f" then
			base, ending = rmatch(lemma, "^(.*)([иИ]́?)$")
			if base then
				return base, "ь-f", ending
			end
		end
	end
	base, ending = rmatch(lemma, "^(.*)([ьЬ][яеёЯЕЁ]́?)$")
	if base then
		return base, ulower(ending)
	end
	base, ending = rmatch(lemma, "^(.*)([йаяеоёъЙАЯЕОЁЪ]́?)$")
	if base then
		return base, ulower(ending)
	end
	base = rmatch(lemma, "^(.*)[ьЬ]$")
	if base then
		if gender == "m" or gender == "f" then
			return base, "ь-" .. gender
		elseif gender == "3f" then
			return base, "ь-f"
		else
			error("Need to specify gender m or f with lemma in -ь: ".. lemma)
		end
	end
	if rfind(lemma, "[уыэюиіѣѵУЫЭЮИІѢѴ]́?$") then
		error("Don't know how to decline lemma ending in this type of vowel: " .. lemma)
	end
	return lemma, ""
end

local plural_variation_detection_map = {
	[""] = {["-ья"]="-ья"},
	["ъ"] = {["-ья"]="ъ-ья"},
}

local special_case_1_to_plural_variation = {
	[""] = "-а",
	["ъ"] = "ъ-а",
	["й"] = "й-я",
	["ь-m"] = "ь-я",
	["о"] = "о-и",
}

local function map_decl(decl, fun)
	if rfind(decl, "/") then
		local split_decl = rsplit(decl, "/")
		if #split_decl ~= 2 then
			error("Mixed declensional class " .. decl
				.. "needs exactly two classes, singular and plural")
		end
		return fun(split_decl[1]) .. "/" .. fun(split_decl[2])
	else
		return fun(decl)
	end
end

-- Canonicalize decl class into non-accented and alias-resolved form;
-- but note that some canonical decl class names with an accent in them
-- (e.g. е́, not the same as е, whose accented version is ё; and various
-- adjective declensions).
local function canonicalize_decl(decl, old)
	-- FIXME: For compatibility; remove it after changing uses of о-ья
	if com.remove_accents(decl) == "о-ья" then
		decl = "о/-ья"
	end
	local function do_canon(decl)
		-- remove accents, but not from е́ (for adj decls, accent matters
		-- as well but we handle that by mapping the accent to a stress pattern
		-- and then to the accented version in determine_stress_variant())
		if decl ~= "е́" then
			decl = com.remove_accents(decl)
		end

		local decl_aliases = old and declensions_old_aliases or declensions_aliases
		local decl_cats = old and declensions_old_cat or declensions_cat
		if decl_aliases[decl] then
			-- If we find an alias, map it.
			decl = decl_aliases[decl]
		elseif not decl_cats[decl] then
			error("Unrecognized declension class " .. decl)
		end
		return decl
	end
	return map_decl(decl, do_canon)
end

-- Attempt to determine the actual declension (including plural variants)
-- based on a combination of the declension the user specified, what can be
-- detected from the lemma, and special case (1), if given in the declension.
-- DECL is the value the user passed for the declension field, after
-- extraneous annotations (special cases (1) and (2), * for reducible,
-- ё for ё/ё alternation and a ; that may precede ё) have been stripped off.
-- What's left is one of the following:
--
-- 1. Blank, meaning to autodetect the declension from the lemma
-- 2. A hyphen followed by a requested plural variant (only -ья currently)
-- 3. A gender (m, f, n, 3f)
-- 4. A gender plural plural variant (e.g. f-ья)
-- 5. An actual declension, possibly including a plural variant (e.g. о-ы) or
--    a slash declension (e.g. я/-ья, used for the noun дядя).
--
-- Return five args: stem (lemma minus ending), canonicalized declension,
-- whether the specified declension or detected ending was accented,
-- whether the detected ending was pl, and whether the declension was
-- autodetected (corresponds to cases where a full word with ending attached
-- is required in the lemma field). "Canonicalized" means after autodetection,
-- with accents removed, with any aliases mapped to their canonical versions
-- and with any requested plural variants applied. The result is either a
-- declension that will have a categorization entry (in declensions_cat[] or
-- declensions_old_cat[]) or a slash declension where each part similarly has
-- a categorization entry.
--
-- Note that gender is never required when an explicit declension is given,
-- and in connection with stem autodetection is required only when the lemma
-- either ends in -ь or is plural.
determine_decl = function(lemma, decl, args)
	-- Assume we're passed a value for DECL of types 1-4 above, and
	-- fetch gender and requested plural variant
	local gender = rmatch(decl, "^(3?[mfn]?)$")
	local stem = lemma
	local want_ya_plural, orig_pl_ending
	local was_autodetected
	if not gender then
		gender, want_ya_plural = rmatch(decl, "^(3?[mfn]?)(%-ья)$")
	end
	-- If DECL is of type 1-4, detect the actual declension from the lemma
	if gender then
		stem, decl, orig_pl_ending = detect_lemma_type(lemma, gender, args.a)
		was_autodetected = true
	else
		stem = lemma
	end
	-- The ending should be treated as accented if either the original singular
	-- or plural ending was accented, or if the stem is non-syllabic.
	local was_accented = com.is_stressed(decl) or
		orig_pl_ending and com.is_stressed(orig_pl_ending) or
		com.is_nonsyllabic(stem)
	local was_plural = not not orig_pl_ending
	decl = canonicalize_decl(decl, args.old)

	-- The rest of this code concerns plural variants. It's somewhat
	-- complicated because there are potentially four sources of plural
	-- variants (not to mention plural variants constructed using slash
	-- notation):
	--
	-- 1. A user-requested plural variant in declension types 2 or 4 above
	--    (currently only -ья)
	-- 2. An explicit plural variant encoded in an explicit declension of
	--    type 5 above
	-- 3. An autodetected plural variant (which will happen in some cases
	--    when autodetection is performed on a nominative plural)
	-- 4. A plural variant derived using special case (1).
	--
	-- Up to three actual plural variants might exist (e.g. if the user
	-- specifies a DECL value or 'm-ья(1)' and a STEM ending in -а,
	-- although not all three can ever be compatible because -ья and (1)
	-- are never compatible). We can't have all four because if there's
	-- an explicit plural variant, there won't be a user-requested or
	-- autodetected plural variant.
	--
	-- The goal below is to do two things: Check that all available plural
	-- variants are the same, and generate the actual declension.
	-- If we have a type-2 or type-3 variant, we already have the actual
	-- declension; else we need to use a table to map the basic declension
	-- to the one with the plural variant encoded in it.
	--
	-- NOTE: The code below was written with a more general plural-variant
	-- system. It probably can be simplified a lot now.

	-- 1: Handle explicit decl with slash variant
	if rfind(decl, "/") then
		if want_ya_plural then
			-- Don't think this can happen
			error("Plural variation " .. want_ya_plural .. " not compatible with slash declension " .. decl)
		end
		if args.want_sc1 then
			error("Special case (1) not compatible with slash declension" .. decl)
		end
		return stem, decl, was_accented, was_plural, was_autodetected
	end

	-- 2: Retrieve explicitly specified or autodetected decl and pl. variant
	local basic_decl, detected_or_explicit_plural = rmatch(decl, "^(.*)(%-[^mf]+)$")
	if basic_decl == "ь" then
		basic_decl = "ь-m"
	end
	basic_decl = basic_decl or decl

	-- 3: Any user-requested plural variant must agree with explicit or
	--    autodetected variant.
	if want_ya_plural and detected_or_explicit_plural and want_ya_plural ~= detected_or_explicit_plural then
		error("Plural variant " .. want_ya_plural .. " requested but plural variant " .. detected_or_explicit_plural .. " detected from plural stem")
	end

	-- 4: Handle special case (1). Derive the full declension, make sure its
	--    plural variant matches any other available plural variants, and
	--    return the declension.
	if args.want_sc1 then
		local sc1_decl = special_case_1_to_plural_variation[basic_decl] or
			error("Special case (1) not compatible with declension " .. basic_decl)
		local sc1_plural = rsub(sc1_decl, "^.*%-", "-")
		local other_plural = want_ya_plural or detected_or_explicit_plural
		if other_plural and sc1_plural ~= other_plural then
			error("Plural variant " .. other_plural .. " specified or detected, but special case (1) calls for plural variant " .. sc1_plural)
		end
		return stem, sc1_decl, was_accented, was_plural, was_autodetected
	end

	-- 5: Handle user-requested plural variant without explicit or detected
	--    one. (If an explicit or detected one exists, we've already checked
	--    that it agrees with the user-requested one, and so we already have
	--    our full declension.)
	if want_ya_plural and not detected_or_explicit_plural then
		local variant_decl
		if plural_variation_detection_map[decl] then
			variant_decl = plural_variation_detection_map[decl][want_ya_plural]
		end
		if variant_decl then
			return stem, variant_decl, was_accented, was_plural, was_autodetected
		else
			return stem, decl .. "/" .. want_ya_plural, was_accented, was_plural, was_autodetected
		end
	end

	-- 6: Just return the full declension, which will include any available
	--    plural variant in it.
	return stem, decl, was_accented, was_plural, was_autodetected
end

-- Attempt to determine the actual adjective declension based on a
-- combination of the declension the user specified and what can be detected
-- from the stem. DECL is the value the user passed for the declension field,
-- after extraneous annotations have been removed (although none are probably
-- relevant here). What's left is one of the following, which always begins
-- with +:
--
-- 1. +, meaning to autodetect the declension from the stem
-- 2. +ь, same as + but selects +ьий instead of +ий if lemma ends in -ий
-- 3. +short, +mixed or +proper, with the declension partly specified but
--    the particular gender/number-specific short/mixed variant to be
--    autodetected
-- 4. An actual declension, possibly including a slash declension
--    (WARNING: Unclear if slash declensions will work, especially those
--    that are adjective/noun combinations)
--
-- Returns the same five args as for determine_decl(). The returned
-- declension will always begin with +.
detect_adj_type = function(lemma, decl, old)
	local was_autodetected
	local base, ending
	if decl == "+" or decl == "+ь" then
		base, ending = rmatch(lemma, "^(.*)([ыиіьаяое]́?[йея])$")
		if ending == "ий" and decl == "+ь" then
			decl = "+ьий"
		elseif ending == "ій" and decl == "+ь" then
			decl = "+ьій"
		elseif ending then
			decl = "+" .. ending
		else
			base, ending = rmatch(lemma, "^(.-)([оаыъ]?́?)$")
			assert(base)
			local shortmixed = rfind(base, "^[" .. com.uppercase .. "].*[иы]́н$") and "stressed-proper" or -- accented
				rfind(base, "^[" .. com.uppercase .. "].*[иы]н$") and "proper" or --not accented
				rlfind(base, "[ёео]́?в$") and "short" or
				rlfind(base, "[ыи]́н$") and "stressed-short" or -- accented
				rlfind(base, "[ыи]н$") and "mixed" --not accented
			if not shortmixed then
				error("Cannot determine stem type of adjective: " .. lemma)
			end
			decl = "+" .. ending .. "-" .. shortmixed
		end
		was_autodetected = true
	elseif ut.contains({"+short", "+mixed", "+proper"}, decl) then
		base, ending = rmatch(lemma, "^(.-)([оаыъ]?́?)$")
		assert(base)
		local shortmixed = usub(decl, 2)
		if rlfind(base, "[ыи]́н$") then -- accented
			if shortmixed == "short" then shortmixed = "stressed-short"
			elseif shortmixed == "proper" then shortmixed = "stressed-proper"
			end
		end
		decl = "+" .. ending .. "-" .. shortmixed
		was_autodetected = true
	else
		base = lemma
	end

	-- Remove any accents from the declension, but not their presence.
	-- We will convert was_accented into stress pattern b, and convert that
	-- back to an accented version in determine_stress_variant(). This way
	-- we end up with the stressed version whether the user placed an accent
	-- in the ending or decl or specified stress pattern b.
	-- FIXME, might not work in the presence of slash declensions
	local was_accented = com.is_stressed(decl)
	decl = com.remove_accents(decl)

	function convert_sib_velar_variant(decl)
		local iend = rmatch(decl, "^%+[іи]([йея]?)$")
		-- Convert ій/ий to ый after velar or sibilant. This is important for
		-- velars; doesn't really matter one way or the other for sibilants as
		-- the sibilant rules will convert both sets of endings to the same
		-- thing (whereas there will be a difference with о vs. е for velars).
		if iend and rfind(base, "[" .. com.velar .. com.sib .. "]$") then
			decl = "+ы" .. iend
		-- The following is necessary for -ц, unclear if makes sense for
		-- sibilants. (Would be necessary -- I think -- if we were
		-- inferring short adjective forms, but we're not.)
		elseif decl == "+ее" and rfind(base, "[" .. com.sib_c .. "]$") then
			decl = "+ое"
		end
		return decl
	end

	decl = map_decl(decl, convert_sib_velar_variant)
	local singdecl
	if decl == "+ые" then
		singdecl = was_accented and "+ой" or "+ый"
	elseif decl == "+ие" and not old then
		singdecl = "+ий"
	elseif decl == "+іе" and old then
		singdecl = "+ій"
	elseif decl == "+ія" and old then
		singdecl = "+яя"
	elseif decl == "+ьи" then
		singdecl = old and "+ьій" or "+ьий"
	elseif rfind(decl, "^%+ы%-") then -- decl +ы-mixed or similar
		singdecl = "+" .. (old and "ъ" or "") .. usub(decl, 3)
	end
	if singdecl then
		was_plural = true
		decl = singdecl
	end
	return base, canonicalize_decl(decl, old), was_accented, was_plural, was_autodetected
end

-- If stress pattern omitted, detect it based on whether ending is stressed
-- or the decl class or stem accent calls for inherent stress, defaulting to
-- pattern a. This is run after alias resolution and accent removal of DECL;
-- WAS_ACCENTED indicates whether the ending was originally stressed.
-- FIXME: This is run before splitting slash patterns but should be run after.
detect_stress_pattern = function(stem, decl, decl_cats, reducible,
		was_plural, was_accented)
	-- ёнок and ёночек always bear stress
	if rfind(decl, "ёнокъ?") or rfind(decl, "ёночекъ?") then
		return "b"
	-- stressed suffix и́н; missing in plural and true endings don't bear stress
	-- (except for exceptional господи́н)
	elseif rfind(decl, "инъ?") and was_accented then
		return "d"
	-- Adjectival -ой always bears the stress
	elseif rfind(decl, "%+ой") then
		return "b"
	-- Pattern b if ending was accented by user
	elseif was_accented then
		return "b"
	-- Nonsyllabic stem means pattern b
	elseif com.is_nonsyllabic(stem) then
		return "b"
	-- Accent on reducible vowel in masc nom sg (not plural) means pattern b.
	-- Think about whether we want to enable this.
--	elseif reducible and not was_plural then
--		-- FIXME hack. Eliminate plural part of slash declension.
--		decl = rsub(decl, "/.*", "")
--		if decl_cats[decl] and decl_cats[decl].g == "m" then
--			if com.is_ending_stressed(stem) or com.is_monosyllabic(stem) then
--				return "b"
--			end
--		end
	end
	return "a"
end

-- In certain special cases, depending on the declension, we override the
-- user-specified stress pattern and convert it to something else.
-- NOTE: This function is run after alias resolution and accent removal,
-- but before canonicalizing the stress pattern from numbered to
-- Zaliznyak-style. FIXME: It's also run before splitting slash patterns
-- but should be run after.
override_stress_pattern = function(decl, stress)
	-- ёнок and ёночек always bear stress; if user specified a or 1,
	-- convert to b. Don't do this with slash patterns (see FIXME above).
	if (stress == "a" or stress == "1") and (rfind(decl, "^ёнокъ?$") or rfind(decl, "^ёночекъ?$")) then
		return "b"
	-- For compatibility, numbered pattern 2 can expand to either b or b';
	-- similarly, 6 can expand to either f or f''.
	elseif rfind(decl, "^ь%-f") then
		if stress == "2" then
			return "b'"
		elseif stress == "6" then
			return "f''"
		end
	end
	return stress
end

determine_stress_variant = function(decl, stress)
	if stress == "b" then
		if decl == "+ая" then
			return "+а́я"
		elseif decl == "+ое" then
			return "+о́е"
		else
			-- Convert +...-short to +...-stressed-short and same for -proper
			local b, e = rmatch(decl, "^%+(.*)%-(short)$")
			if not b then
				b, e = rmatch(decl, "^%+(.*)%-(proper)$")
			end
			if b then
				return "+" .. b .. "-stressed-" .. e
			end
		end
	end
	return decl
end

is_reducible = function(decl_cat)
	if decl_cat.suffix or decl_cat.cant_reduce or decl_cat.adj then
		return false
	elseif decl_cat.decl == "3rd" and decl_cat.g == "f" or decl_cat.g == "m" then
		return true
	else
		return false
	end
end

-- Reduce nom sg to stem by eliminating the "epenthetic" vowel. Applies to
-- masculine 2nd-declension hard and soft, and 3rd-declension feminine in
-- -ь. STEM and DECL are after determine_decl(), before converting
-- outward-facing declensions to inward ones.
function export.reduce_nom_sg_stem(stem, decl, can_err)
	local full_stem = stem .. (decl == "й" and decl or "")
	local ret = com.reduce_stem(full_stem)
	if not ret and can_err then
		error("Unable to reduce stem " .. stem)
	end
	return ret
end

is_dereducible = function(decl_cat)
	if decl_cat.suffix or decl_cat.cant_reduce or decl_cat.adj then
		return false
	elseif decl_cat.decl == "1st" or decl_cat.decl == "2nd" and decl_cat.g == "n" then
		return true
	else
		return false
	end
end

-- Add a possible suffix to the bare stem, according to the declension and
-- value of OLD. This may be -ь, -ъ, -й or nothing. We need to do this here
-- because we don't actually attach such a suffix in attach_unstressed() due
-- to situations where we don't want the suffix added, e.g. dereducible nouns
-- in -ня.
add_bare_suffix = function(bare, old, sgdc, dereduced)
	if old and sgdc.hard == "hard" then
		return bare .. "ъ"
	elseif sgdc.hard == "soft" or sgdc.hard == "palatal" then
		-- This next clause corresponds to a special case in Vitalik's module.
		-- It says that nouns in -ня (accent class a) have gen pl without
		-- trailing -ь. It appears to apply to most nouns in -ня (possibly
		-- all in -льня), but ку́хня (gen pl ку́хонь) and дерéвня (gen pl
		-- дереве́нь) is an exception. (Vitalik's module has an extra
		-- condition here 'stress == "a"' that would exclude дере́вня but I
		-- don't think this condition is in Zaliznyak, as he indicates
		-- дере́вня as having an exceptional genitive plural.)
		if dereduced and rfind(bare, "[нН]$") and sgdc.decl == "1st" then
			-- FIXME: What happens in this case old-style? I assume that
			-- -ъ is added, but this is a guess.
			return bare .. (old and "ъ" or "")
		elseif rfind(bare, "[" .. com.vowel .. "]́?$") then
			return bare .. "й"
		else
			return bare .. "ь"
		end
	else
		return bare
	end
end

-- Dereduce stem to the form found in the gen pl (and maybe nom sg) by
-- inserting an epenthetic vowel. Applies to 1st declension and 2nd
-- declension neuter, and to 2nd declension masculine when the stem was
-- specified as a plural form (in which case we're deriving the nom sg,
-- and also the gen pl in the alt-gen-pl scenario). STEM and DECL are
-- after determine_decl(), before converting outward-facing declensions
-- to inward ones. STRESS is the stess pattern.
function export.dereduce_nom_sg_stem(stem, sgdc, stress, old, can_err)
	local epenthetic_stress = ending_stressed_gen_pl_patterns[stress]
	local ret = com.dereduce_stem(stem, epenthetic_stress)
	if not ret then
		if can_err then
			error("Unable to dereduce stem " .. stem)
		else
			return nil
		end
	end
	return add_bare_suffix(ret, old, sgdc, true)
end

--------------------------------------------------------------------------
--                      Second-declension masculine                     --
--------------------------------------------------------------------------

----------------- Masculine hard -------------------

-- Hard-masculine declension, ending in a hard consonant
-- (ending in -ъ, old-style).
declensions_old["ъ"] = {
	["nom_sg"] = "ъ",
	["gen_sg"] = "а́",
	["dat_sg"] = "у́",
	["acc_sg"] = nil,
	["ins_sg"] = "о́мъ",
	["pre_sg"] = "ѣ́",
	["nom_pl"] = "ы́",
	["gen_pl"] = function(stem, stress)
		return sibilant_suffixes[ulower(usub(stem, -1))] and "е́й" or "о́въ"
	end,
	["alt_gen_pl"] = "ъ",
	["dat_pl"] = "а́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "а́ми",
	["pre_pl"] = "а́хъ",
}

declensions_old_cat["ъ"] = { decl="2nd", hard="hard", g="m" }

-- Normal mapping of old ъ would be "" (blank), but we set up "#" as an alias
-- so we have a way of referring to it without defaulting if need be and
-- distinct from auto-detection (e.g. in the second stem of a word, or to
-- override autodetection of -ёнок or -ин -- the latter is necessary in the
-- case of семьянин).
declensions_aliases["#"] = ""

----------------- Masculine hard, irregular plural -------------------

-- Hard-masculine declension, ending in a hard consonant
-- (ending in -ъ, old-style), with irreg nom pl -а.
declensions_old["ъ-а"] = mw.clone(declensions_old["ъ"])
declensions_old["ъ-а"]["nom_pl"] = "а́"

declensions_old_cat["ъ-а"] = { decl="2nd", hard="hard", g="m", irregpl=true }
declensions_cat["-а"] = {
	singular = "ending in a consonant",
	decl="2nd", hard="hard", g="m", irregpl=true
}
declensions_aliases["#-a"] = "-a"

-- Hard-masculine declension, ending in a hard consonant
-- (ending in -ъ, old-style), with irreg soft pl -ья.
-- Differs from the normal declension throughout the plural.
declensions_old["ъ-ья"] = {
	["nom_sg"] = "ъ",
	["gen_sg"] = "а́",
	["dat_sg"] = "у́",
	["acc_sg"] = nil,
	["ins_sg"] = "о́мъ",
	["pre_sg"] = "ѣ́",
	["nom_pl"] = "ья́",
	["gen_pl"] = "ьёвъ",
	["alt_gen_pl"] = "е́й",
	["dat_pl"] = "ья́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "ья́ми",
	["pre_pl"] = "ья́хъ",
}

declensions_old_cat["ъ-ья"] = { decl="2nd", hard="hard", g="m", irregpl=true }
declensions_cat["-ья"] = {
	singular = "ending in a consonant",
	decl="2nd", hard="hard", g="m", irregpl=true,
}
declensions_aliases["#-ья"] = "-ья"

----------------- Masculine hard, suffixed, irregular plural -------------------

declensions_old["инъ"] = {
	["nom_sg"] = "и́нъ",
	["gen_sg"] = "и́на",
	["dat_sg"] = "и́ну",
	["acc_sg"] = nil,
	["ins_sg"] = "и́номъ",
	["pre_sg"] = "и́нѣ",
	["nom_pl"] = "е́",
	["gen_pl"] = "ъ",
	["dat_pl"] = "а́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "а́ми",
	["pre_pl"] = "а́хъ",
}

declensions_old_cat["инъ"] = { decl="2nd", hard="hard", g="m", suffix=true }

declensions_old["ёнокъ"] = {
	["nom_sg"] = "ёнокъ",
	["gen_sg"] = "ёнка",
	["dat_sg"] = "ёнку",
	["acc_sg"] = nil,
	["ins_sg"] = "ёнкомъ",
	["pre_sg"] = "ёнкѣ",
	["nom_pl"] = "я́та",
	["gen_pl"] = "я́тъ",
	["dat_pl"] = "я́тамъ",
	["acc_pl"] = nil,
	["ins_pl"] = "я́тами",
	["pre_pl"] = "я́тахъ",
}
declensions_old_cat["ёнокъ"] = { decl="2nd", hard="hard", g="m", suffix=true }

declensions_old_aliases["онокъ"] = "ёнокъ"
declensions_old_aliases["енокъ"] = "ёнокъ"

declensions_old["ёночекъ"] = {
	["nom_sg"] = "ёночекъ",
	["gen_sg"] = "ёночка",
	["dat_sg"] = "ёночку",
	["acc_sg"] = nil,
	["ins_sg"] = "ёночкомъ",
	["pre_sg"] = "ёночкѣ",
	["nom_pl"] = "я́тки",
	["gen_pl"] = "я́токъ",
	["dat_pl"] = "я́ткамъ",
	["acc_pl"] = nil,
	["ins_pl"] = "я́тками",
	["pre_pl"] = "я́ткахъ",
}
declensions_old_cat["ёночекъ"] = { decl="2nd", hard="hard", g="m", suffix=true }

declensions_old_aliases["оночекъ"] = "ёночекъ"
declensions_old_aliases["еночекъ"] = "ёночекъ"

----------------- Masculine soft -------------------

-- Normal soft-masculine declension in -ь
declensions_old["ь-m"] = {
	["nom_sg"] = "ь",
	["gen_sg"] = "я́",
	["dat_sg"] = "ю́",
	["acc_sg"] = nil,
	["ins_sg"] = "ёмъ",
	["pre_sg"] = "ѣ́",
	["nom_pl"] = "и́",
	["gen_pl"] = "е́й",
	["alt_gen_pl"] = "ь",
	["dat_pl"] = "я́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "я́ми",
	["pre_pl"] = "я́хъ",
}

declensions_old_cat["ь-m"] = { decl="2nd", hard="soft", g="m" }

-- Soft-masculine declension in -ь with irreg nom pl -я
declensions_old["ь-я"] = mw.clone(declensions_old["ь-m"])
declensions_old["ь-я"]["nom_pl"] = "я́"

declensions_old_cat["ь-я"] = { decl="2nd", hard="soft", g="m", irregpl=true }

----------------- Masculine palatal -------------------

-- Masculine declension in palatal -й
declensions_old["й"] = {
	["nom_sg"] = "й",
	["gen_sg"] = "я́",
	["dat_sg"] = "ю́",
	["acc_sg"] = nil,
	["ins_sg"] = "ёмъ",
	["pre_sg"] = function(stem, stress)
		return rlfind(stem, "[іи]́?$") and not ending_stressed_pre_sg_patterns[stress] and "и" or "ѣ́"
	end,
	["nom_pl"] = "и́",
	["gen_pl"] = "ёвъ",
	["alt_gen_pl"] = "й",
	["dat_pl"] = "я́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "я́ми",
	["pre_pl"] = "я́хъ",
}

declensions_old_cat["й"] = { decl="2nd", hard="palatal", g="m" }

declensions_old["й-я"] = mw.clone(declensions_old["й"])
declensions_old["й-я"]["nom_pl"] = "я́"

declensions_old_cat["й-я"] = { decl="2nd", hard="palatal", g="m", irregpl=true }

--------------------------------------------------------------------------
--                       First-declension feminine                      --
--------------------------------------------------------------------------

----------------- Feminine hard -------------------

-- Hard-feminine declension in -а
declensions_old["а"] = {
	["nom_sg"] = "а́",
	["gen_sg"] = "ы́",
	["dat_sg"] = "ѣ́",
	["acc_sg"] = "у́",
	["ins_sg"] = {"о́й", "о́ю"},
	["pre_sg"] = "ѣ́",
	["nom_pl"] = "ы́",
	["gen_pl"] = function(stem, stress)
		return sibilant_suffixes[ulower(usub(stem, -1))] and ending_stressed_gen_pl_patterns[stress] and "е́й" or "ъ"
	end,
	["alt_gen_pl"] = "е́й",
	["dat_pl"] = "а́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "а́ми",
	["pre_pl"] = "а́хъ",
}

declensions_old_cat["а"] = { decl="1st", hard="hard", g="f" }

----------------- Feminine soft -------------------

-- Soft-feminine declension in -я
declensions_old["я"] = {
	["nom_sg"] = "я́",
	["gen_sg"] = "и́",
	["dat_sg"] = function(stem, stress)
		return rlfind(stem, "[іи]́?$") and not ending_stressed_dat_sg_patterns[stress] and "и" or "ѣ́"
	end,
	["acc_sg"] = "ю́",
	["ins_sg"] = {"ёй", "ёю"},
	["pre_sg"] = function(stem, stress)
		return rlfind(stem, "[іи]́?$") and not ending_stressed_pre_sg_patterns[stress] and "и" or "ѣ́"
	end,
	["nom_pl"] = "и́",
	["gen_pl"] = function(stem, stress)
		return ending_stressed_gen_pl_patterns[stress] and not rlfind(stem, "[" .. com.vowel .. "]́?$") and "е́й" or "й"
	end,
	["alt_gen_pl"] = "е́й",
	["dat_pl"] = "я́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "я́ми",
	["pre_pl"] = "я́хъ",
}

declensions_old_cat["я"] = { decl="1st", hard="soft", g="f" }

-- Soft-feminine declension in -ья.
-- Almost like ь + -я endings except for genitive plural.
declensions_old["ья"] = {
	["nom_sg"] = "ья́",
	["gen_sg"] = "ьи́",
	["dat_sg"] = "ьѣ́",
	["acc_sg"] = "ью́",
	["ins_sg"] = {"ьёй", "ьёю"},
	["pre_sg"] = "ьѣ́",
	["nom_pl"] = "ьи́",
	["gen_pl"] = function(stem, stress)
		-- circumflex accent is a signal that forces stress, particularly
		-- in accent pattern d/d'.
		return (ending_stressed_gen_pl_patterns[stress] or stress == "d" or stress == "d'") and "е̂й" or "ий"
	end,
	["dat_pl"] = "ья́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "ья́ми",
	["pre_pl"] = "ья́хъ",
}

declensions_old_cat["ья"] = {
	decl="1st", hard="soft", g="f",
	stem_suffix="ь", gensg=true,
	ignore_reduce=true -- already has dereduced gen pl
}

--------------------------------------------------------------------------
--                       Second-declension neuter                       --
--------------------------------------------------------------------------

----------------- Neuter hard -------------------

-- Normal hard-neuter declension in -о
declensions_old["о"] = {
	["nom_sg"] = "о́",
	["gen_sg"] = "а́",
	["dat_sg"] = "у́",
	["acc_sg"] = "о́",
	["ins_sg"] = "о́мъ",
	["pre_sg"] = "ѣ́",
	["nom_pl"] = "а́",
	["gen_pl"] = function(stem, stress)
		return sibilant_suffixes[ulower(usub(stem, -1))] and ending_stressed_gen_pl_patterns[stress] and "е́й" or "ъ"
	end,
	["alt_gen_pl"] = "о́въ",
	["dat_pl"] = "а́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "а́ми",
	["pre_pl"] = "а́хъ",
}
declensions_old_cat["о"] = { decl="2nd", hard="hard", g="n" }

-- Hard-neuter declension in -о with irreg nom pl -и
declensions_old["о-и"] = mw.clone(declensions_old["о"])
declensions_old["о-и"]["nom_pl"] = "ы́"
declensions_old_cat["о-и"] = { decl="2nd", hard="hard", g="n", irregpl=true }

declensions_old_aliases["о-ы"] = "о-и"

----------------- Neuter soft -------------------

-- Soft-neuter declension in -е (stressed -ё)
declensions_old["е"] = {
	["nom_sg"] = "ё",
	["gen_sg"] = "я́",
	["dat_sg"] = "ю́",
	["acc_sg"] = "ё",
	["ins_sg"] = "ёмъ",
	["pre_sg"] = function(stem, stress)
		return rlfind(stem, "[іи]́?$") and not ending_stressed_pre_sg_patterns[stress] and "и" or "ѣ́"
	end,
	["nom_pl"] = "я́",
	["gen_pl"] = function(stem, stress)
		return ending_stressed_gen_pl_patterns[stress] and not rlfind(stem, "[" .. com.vowel .. "]́?$") and "е́й" or "й"
	end,
	["alt_gen_pl"] = "ёвъ",
	["dat_pl"] = "я́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "я́ми",
	["pre_pl"] = "я́хъ",
}

declensions_old_cat["е"] = {
	singular = function(suffix)
		if suffix == "ё" then
			return "ending in -ё"
		else
			return {}
		end
	end,
	decl="2nd", hard="soft", g="n", gensg=true
}

-- User-facing declension type "ё" = "е"
declensions_old_aliases["ё"] = "е"

-- Rare soft-neuter declension in stressed -е́ (e.g. муде́, бытие́)
declensions_old["е́"] = {
	["nom_sg"] = "е́",
	["gen_sg"] = "я́",
	["dat_sg"] = "ю́",
	["acc_sg"] = "е́",
	["ins_sg"] = "е́мъ",
	["pre_sg"] = function(stem, stress)
		-- FIXME!!! Are we sure about this condition? This is what was
		-- found in the old template, but the related -е declension has
		-- -ие prep sg ending -(и)и only when *not* stressed.
		return rlfind(stem, "[іи]́?$") and "и́" or "ѣ́"
	end,
	["nom_pl"] = "я́",
	["gen_pl"] = function(stem, stress)
		return rlfind(stem, "[" .. com.vowel .. "]́?$") and "й" or "е́й"
	end,
	["alt_gen_pl"] = "ёвъ",
	["dat_pl"] = "я́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "я́ми",
	["pre_pl"] = "я́хъ",
}

declensions_old_cat["е́"] = {
	singular = "ending in stressed -е",
	decl="2nd", hard="soft", g="n", gensg=true
}

-- Soft-neuter declension in unstressed -ье (stressed -ьё).
declensions_old["ье"] = {
	["nom_sg"] = "ьё",
	["gen_sg"] = "ья́",
	["dat_sg"] = "ью́",
	["acc_sg"] = "ьё",
	["ins_sg"] = "ьёмъ",
	["pre_sg"] = "ьѣ́",
	["nom_pl"] = "ья́",
	["gen_pl"] = function(stem, stress)
		return ending_stressed_gen_pl_patterns[stress] and "е́й" or "ий"
	end,
	["alt_gen_pl"] = "ьёвъ",
	["dat_pl"] = "ья́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "ья́ми",
	["pre_pl"] = "ья́хъ",
}

declensions_old_cat["ье"] = {
	decl="2nd", hard="soft", g="n",
	stem_suffix="ь", gensg=true,
	ignore_reduce=true -- already has dereduced gen pl
}

declensions_old_aliases["ьё"] = "ье"

--------------------------------------------------------------------------
--                           Third declension                           --
--------------------------------------------------------------------------

declensions_old["ь-f"] = {
	["nom_sg"] = "ь",
	["gen_sg"] = "и́",
	["dat_sg"] = "и́",
	["acc_sg"] = "ь",
	["ins_sg"] = "ью́",
	["pre_sg"] = "и́",
	["nom_pl"] = "и́",
	["gen_pl"] = "е́й",
	["dat_pl"] = "я́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "я́ми",
	["pre_pl"] = "я́хъ",
}

declensions_old_cat["ь-f"] = { decl="3rd", hard="soft", g="f" }

declensions_old["мя"] = {
	["nom_sg"] = "мя",
	["gen_sg"] = "мени",
	["dat_sg"] = "мени",
	["acc_sg"] = nil,
	["ins_sg"] = "менемъ",
	["pre_sg"] = "мени",
	["nom_pl"] = "мена́",
	["gen_pl"] = "мёнъ",
	["dat_pl"] = "мена́мъ",
	["acc_pl"] = nil,
	["ins_pl"] = "мена́ми",
	["pre_pl"] = "мена́хъ",
}

declensions_old_cat["мя"] = { decl="3rd", hard="soft", g="n", cant_reduce=true }

--------------------------------------------------------------------------
--                              Invariable                              --
--------------------------------------------------------------------------

-- Invariable declension; no endings.
declensions_old["$"] = {
	["nom_sg"] = "",
	["gen_sg"] = "",
	["dat_sg"] = "",
	["acc_sg"] = "",
	["ins_sg"] = "",
	["pre_sg"] = "",
	["nom_pl"] = "",
	["gen_pl"] = "",
	["dat_pl"] = "",
	["acc_pl"] = "",
	["ins_pl"] = "",
	["pre_pl"] = "",
}
declensions_old_cat["$"] = { decl="invariable", hard="none", g="none" }

--------------------------------------------------------------------------
--                              Adjectival                              --
--------------------------------------------------------------------------

-- This needs to be up here because it is called just below.
local function old_to_new(v)
	v = rsub(v, "ъ$", "")
	v = rsub(v, "^ъ", "")
	v = rsub(v, "(%A)ъ", "%1")
	v = rsub(v, "ъ(%A)", "%1")
	v = rsub(v, "і", "и")
	v = rsub(v, "ѣ", "е")
	return v
end

-- Meaning of entry is:
-- 1. The declension name in module ru-adjective
-- 2. The masculine declension name in this module
-- 3. The neuter declension name in this module
-- 4. The feminine declension name in this module
-- 5. The value of hard= for the declensions_cat entry
-- 6. The value of decl= for the declensions_cat entry
-- 7. The value of possadj= for the declensions_cat entry (true if possessive
--    or similar type of adjective)
local adj_decl_map = {
	{"ый", "ый", "ое", "ая", "hard", "long", false},
	{"ій", "ій", "ее", "яя", "soft", "long", false},
	{"ой", "ой", "о́е", "а́я", "hard", "long", false},
	{"ьій", "ьій", "ье", "ья", "palatal", "long", true},
	{"short", "ъ-short", "о-short", "а-short", "hard", "short", true},
	{"mixed", "ъ-mixed", "о-mixed", "а-mixed", "hard", "mixed", true},
	{"proper", "ъ-proper", "о-proper", "а-proper", "hard", "proper", true},
	{"stressed-short", "ъ-stressed-short", "о-stressed-short", "а-stressed-short", "hard", "short", true},
	{"stressed-proper", "ъ-stressed-proper", "о-stressed-proper", "а-stressed-proper", "hard", "proper", true},
}

local function get_adjectival_decl(adjtype, gender, old)
	local decl = m_ru_adj.get_nominal_decl(adjtype, gender, old)
	-- signal to make_table() to use the special tr_adj() function so that
	-- -го gets transliterated to -vo
	if type(decl["gen_sg"]) == "table" then
		local entries = {}
		for _, entry in ipairs(decl["gen_sg"]) do
			table.insert(entries, rsub(entry, "го$", "го<adj>"))
		end
		decl["gen_sg"] = entries
	else
		decl["gen_sg"] = rsub(decl["gen_sg"], "го$", "го<adj>")
	end
	return decl
end

for _, declspec in ipairs(adj_decl_map) do
	local oadjdecl = declspec[1]
	local nadjdecl = old_to_new(oadjdecl)
	local odecl = {m="+" .. declspec[2], n="+" .. declspec[3], f="+" .. declspec[4]}
	local hard = declspec[5]
	local decltype = declspec[6]
	local possadj = declspec[7]
	for _, g in ipairs({"m", "n", "f"}) do
		declensions_old[odecl[g]] =
			get_adjectival_decl(oadjdecl, g, true)
		declensions[old_to_new(odecl[g])] =
			get_adjectival_decl(nadjdecl, g, false)
		declensions_old_cat[odecl[g]] = {
			decl=decltype, hard=hard, g=g, adj=true, possadj=possadj }
		declensions_cat[old_to_new(odecl[g])] = {
			decl=decltype, hard=hard, g=g, adj=true, possadj=possadj }
	end
end

-- Set up some aliases.
declensions_old_aliases["+о́-short"] = "+о-stressed-short"
declensions_old_aliases["+а́-short"] = "+а-stressed-short"
declensions_old_aliases["+о́-proper"] = "+о-stressed-proper"
declensions_old_aliases["+а́-proper"] = "+а-stressed-proper"
declensions_aliases["+#-short"] = "+-short"
declensions_aliases["+#-mixed"] = "+-mixed"
declensions_aliases["+#-proper"] = "+-proper"
declensions_aliases["+#-stressed-short"] = "+-stressed-short"
declensions_aliases["+#-stressed-proper"] = "+-stressed-proper"

--------------------------------------------------------------------------
--                         Populate new from old                        --
--------------------------------------------------------------------------

-- Function to convert an entry in an old declensions table to new.
local function old_decl_entry_to_new(v)
	if type(v) == "table" then
		local new_entry = {}
		for _, i in ipairs(v) do
			table.insert(new_entry, old_decl_entry_to_new(i))
		end
		return new_entry
	elseif type(v) == "function" then
		return function(stem, suffix)
			return old_decl_entry_to_new(v(stem, suffix))
		end
	else
		return old_to_new(v)
	end
end

-- Function to convert an old declensions table to new.
local function old_decl_to_new(odecl)
	local ndecl = {}
	for k, v in pairs(odecl) do
		ndecl[k] = old_decl_entry_to_new(v)
	end
	return ndecl
end

-- Function to convert an entry in an old declensions_cat table to new.
local function old_decl_cat_entry_to_new(odecl_cat_entry)
	if not odecl_cat_entry then
		return nil
	elseif type(odecl_cat_entry) == "function" then
		return function(suffix)
			return old_decl_cat_entry_to_new(odecl_cat_entry(suffix))
		end
	elseif type(odecl_cat_entry) == "table" then
		local ndecl_cat_entry = {}
		for k, v in pairs(odecl_cat_entry) do
			ndecl_cat_entry[k] = old_decl_cat_entry_to_new(v)
		end
		return ndecl_cat_entry
	elseif type(odecl_cat_entry) == "boolean" then
		return odecl_cat_entry
	else
		assert(type(odecl_cat_entry) == "string")
		return old_to_new(odecl_cat_entry)
	end
end

-- Function to convert an old declensions_cat table to new.
local function old_decl_cat_to_new(odeclcat)
	local ndeclcat = {}
	for k, v in pairs(odeclcat) do
		ndeclcat[k] = old_decl_cat_entry_to_new(v)
	end
	return ndeclcat
end

-- populate declensions[] from declensions_old[]
for odecltype, odecl in pairs(declensions_old) do
	local ndecltype = old_to_new(odecltype)
	if not declensions[ndecltype] then
		declensions[ndecltype] = old_decl_to_new(odecl)
	end
end

-- populate declensions_cat[] from declensions_old_cat[]
for odecltype, odeclcat in pairs(declensions_old_cat) do
	local ndecltype = old_to_new(odecltype)
	if not declensions_cat[ndecltype] then
		declensions_cat[ndecltype] = old_decl_cat_to_new(odeclcat)
	end
end

-- populate declensions_aliases[] from declensions_old_aliases[]
for ofrom, oto in pairs(declensions_old_aliases) do
	local from = old_to_new(ofrom)
	if not declensions_aliases[from] then
		declensions_aliases[from] = old_to_new(oto)
	end
end

--------------------------------------------------------------------------
--                        Inflection functions                          --
--------------------------------------------------------------------------

local stressed_sibilant_rules = {
	["я"] = "а",
	["ы"] = "и",
	["ё"] = "о́",
	["ю"] = "у",
}

local stressed_c_rules = {
	["я"] = "а",
	["ё"] = "о́",
	["ю"] = "у",
}

local unstressed_sibilant_rules = {
	["я"] = "а",
	["ы"] = "и",
	["о"] = "е",
	["ю"] = "у",
}

local unstressed_c_rules = {
	["я"] = "а",
	["о"] = "е",
	["ю"] = "у",
}

local velar_rules = {
	["ы"] = "и",
}

local stressed_rules = {
	["ш"] = stressed_sibilant_rules,
	["щ"] = stressed_sibilant_rules,
	["ч"] = stressed_sibilant_rules,
	["ж"] = stressed_sibilant_rules,
	["ц"] = stressed_c_rules,
	["к"] = velar_rules,
	["г"] = velar_rules,
	["х"] = velar_rules,
}

local unstressed_rules = {
	["ш"] = unstressed_sibilant_rules,
	["щ"] = unstressed_sibilant_rules,
	["ч"] = unstressed_sibilant_rules,
	["ж"] = unstressed_sibilant_rules,
	["ц"] = unstressed_c_rules,
	["к"] = velar_rules,
	["г"] = velar_rules,
	["х"] = velar_rules,
}

nonsyllabic_suffixes = ut.list_to_set({"", "ъ", "ь", "й"})

sibilant_suffixes = ut.list_to_set({"ш", "щ", "ч", "ж"})

local function combine_stem_and_suffix(stem, suf, rules, old)
	local first = usub(suf, 1, 1)
	if rules then
		local conv = rules[first]
		if conv then
			local ending = usub(suf, 2)
			if old and conv == "и" and mw.ustring.find(ending, "^́?[" .. com.vowel .. "]") then
				conv = "і"
			end
			suf = conv .. ending
		end
	end
	return stem .. suf, suf
end

-- Attach the stressed stem (or plural stem, or barestem) out of ARGS
-- to the unstressed suffix SUF, modifying the suffix as necessary for the
-- last letter of the stem (e.g. if it is velar, sibilant or ц). CASE is
-- the case form being created and is used to select the plural stem if
-- needed. Returns two values, the combined form and the modified suffix.
local function attach_unstressed(args, case, suf, was_stressed)
	if suf == nil then
		return nil, nil
	elseif rfind(suf, CFLEX) then -- if suf has circumflex accent, it forces stressed
		return attach_stressed(args, case, suf)
	end
	local old = args.old
	local stem = rfind(case, "_pl") and args.pl or case == "ins_sg" and args.ins_sg_stem or args.stem
	if nonsyllabic_suffixes[suf] then
		-- If gen_pl, use special args.gen_pl_bare if given, else regular
		-- args.bare if there isn't a plural stem. If nom_sg, always use
		-- regular args.bare.
		local barearg
		if case == "gen_pl" then
			barearg = args.gen_pl_bare or (args.pl == args.stem) and args.bare
		else
			barearg = args.bare
		end
		local barestem = barearg or stem
		if was_stressed and case == "gen_pl" then
			if not barearg then
				local gen_pl_stem = com.make_ending_stressed(stem)
				-- FIXME: temporary tracking code to identify places where
				-- the change to the algorithm here that end-stresses the
				-- genitive plural in stress patterns with gen pl end stress
				-- (cf. words like голова́, with nom pl. го́ловы but gen pl.
				-- голо́в) would cause changes.
				if com.is_stressed(stem) and stem ~= gen_pl_stem then
					track("gen-pl-moved-stress")
				end
				barestem = gen_pl_stem
			end
		end

		if rlfind(barestem, "[йьъ]$") then
			suf = ""
		else
			if suf == "ъ" then
				-- OK
			elseif suf == "й" or suf == "ь" then
				if barearg and case == "gen_pl" then
					-- FIXME: temporary tracking code
					track("explicit-bare-no-suffix")
					if old then
						track("explicit-bare-old-no-suffix")
					end
					-- explicit bare or reducible, don't add -ь
					suf = ""
				elseif rfind(barestem, "[" .. com.vowel .. "]́?$") then
					-- not reducible, do add -ь and correct to -й if necessary
					suf = "й"
				else
					suf = "ь"
				end
			end
		end
		return barestem .. suf, suf
	end
	suf = com.make_unstressed(suf)
	local rules = unstressed_rules[ulower(usub(stem, -1))]
	return combine_stem_and_suffix(stem, suf, rules, old)
end

-- Analogous to attach_unstressed() but for the unstressed stem and a
-- stressed suffix.
attach_stressed = function(args, case, suf)
	if suf == nil then
		return nil, nil
	end
 	-- circumflex forces stress even when the accent pattern calls for no stress
	suf = rsub(suf, "̂", "́")
	if not rfind(suf, "[ё́]") then -- if suf has no "ё" or accent marks
		return attach_unstressed(args, case, suf, "was stressed")
	end
	local old = args.old
	local stem = rfind(case, "_pl") and args.upl or args.ustem
	local rules = stressed_rules[ulower(usub(stem, -1))]
	return combine_stem_and_suffix(stem, suf, rules, old)
end

-- Attach the appropriate stressed or unstressed stem (or plural stem as
-- determined by CASE, or barestem) out of ARGS to the suffix SUF, which may
-- be a list of alternative suffixes (e.g. in the inst sg of feminine nouns).
-- Calls FUN (either attach_stressed() or attach_unstressed() to do the work
-- for an individual suffix. Returns two values, a list of combined forms
-- and a list of the real suffixes used (which may be modified from the
-- passed-in suffixes, e.g. by removing stress marks or modifying vowels in
-- various ways after a stem-final velar, sibilant or ц).
local function attach_with(args, case, suf, fun)
	if type(suf) == "table" then
		local all_combineds = {}
		local all_realsufs = {}
		for _, x in ipairs(suf) do
			local combineds, realsufs = attach_with(args, case, x, fun)
			for _, combined in ipairs(combineds) do
				table.insert(all_combineds, combined)
			end
			for _, realsuf in ipairs(realsufs) do
				table.insert(all_realsufs, realsuf)
			end
		end
		return all_combineds, all_realsufs
	else
		local combined, realsuf = fun(args, case, suf)
		return {combined and combined .. args.suffix}, {realsuf}
	end
end

-- Generate the form(s) and suffix(es) for CASE according to the declension
-- table DECL, using the attachment function FUN (one of attach_stressed()
-- or attach_unstressed()).
local function gen_form(args, decl, case, stress, fun)
	if not args.forms[case] then
		args.forms[case] = {}
	end
	if not args.suffixes[case] then
		args.suffixes[case] = {}
	end
	local suf = decl[case]
	if type(suf) == "function" then
		suf = suf(rfind(case, "_pl") and args.pl or args.stem, stress)
	end
	if case == "gen_pl" and args.alt_gen_pl then
		suf = decl.alt_gen_pl
		if not suf then
			error("No alternate genitive plural available for this declension class")
		end
	end
	local combineds, realsufs = attach_with(args, case, suf, fun)
	for _, form in ipairs(combineds) do
		ut.insert_if_not(args.forms[case], form)
	end
	for _, realsuf in ipairs(realsufs) do
		ut.insert_if_not(args.suffixes[case], realsuf)
	end
end

local attachers = {
	["+"] = attach_stressed,
	["-"] = attach_unstressed,
}

do_stress_pattern = function(stress, args, decl, number)
	for _, case in ipairs(decl_cases) do
		if not number or (number == "sg" and rfind(case, "_sg")) or
			(number == "pl" and rfind(case, "_pl")) then
			gen_form(args, decl, case, stress,
				attachers[stress_patterns[stress][case]])
		end
	end
end

stress_patterns["a"] = {
	nom_sg="-", gen_sg="-", dat_sg="-", acc_sg="-", ins_sg="-", pre_sg="-",
	nom_pl="-", gen_pl="-", dat_pl="-", acc_pl="-", ins_pl="-", pre_pl="-",
}

stress_patterns["b"] = {
	nom_sg="+", gen_sg="+", dat_sg="+", acc_sg="+", ins_sg="+", pre_sg="+",
	nom_pl="+", gen_pl="+", dat_pl="+", acc_pl="+", ins_pl="+", pre_pl="+",
}

stress_patterns["b'"] = {
	nom_sg="+", gen_sg="+", dat_sg="+", acc_sg="+", ins_sg="-", pre_sg="+",
	nom_pl="+", gen_pl="+", dat_pl="+", acc_pl="+", ins_pl="+", pre_pl="+",
}

stress_patterns["c"] = {
	nom_sg="-", gen_sg="-", dat_sg="-", acc_sg="-", ins_sg="-", pre_sg="-",
	nom_pl="+", gen_pl="+", dat_pl="+", acc_pl="+", ins_pl="+", pre_pl="+",
}

stress_patterns["d"] = {
	nom_sg="+", gen_sg="+", dat_sg="+", acc_sg="+", ins_sg="+", pre_sg="+",
	nom_pl="-", gen_pl="-", dat_pl="-", acc_pl="-", ins_pl="-", pre_pl="-",
}

stress_patterns["d'"] = {
	nom_sg="+", gen_sg="+", dat_sg="+", acc_sg="-", ins_sg="+", pre_sg="+",
	nom_pl="-", gen_pl="-", dat_pl="-", acc_pl="-", ins_pl="-", pre_pl="-",
}

stress_patterns["e"] = {
	nom_sg="-", gen_sg="-", dat_sg="-", acc_sg="-", ins_sg="-", pre_sg="-",
	nom_pl="-", gen_pl="+", dat_pl="+", acc_pl="+", ins_pl="+", pre_pl="+",
}

stress_patterns["f"] = {
	nom_sg="+", gen_sg="+", dat_sg="+", acc_sg="+", ins_sg="+", pre_sg="+",
	nom_pl="-", gen_pl="+", dat_pl="+", acc_pl="+", ins_pl="+", pre_pl="+",
}

stress_patterns["f'"] = {
	nom_sg="+", gen_sg="+", dat_sg="+", acc_sg="-", ins_sg="+", pre_sg="+",
	nom_pl="-", gen_pl="+", dat_pl="+", acc_pl="+", ins_pl="+", pre_pl="+",
}

stress_patterns["f''"] = {
	nom_sg="+", gen_sg="+", dat_sg="+", acc_sg="+", ins_sg="-", pre_sg="+",
	nom_pl="-", gen_pl="+", dat_pl="+", acc_pl="+", ins_pl="+", pre_pl="+",
}

ending_stressed_gen_pl_patterns = ut.list_to_set({"b", "b'", "c", "e", "f", "f'", "f''"})
ending_stressed_pre_sg_patterns = ut.list_to_set({"b", "b'", "d", "d'", "f", "f'", "f''"})
ending_stressed_dat_sg_patterns = ending_stressed_pre_sg_patterns
ending_stressed_sg_patterns = ending_stressed_pre_sg_patterns
ending_stressed_pl_patterns = ut.list_to_set({"b", "b'", "c"})

local after_titles = {
	["a"] = " (animate)",
	["i"] = " (inanimate)",
	["b"] = "",
}

local numbers = {
	["s"] = "singular",
	["p"] = "plural",
}

local form_temp = [=[{term}<br/><span style="color: #888">{tr}</span>]=]
local old_title_temp = [=[Pre-reform declension of <b lang="ru" class="Cyrl">{lemma}</b>]=]
local title_temp = [=[Declension of <b lang="ru" class="Cyrl">{lemma}</b>]=]

local partitive = nil
local locative = nil
local vocative = nil
local internal_notes_template = nil
local notes_template = nil
local templates = {}

-- cases that are declined normally instead of handled through overrides
decl_cases = {
	"nom_sg", "gen_sg", "dat_sg", "acc_sg", "ins_sg", "pre_sg",
	"nom_pl", "gen_pl", "dat_pl", "acc_pl", "ins_pl", "pre_pl",
}

-- all cases displayable or handleable through overrides
cases = {
	"nom_sg", "gen_sg", "dat_sg", "acc_sg", "ins_sg", "pre_sg",
	"nom_pl", "gen_pl", "dat_pl", "acc_pl", "ins_pl", "pre_pl",
	"par", "loc", "voc",
}

-- Convert a raw override into a canonicalized list of individual overrides.
-- If input is nil, so is output. Certain junk (e.g. <br/>) is removed,
-- and ~ and ~~ are substituted appropriately; ARGS and ISPL are required for
-- this purpose. if will still be necessary to call m_table_tools.get_notes()
-- to separate off any trailing "notes" (asterisks, superscript numbers, etc.),
-- and m_links.remove_links() to remove any links to get the raw override
-- form.
canonicalize_override = function(args, case)
	local val = args[case]
	if not val then
		return nil
	end

	-- clean <br /> that's in many multi-form entries and messes up linking
	val = rsub(val, "<br%s*/>", "")
	-- substitute ~ and ~~ and split by commas
	local stem = rfind(case, "_pl") and args.pl or args.stem
	val = rsub(val, "~~", com.make_unstressed_once(stem))
	val = rsub(val, "~", stem)
	val = rsplit(val, "%s*,%s*")

	-- handle + in loc/par meaning "the expected form"; NOTE: This requires
	-- that dat_sg has already been processed!
	if case == "loc" or case == "par" then
		local new_vals = {}
		for _, arg in ipairs(val) do
			-- don't just handle + by itself in case the arg has в or на
			-- or whatever attached to it
			if rfind(arg, "^%+") or rfind(arg, "[%s%[|]%+") then
				for _, dat in ipairs(args["dat_sg"]) do
					local subval = case == "par" and dat or com.make_ending_stressed(dat)
					-- wrap the word in brackets so it's linked; but not if it
					-- appears to already be linked
					local newarg = rsub(arg, "^%+", "[[" .. subval .. "]]")
					newarg = rsub(newarg, "([%[|])%+", "%1" .. subval)
					newarg = rsub(newarg, "(%s)%+", "%1[[" .. subval .. "]]")
					table.insert(new_vals, newarg)
				end
			else
				table.insert(new_vals, arg)
			end
		end
		val = new_vals
	end

	-- auto-accent/check for necessary accents
	local newvals = {}
	for _, v in ipairs(val) do
		if not args.allow_unaccented then
			-- it's safe to accent monosyllabic stems
			if com.is_monosyllabic(v) then
				v = com.make_ending_stressed(v)
			elseif com.needs_accents(v) then
				error("Override " .. v .. " for case " .. case .. " requires an accent")
			end
		end
		table.insert(newvals, v)
	end
	val = newvals

	return val
end

handle_forms_and_overrides = function(args)
	for _, case in ipairs(cases) do
		local ispl = rfind(case, "_pl")
		if args[case .. "_tail"] and args.forms[case] then
			local lastarg = #(args.forms[case])
			if lastarg > 0 then
				args.forms[case][lastarg] = args.forms[case][lastarg] .. args[case .. "_tail"]
			end
		end
		if not ispl and args.forms[case] then
			local lastarg = #(args.forms[case])
			if lastarg > 0 and args.sgtailall then
				args.forms[case][lastarg] = args.forms[case][lastarg] .. args.sgtailall
			end
			if lastarg > 1 and args.sgtail then
				args.forms[case][lastarg] = args.forms[case][lastarg] .. args.sgtail
			end
		end
		if ispl and args.forms[case] then
			local lastarg = #(args.forms[case])
			if lastarg > 0 and args.pltailall then
				args.forms[case][lastarg] = args.forms[case][lastarg] .. args.pltailall
			end
			if lastarg > 1 and args.pltail then
				args.forms[case][lastarg] = args.forms[case][lastarg] .. args.pltail
			end
		end
	end

	local function process_override(case)
		if args[case] then
			args[case] = canonicalize_override(args, case)
		else
			args[case] = args.forms[case]
		end
	end

	-- do dative singular first because it will be used by loc/par
	process_override("dat_sg")

	-- now do the rest
	for _, case in ipairs(cases) do
		if case ~= "dat_sg" then
			process_override(case)
		end
	end
end

-- Make the table
make_table = function(args)
	local anim = args.a
	local numb = args.n
	local old = args.old
	args.after_title = after_titles[anim]
	args.number = numbers[numb]

	args.lemma = m_links.remove_links((numb == "p") and table.concat(args.nom_pl, ", ") or table.concat(args.nom_sg, ", "))
	args.title = args.title or
		strutils.format(old and old_title_temp or title_temp, args)

	for _, case in ipairs(cases) do
		if args[case] then
			if type(args[case]) ~= "table" then
				error("Logic error, args[case] should be nil or table")
			end
			if #args[case] == 0 then
				args[case] = nil
			end
		end
	end

	if anim == "a" then
		if not args.acc_sg then
			args.acc_sg = args.gen_sg
		end
		if not args.acc_pl then
			args.acc_pl = args.gen_pl
		end
	elseif anim == "i" then
		if not args.acc_sg then
			args.acc_sg = args.nom_sg
		end
		if not args.acc_pl then
			args.acc_pl = args.nom_pl
		end
	end

	for _, case in ipairs(cases) do
		if args[case] then
			if #args[case] == 1 and args[case][1] == "-" then
				args[case] = "&mdash;"
			else
				local ru_vals = {}
				local tr_vals = {}
				for i, x in ipairs(args[case]) do
					local is_adj = rfind(x, "<adj>")
					x = rsub(x, "<adj>", "")
					local entry, notes = m_table_tools.get_notes(x)
					if old then
						ut.insert_if_not(ru_vals, m_links.full_link(com.remove_jo(entry), entry, lang, nil, nil, nil, {tr = "-"}, false) .. notes)
					else
						ut.insert_if_not(ru_vals, m_links.full_link(entry, nil, lang, nil, nil, nil, {tr = "-"}, false) .. notes)
					end
					local nolinks = m_links.remove_links(entry)
					local entrytr = is_adj and m_ru_translit.tr_adj(nolinks) or lang:transliterate(nolinks)
					ut.insert_if_not(tr_vals, entrytr .. notes)
				end
				local term = table.concat(ru_vals, ", ")
				local tr = table.concat(tr_vals, ", ")
				args[case] = strutils.format(form_temp, {["term"] = term, ["tr"] = tr})
			end
		end
	end

	local temp = nil

	if numb == "s" then
		args.nom_x = args.nom_sg
		args.gen_x = args.gen_sg
		args.dat_x = args.dat_sg
		args.acc_x = args.acc_sg
		args.ins_x = args.ins_sg
		args.pre_x = args.pre_sg
		if args.acc_sg then
			temp = "half"
		else
			temp = "half_a"
		end
	elseif numb == "p" then
		args.nom_x = args.nom_pl
		args.gen_x = args.gen_pl
		args.dat_x = args.dat_pl
		args.acc_x = args.acc_pl
		args.ins_x = args.ins_pl
		args.pre_x = args.pre_pl
		args.par = nil
		args.loc = nil
		args.voc = nil
		if args.acc_pl then
			temp = "half"
		else
			temp = "half_a"
		end
	else
		if args.acc_pl then
			temp = "full"
		elseif args.acc_sg then
			temp = "full_af"
		else
			temp = "full_a"
		end
	end

	args.par_clause = args.par and strutils.format(partitive, args) or ""
	args.loc_clause = args.loc and strutils.format(locative, args) or ""
	args.voc_clause = args.voc and strutils.format(vocative, args) or ""
	args.notes_clause = args.notes and strutils.format(notes_template, args) or ""

	args.internal_notes = table.concat(args.internal_notes, "<br />")
	args.internal_notes_clause = #args.internal_notes > 0 and strutils.format(internal_notes_template, args) or ""

	return strutils.format(templates[temp], args)
end

partitive = [===[

! style="background:#eff7ff" | partitive
| {par}
|-]===]

locative = [===[

! style="background:#eff7ff" | locative
| {loc}
|-]===]

vocative = [===[

! style="background:#eff7ff" | vocative
| {voc}
|-]===]

notes_template = [===[
<div style="width:100%;text-align:left;background:#d9ebff">
<div style="display:inline-block;text-align:left;padding-left:1em;padding-right:1em">
{notes}
</div></div>
]===]

internal_notes_template = rsub(notes_template, "notes", "internal_notes")

local function template_prelude(min_width)
	min_width = min_width or "70"
	return rsub([===[
<div>
<div class="NavFrame" style="display: inline-block; min-width: MINWIDTHem">
<div class="NavHead" style="background:#eff7ff">{title}{after_title}</div>
<div class="NavContent">
{\op}| style="background:#F9F9F9;text-align:center; min-width:MINWIDTHem" class="inflection-table"
|-
]===], "MINWIDTH", min_width)
end

local function template_postlude()
	return [===[|-{par_clause}{loc_clause}{voc_clause}
|{\cl}{internal_notes_clause}{notes_clause}</div></div></div>]===]
end

templates["full"] = template_prelude("45") .. [===[
! style="width:10em;background:#d9ebff" | 
! style="background:#d9ebff" | singular
! style="background:#d9ebff" | plural
|-
! style="background:#eff7ff" | nominative
| {nom_sg}
| {nom_pl}
|-
! style="background:#eff7ff" | genitive
| {gen_sg}
| {gen_pl}
|-
! style="background:#eff7ff" | dative
| {dat_sg}
| {dat_pl}
|-
! style="background:#eff7ff" | accusative
| {acc_sg}
| {acc_pl}
|-
! style="background:#eff7ff" | instrumental
| {ins_sg}
| {ins_pl}
|-
! style="background:#eff7ff" | prepositional
| {pre_sg}
| {pre_pl}
]===] .. template_postlude()

templates["full_a"] = template_prelude("50") .. [===[
! style="width:15em;background:#d9ebff" | 
! style="background:#d9ebff" | singular
! style="background:#d9ebff" | plural
|-
! style="background:#eff7ff" | nominative
| {nom_sg}
| {nom_pl}
|-
! style="background:#eff7ff" | genitive
| {gen_sg}
| {gen_pl}
|-
! style="background:#eff7ff" | dative
| {dat_sg}
| {dat_pl}
|-
! style="background:#eff7ff" rowspan="2" | accusative <span style="padding-left:1em;display:inline-block;vertical-align:middle">animate<br/>inanimate</span>
| {gen_sg}
| {gen_pl}
|-
| {nom_sg}
| {nom_pl}
|-
! style="background:#eff7ff" | instrumental
| {ins_sg}
| {ins_pl}
|-
! style="background:#eff7ff" | prepositional
| {pre_sg}
| {pre_pl}
]===] .. template_postlude()

templates["full_af"] = template_prelude("50") .. [===[
! style="width:15em;background:#d9ebff" | 
! style="background:#d9ebff" | singular
! style="background:#d9ebff" | plural
|-
! style="background:#eff7ff" | nominative
| {nom_sg}
| {nom_pl}
|-
! style="background:#eff7ff" | genitive
| {gen_sg}
| {gen_pl}
|-
! style="background:#eff7ff" | dative
| {dat_sg}
| {dat_pl}
|-
! style="background:#eff7ff" rowspan="2" | accusative <span style="padding-left:1em;display:inline-block;vertical-align:middle">animate<br/>inanimate</span>
| rowspan="2" | {acc_sg}
| {gen_pl}
|-
| {nom_pl}
|-
! style="background:#eff7ff" | instrumental
| {ins_sg}
| {ins_pl}
|-
! style="background:#eff7ff" | prepositional
| {pre_sg}
| {pre_pl}
]===] .. template_postlude()

templates["half"] = template_prelude("30") .. [===[
! style="width:10em;background:#d9ebff" | 
! style="background:#d9ebff" | {number}
|-
! style="background:#eff7ff" | nominative
| {nom_x}
|-
! style="background:#eff7ff" | genitive
| {gen_x}
|-
! style="background:#eff7ff" | dative
| {dat_x}
|-
! style="background:#eff7ff" | accusative
| {acc_x}
|-
! style="background:#eff7ff" | instrumental
| {ins_x}
|-
! style="background:#eff7ff" | prepositional
| {pre_x}
]===] .. template_postlude()

templates["half_a"] = template_prelude("35") .. [===[
! style="width:15em;background:#d9ebff" | 
! style="background:#d9ebff" | {number}
|-
! style="background:#eff7ff" | nominative
| {nom_x}
|-
! style="background:#eff7ff" | genitive
| {gen_x}
|-
! style="background:#eff7ff" | dative
| {dat_x}
|-
! style="background:#eff7ff" rowspan="2" | accusative <span style="padding-left:1em;display:inline-block;vertical-align:middle">animate<br/>inanimate</span>
| {gen_x}
|-
| {nom_x}
|-
! style="background:#eff7ff" | instrumental
| {ins_x}
|-
! style="background:#eff7ff" | prepositional
| {pre_x}
]===] .. template_postlude()

return export

-- For Vim, so we get 4-space tabs
-- vim: set ts=4 sw=4 noet:
