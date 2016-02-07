#!/usr/bin/env python
# -*- coding: utf-8 -*-

# FIXME (many older FIXME's missing):

# 1. (DONE) Check that headword matches page name
# 2. (DONE) Auto-accent single-syllable words that would be accented by ru-pron,
#    when comparing multiple etymologies.
# 3. (DONE) When adding a pronunciation, check if it's the same as the page name
#    (presence of ё), or is monosyllabic, in which case use an empty
#    pronunciation.
# 4. (DONE) When deleting pronunciations because monosyllabic, also delete
#    pronunciations that have ё in them and are the same as the page title.
# 5. (DONE) Preserve order of pronunciations derived from headwords (cf. бора,
#    where the headword order is бора́,бо́ра but we get the order backwards
#    because we conver to a set and then sort as a list).
# 6. (DONE) Add ann=y if more than one pronun line (NOTE: fixed below when
#    reverse-translit support added to include ann=фоо for actual headword, and
#    only add ann= if multiple headwords, not just multiple pronun variants
#    of same headword ({{ru-noun form|а́бвера|tr=ábvera, ábvɛra|m-in}})).
# 7. (DONE) Warn if there appear to be missing pronunciations when we're done
#    (fewer ru-IPA than headwords).
# 8. (DONE) Support --cats so we can do just non-lemma forms.
# 9. (DONE) Reverse-transliterate Latin to Cyrillic to generate phon= arguments
#    for noun forms etc. with transliterations. Make sure to check whether
#    the transliteration is redundant. Need to check the following to see how
#    translit with semireduction is handled: адекватностям, амнезиями,
#    амнезиях, педерастиями. The last one has annotations with translit
#    (in fact, two translits for each of two forms, four total pronuns).
#    Make sure the dot-under doesn't show in the annotation.
# 10. (DONE) Clean up 'Existing pronunciation ... different' messages to not
#    pay attention to pos=,adj=,etc. and to output different messages when
#    differences only due to gem= or dotbelow.
# 11. (DONE) Remove no-longer-used adj=.
# 12. (DONE) When splitting translit on a comma, don't do it if there's a comma
#     in the headword.
# 13. (DONE) If there are inflection-of templates on the page, look up the
#     lemma page and fetch the ru-IPA template(s) on the page and the value(s)
#     of gem= for each. (Cache these results.) (If there are multiple
#     lemmas mentioned in inflection-of templates, look up the value(s) of
#     gem= for each page and combine them all.) If multiple such values, issue
#     a warning and do do nothing. If one value representing no gem= param,
#     do nothing. Else, add the value to each of the headword-derived ru-IPA
#     templates. When comparing existing pronunciations to headword-derived
#     pronunciations, if they differ and gem= is present in headword-derived
#     pronun, and not in existing pronun, issue a warning.
# 14. Allow fixing of situation where gem= is in headword-derived
#     pronun but not existing pronun, either given a list of lemmas whose forms
#     we should fix, or perhaps a list of those forms themselves.
# 15. (DONE) If a word in base lemma ends in a double consonant cf. абсцесс,
#     аламанн/алеманн, англо-норманн: If no other double consonant, add gem=n.
#     If other double consonant or if gem= is already present, issue warning
#     and do nothing. To check for double consonant, expand the base
#     pronunciation and look for geminates other than those caused by щ or ӂӂ.
# 16. If form ends in a double consonant and gem=y or gem=opt is
#     present on the base form (cf. абелевых групп of абелева группа,
#     абсцисс of абсцисса, etc.), we need to remove this. FIXME: Need to
#     check if removing is safe, and issue warning if not. FIXME: Handle
#     multiple words (e.g. групп войск of группа войск).
# 17. (DONE, WITHOUT WARNING) Issue warning if grave accent or circumflex or ӂӂ
#     found in lemma pronun, and maybe more generally if lemma pronun doesn't
#     match headword pronun. Consider trying to copy the lemma pronun by
#     extracting out the stem.
# 18. (DONE) When creating the pronunciation of non-lemma forms, work out a
#     mapping from headword stems to pronunciation for the lemma and propagate
#     that to the non-lemma. Do this in a way that succeeds if there are
#     multiple headword stems and pronunciations, as long as we can match them
#     up. Matching up should allow for identity as well as extra grave accents,
#     circumflexes, э vs. е, зж/жж vs ӂӂ in the ru-IPA pronunciation. We
#     propagate by just trying to replace the corresponding headword stem with
#     the pronunciation (although this won't work for multiple words). This
#     will take care of many cases with translit, but we fall back on reverse
#     translit.
# 19. (DONE) Need to handle reduce/dereduce forms when mapping headword stems
#     to pronunciation.
# 20. (DONE) Need to handle adjectival nouns when mapping headword stems to
#     pronun, cf. несовершѐнноле́тний. Probably need to have some way of
#     indicating multiple possible stems in the stem->pronun mapping.
# 21. (DONE) Add dot-under to both Cyrillic and translit so it's present in
#     case we use the Cyrillic with propagated headword->pronun mapping rather
#     than reverse-transliterating, and remove the dot-under from the
#     annotation.
# 22. (DONE) Support manual_pronun_mapping for cases where stem->pronun mapping
#     fails.
# 23. In reverse transliteration, check against original when -vo or -vó is
#     found, and convert to -го or -го́ if appropriate.

# WORDS NEEDING SPECIAL HANDLING IN PRONUN:
#
# бессо́нница: First is geminated, second is not, needs respelling бессо́ница,
#    etc.
# расстаться, расставаться: Needs с(с) because first gemination is optional,
#    second isn't.
# колба Эрленмейера: should have two pronuns, first gem=n, second gem=y,
#    because (j) is optional when gem=n. Don't use gem=opt.

import pywikibot, re, sys, codecs, argparse
import difflib
import unicodedata
from collections import Counter

import blib
from blib import getparam, rmparam, msg, site

import rulib as ru
from rulib import AC, GR, CFLEX, DOTBELOW
import ru_reverse_translit

vowel_list = u"aeiouyɛəäëöü"
ipa_vowel_list = vowel_list + u"ɐɪʊɨæɵʉ"
ipa_vowels_re = "[" + ipa_vowel_list + "]"
ipa_vowels_c = "([" + ipa_vowel_list + "])"
non_ipa_vowels_re = "[^ " + ipa_vowel_list + "]"
non_ipa_vowels_non_accent_re = u"[^ ˈˌ" + ipa_vowel_list + "]"

# Other possibilities are special-cased, e.g. sn/zn/dn/tn, st/zd
cons_assim_palatal = {
  'compulsory':set([u'nt͡ɕ', u'nɕ', u'ntʲ', u'ndʲ', u'xkʲ',
    u't͡ssʲ', u'd͡zzʲ']),
  'optional':set([u'slʲ', u'zlʲ', u'nsʲ', u'nzʲ',
    u'mpʲ', u'mbʲ', u'mfʲ', u'fmʲ'])
}

# Original comment from Lua:
# [words which will be treated as accentless (i.e. their vowels will be
# reduced), and which will liaise with a preceding or following word;
# this will not happen if the words have an accent mark, cf.
# по́ небу vs. по не́бу, etc.]
# We use these lists to determine whether to auto-accent monosyllabic words.
accentless = {
  # class 'pre': particles that join with a following word
  'pre':set([u'без', u'близ', u'в', u'во', u'да', u'до',
    u'за', u'из', u'из-под', u'из-за', u'изо', u'к', u'ко', u'меж',
    u'на', u'над', u'надо', u'не', u'ни', u'об', u'обо', u'от', u'ото',
    u'перед', u'передо', u'по', u'под', u'подо', u'пред', u'предо', u'при', u'про',
    u'с', u'со', u'у', u'через']),
  # class 'prespace': particles that join with a following word, but only
  #   if a space (not a hyphen) separates them; hyphens are used here
  #   to spell out letters, e.g. а-эн-бэ́ for АНБ (NSA = National Security
  #   Agency) or о-а-э́ for ОАЭ (UAE = United Arab Emirates)
  'prespace':set([u'а', u'о']),
  # class 'post': particles that join with a preceding word
  'post':set([u'бы', u'б', u'ж', u'же', u'ли', u'либо', u'ль', u'ка',
    u'нибудь', u'тка']),
  # class 'posthyphen': particles that join with a preceding word, but only
  #   if a hyphen (not a space) separates them
  'posthyphen':set([u'то']),
}

fronting = {
  'a': u'æ',
  u'u': u'ʉ',
  u'ʊ': u'ʉ',
}

skip_pages = [
    u"г-жа",
    u"е",
    u"и",
    u"ы",
    u"я"
]

applied_manual_pronun_mappings = set()

# Used when the automatic headword->pronun mapping fails (typically, where
# there's a secondary stress and either multiword phrases or accent type
# c/d/e/f or accent type b with masculine nouns). Each tuple is of the form
# (HEADWORD, SUB) where HEADWORD is a regex and SUB is either a single string
# to substitute in the regex or a list of such strings.
manual_pronun_mapping = [
    (u"^аминокисло́т", u"амѝнокисло́т"),
    (u"^ампер-час", u"ампѐр-час"),
    (u"^антител", u"а̀нтител"),
    (u"^аэронавигацио́нн(.*?) ог", ur"а̀эронавигацио́нн\1 ог"),
    # override pronunciation бох
    (u"^(бо́?г)", ur"\1"),
    (u"^бронекатер", u"бро̀некатер"),
    (u"^бухга́лтерск(.*?) кни́г", [
      ur"phon=буга́лтерск\1 кни́г",
      ur"буɣа́лтерск\1 кни́г",
      ur"бухга́лтерск\1 кни́г"]),
    (u"^видеои́гр", u"вѝдеои́гр"),
    (u"^госдепартамент", u"го̀сдепартамент"),
    (u"^госсекретар", u"го̀ссекретар"),
    (u"^го́спод", [u"го́спод", u"ɣо́спод"]),
    (u"^Го́спод", [u"Го́спод", u"ɣо́спод"]),
    (u"^дезоксирибонуклеи́нов(.*?) кисл", ur"дезоксирѝбонуклеи́нов\1 кисл"),
    # override pronunciation дощ
    (u"^(до́?жд)", ur"\1"),
    (u"^домохозя́", u"до̀мохозя́"),
    (u"^дрожж", [u"дроӂӂ", u"дрожж"]),
    (u"^заво́д(.*?)-подря́дчик", ur"заво̀д\1-подря́дчик"),
    # reverse-translit would produce ёркширский тэрье́р etc.
    (u"^(йо́ркширск.*?) терье́р", ur"phon=\1 тэрье́р"),
    (u"^квартирохозя́", u"квартѝрохозя́"),
    # override pronunciation кислотный дощ
    (u"^(кисло́тн.* до́?жд)", ur"\1"),
    (u"^кни́г(.*?) за семью́ печа́тям", ur"кни́г\1 за семью́ печа́тя̣м"),
    (u"^лесополо́с", u"лѐсополо́с"),
    (u"^льносем", u"льно̀сем"),
    (u"^лю́к(.*?) фотопулемёт", ur"лю́к\1 фо̀топулемёт"),
    (u"^мундштук", u"phon=мунштук"),
    (u"^несча́стий$", u"неща́стий"),
    (u"^обезьяно([лч])", ur"обезья̀но\1"),
    (u"^(пере́дн.*?) бронеперегоро́д", ur"\1 бро̀неперегоро́д"),
    (u"^подна́йм", u"по̀дна́йм"),
    (u"^полу(в[её]д)", ur"по̀лу\1"),
    (u"^полу(ве́?к)", ur"по̀лу\1"),
    (u"^полу(го́?д)", ur"по̀лу\1"),
    (u"^полу(им)", ur"по̀лу\1"),
    (u"^полу(килом)", ur"по̀лу\1"),
    (u"^полу(ли́тр)", ur"по̀лу\1"),
    (u"^полу(лю́?д|челов)", ur"по̀лу\1"),
    (u"^полу(ме́сяц)", ur"по̀лу\1"),
    (u"^полу(ме́тр)", ur"по̀лу\1"),
    (u"^полу(миллио́н)", ur"по̀лу\1"),
    (u"^полу(ос)", ur"по̀лу\1"),
    (u"^полу(остров)", [ur"по̀лу\1", ur"полу\1"]),
    (u"^полу(очк)", ur"по̀лу\1"),
    (u"^полу(со́тн)", ur"по̀лу\1"),
    (u"^полу(ты́сяч)", ur"по̀лу\1"),
    (u"^полу(ча́?с)", ur"по̀лу\1"),
    (u"^посттравмат", u"по̀сттравмат"),
    (u"^пра(воохрани́тельн.*? о́рган)", ur"пра̀\1"),
    (u"^пра(материк)", ur"пра̀\1"),
    (u"^пресс(-секретар)", ur"прѐсс\1"),
    (u"^про́волок", u"про́вол(о)к"),
    (u"^прое́зж(.*? ча́?ст)", [ur"прое́зж\1", ur"прое́ӂӂ\1"]),
    (u"^соцсет", u"со̀цсет"),
    (u"^(су́?д.*? на подво́дных )кры́льях", ur"\1кры́лья̣х"),
    (u"^тео́ри(.*?) ха́оса", ur"тео́ри\1 ха́о̂са"),
    (u"^трёх(эта́жн.*? сло́?в)", ur"трё̀х\1"),
    (u"^четырёх(та́кт.*? дви́гател)", ur"четырё̀х\1"),
    (u"^четверг", [u"четверг", "phon=четверьг"]),
]

# Make sure there are two trailing newlines
def ensure_two_trailing_nl(text):
  return re.sub(r"\n*$", r"\n\n", text)

def contains_latin(text):
  return re.search(u"[0-9a-zščžáéíóúýàèìòùỳɛě]", text.lower())

def contains_non_cyrillic_non_latin(text):
  # 0300 = grave, 0301 = acute, 0302 = circumflex, 0308 = diaeresis,
  # 0307 = dot-above, 0323 = dot-below
  # We also include basic punctuation as well as IPA chars ɣ ɕ ʑ, which
  # we allow in Cyrillic pronunciation; FIXME: We allow Latin h as a substitute
  # for ɣ, we should allow it here and not have it trigger contains_latin()
  # by itself
  return re.sub(ur"[\u0300\u0301\u0302\u0308\u0307\u0323 \-,.?!ɣɕʑЀ-ԧꚀ-ꚗa-zščžáéíóúýàèìòùỳɛě]", "", text.lower()) != ""

def ipa_matches(headword, manual, auto, ipa_templates_msg, pagemsg):
  orig_auto = auto
  orig_manual = manual
  manual = re.sub(r"[\[\]/.]", "", manual)
  if auto == manual:
    pagemsg("For headword %s, %s equal to %s" % (headword,
      "auto %s" % auto if auto == orig_auto else
        "canon auto %s (orig %s)" % (auto, orig_auto),
      "manual" if manual == orig_manual else
        "canon manual (orig %s)" % (orig_manual)))
    return True

  manual = re.sub(u"ᵻ", u"ɨ", manual)
  # Get rid of raising/lowering diacritics
  manual = re.sub(u"[\u031d\u031e]", u"", manual)
  manual = re.sub(u"ʲɛ", u"ʲe", manual)
  manual = re.sub(u"[ɘɞ]", u"ə", manual)
  # expand long vowels; need to do this before moving stress to beginning
  # of consonant clusters, below, as ː is treated as a consonant
  manual = re.sub(u"[ɐə]ː", u"ɐɐ", manual)
  manual = re.sub(u"ɪː", u"ɪɪ", manual)
  manual = re.sub(u"ɑ", "a", manual)
  manual = re.sub(u"ɔ", "o", manual)
  manual = re.sub(u"ɾ", "r", manual)
  manual = re.sub(u"χ", "x", manual)
  manual = re.sub(ur"\(ʲ\)", u"⁽ʲ⁾", manual)
  manual = re.sub(u"ʌ", u"ɐ", manual)
  manual = re.sub(u"'", u"ˈ", manual)
  manual = re.sub(u"ˈˈ", u"ˈ", manual)
  manual = re.sub(u"ɫ", "l", manual)
  manual = re.sub(ur"ʈʂ", u"t͡ʂʂ", manual)
  manual = re.sub(u"t͡ʃ|tʃ", u"t͡ɕ", manual)
  manual = re.sub(u"ʃ", u"ʂ", manual)
  # Convert regular g to IPA ɡ (looks same but different char)
  manual = re.sub("g", u"ɡ", manual)
  # Both ɡ's below are IPA ɡ's
  manual = re.sub(u"ŋɡ", u"nɡ", manual)
  manual = re.sub(u"st([ln])", r"s\1", manual)
  # Canonicalize spaces and hyphens in manual
  manual = re.sub(r"^[\s\-]+", "", manual)
  manual = re.sub(r"[\s\-]+$", "", manual)
  manual = re.sub(r"[\s\-]+", " ", manual)

  autowords = re.split(" ", auto)
  manwords = re.split(" ", manual)
  hwords = re.split(r"[\s\-]", headword)
  if len(autowords) != len(manwords):
    return "WARNING: For headword %s, auto %s%s not same as manual %s%s: different number of words (auto %s vs manual %s): %s" % (
      headword, auto, orig_auto != auto and " (%s)" % orig_auto or "",
      manual, orig_manual != manual and " (%s)" % orig_manual or "",
      len(autowords), len(manwords), ipa_templates_msg)
  if len(hwords) != len(autowords):
    pagemsg("WARNING: Number of words in headword %s not same as in auto %s" % (
      headword, auto))
    hwords = [""]*len(autowords)
  for j in xrange(len(autowords)):
    autoword = autowords[j]
    manword = manwords[j]
    hword = hwords[j]
    auto_monosyllabic = len(re.sub(non_ipa_vowels_re, "", autoword)) == 1
    man_monosyllabic = len(re.sub(non_ipa_vowels_re, "", manword)) == 1

    # If both auto and manual are monosyllabic, canonicalize by
    # removing primary accent
    if auto_monosyllabic and man_monosyllabic:
      autoword = re.sub(u"ˈ", "", autoword)
      manword = re.sub(u"ˈ", "", manword)

    # Convert some instances of ˈ (earlier converted from to ') to ʲ --
    # after a palatalizable consonant, before a vowel or end of word;
    # ɡ is IPA ɡ; do this before moving stress to beginning of cons clusters
    manword = re.sub(ur"([dtbpkɡszfvxrlmn])ˈ(ː?)($|" + ipa_vowels_re + ")",
        ur"\1ʲ\2\3", manword)

    # Canonicalize by moving stress at the beginning of all consonant
    # clusters
    autoword = re.sub("(" + non_ipa_vowels_re + u"+)([ˈˌ])", r"\2\1", autoword)
    manword = re.sub("(" + non_ipa_vowels_re + u"+)([ˈˌ])", r"\2\1", manword)

    # т(ь)ся and related fixes
    manual = re.sub(u"nt͡sk", u"n(t)sk", manual)
    manual = re.sub(u"ntsk", u"n(t)sk", manual)
    manual = re.sub(u"tts", u"t͡sː", manual)
    manword = re.sub(u"tt͡s", u"t͡sː", manword)
    manword = re.sub(u"tːs", u"t͡sː", manword)
    manword = re.sub(u"tʲ?t͡ɕ", u"t͡ɕː", manword)
    manword = re.sub(u"tːɕ", u"t͡ɕː", manword)
    # with -т(ь)ся after stressed syllable, need "t͡sː"; after unstressed,
    # need just t͡s
    if re.search(u"\u0301ть?ся$", hword):
      manword = re.sub(u"(" + ipa_vowels_re + u")(ts|t͡s)ə$", ur"\1t͡sːə", manword)
    elif re.search(u"ть?ся$", hword):
      manword = re.sub(u"(" + ipa_vowels_re + u")(ts|t͡s)ːə$", ur"\1t͡sə", manword)

    # ɐ vs. ə fixes; ɐ at beginning of word or directly before the stress
    # or in ɐɐ sequences, else ə
    manword = re.sub(u"ɐ", u"ə", manword)
    manword = re.sub(u"^ə", u"ɐ", manword)
    manword = re.sub(u"[ɐə][ɐə]", u"ɐɐ", manword)
    # need to do this after moving stress to beginning of consonant clusters
    manword = re.sub(u"əˈ", u"ɐˈ", manword)

    # Fix bug in auto (FIXME, remove this)
    autoword = re.sub(u"ɕ(ː?)ʲə", ur"ɕ\1ə", autoword)

    # palatalization and gemination need to be in the right order
    # Fix bug in auto
    autoword = re.sub(u"ːʲ", u"ʲː", autoword)
    manword = re.sub(u"ːʲ", u"ʲː", manword)

    # front vowel variants; here we first convert existing front variants
    # to back vowels, then eventually we apply the exact same fronting
    # logic as in ru-pron.lua.
    manword = re.sub(u"æ", u"a", manword)
    # u will get converted below to ʊ in unstressed syllables
    manword = re.sub(u"ʉ", u"u", manword)
    # Much simpler for o
    manword = re.sub(u"([ʲjɕ]ː?)o", ur"\1ɵ", manword)

    # i vs. ɪ fixes: i when stressed, ɪ otherwise; same for u vs. ʊ
    if not man_monosyllabic and re.search(u"ˈ", manword):
      # Convert all i to ɪ, then back to i in stressed syllables
      manword = re.sub(u"i", u"ɪ", manword)
      manword = re.sub(u"([ˈˌ]" + non_ipa_vowels_re + u"*)ɪ", r"\1i", manword)
      # Convert all u to ʊ, then back to u in stressed syllables
      manword = re.sub(u"u", u"ʊ", manword)
      manword = re.sub(u"([ˈˌ]" + non_ipa_vowels_re + u"*)ʊ", r"\1u", manword)
    # If monosyllabic, i and u unless word is accentless
    if man_monosyllabic and headword not in (
        [u"без", u"близ", u"из", u"меж", u"пред", u"при", u"не", u"ли"]):
      manword = re.sub(u"ɪ", u"i", manword)
    if man_monosyllabic and headword != u"у":
      manword = re.sub(u"ʊ", u"u", manword)

    # ɕ that's not geminate and not in t͡ɕ (with or without tie bar)
    # needs to be geminated (0361 = tie bar)
    manword = re.sub(u"([^t\u0361])ɕ($|[^ː])", ur"\1ɕː\2", manword)

    # Convert nː in n(ː) in various endings in n(ː) in auto
    if re.search(u"n\(ː\)", autoword):
      manword = re.sub(u"nː(ɨj|əsʲtʲ|əjə|ə)$", ur"n(ː)\1", manword)

    # Convert sʲə to s⁽ʲ⁾ə at end of word if necessary
    if re.search(u"s⁽ʲ⁾ə$", autoword):
      manword = re.sub(u"sʲə$", u"s⁽ʲ⁾ə", manword)

    # if affricate assimilation in autoword, make it same in manword
    # do before adding tie bar in ts
    if re.search(u"d͡zz", autoword):
      manword = re.sub(u"dz", u"d͡zz", manword)
    if re.search(u"t͡ss", autoword):
      manword = re.sub(u"ts", u"t͡ss", manword)

    # add tie bar if needed
    if re.search(u"t͡s", autoword):
      manword = re.sub(u"ts", u"t͡s", manword)
    if re.search(u"t͡ɕ", autoword):
      manword = re.sub(u"tɕ", u"t͡ɕ", manword)

    # palatalization needed before front vowels; note, ɡ is IPA ɡ
    # we don't do this before e because the lack of palatalization might
    # be legitimate (although should be written with ɛ)
    manword = re.sub(ur"([dtbpkɡszfvxrlmn])(ː?[ɪiɵæʉ])", ur"\1ʲ\2", manword)

    # Apply optional and compulsory palatal assimilation to manword
    def apply_tn_dn_assim_palatal(m):
      a, b, c = m.groups()
      if a == '':
        return a + b + u'ʲ' + c
      else:
        return a + b + u'⁽ʲ⁾' + c

    # Optional (j) before ɪ
    manword = re.sub(u"(^| )jɪ", ur"\1(j)ɪ", manword)
    manword = re.sub(ipa_vowels_c + u"([‿-]?)jɪ", ur"\1\2(j)ɪ", manword)

    # consonant assimilative palatalisation of tn/dn/sn/zn, depending on
    # whether [rl] precedes
    manword = re.sub(u"([rl]?)([ˈˌ]?[dtsz])ʲ?([ˈˌ]?nʲ)",
        apply_tn_dn_assim_palatal, manword)

    # consonant assimilative palatalisation of st/zd, depending on
    # whether [rl] precedes
    manword = re.sub(u"([rl]?)([ˈˌ]?[sz])ʲ?([ˈˌ]?[td]ʲ)",
        apply_tn_dn_assim_palatal, manword)

    def apply_assim_palatal(m):
      a, b, c = m.groups()
      if a + c in cons_assim_palatal['compulsory']:
        return a + u'ʲ' + b + c
      elif a + c in cons_assim_palatal['optional']:
        return a + u'⁽ʲ⁾' + b + c
      else:
        return m.group(0)

    #apply general consonant assimilative palatalisation, repeatedly for
    #recursive assimilation
    while True:
      new_manword = re.sub(u'(t͡s|d͡z|[xszntdmbpf])ʲ?([ˈˌ]?)(t͡ɕ|[ktdǰɕlnszmpbf]ʲ)',
        apply_assim_palatal, manword)
      if new_manword == manword:
        break
      manword = new_manword

    # optional palatal assimilation of вп, вб only word-initially
    manword = re.sub(u'^([ˈˌ]?[fv])ʲ?([ˈˌ]?[pb]ʲ)', ur'\1⁽ʲ⁾\2', manword)

    # END OF PER-WORD PROCESSING
    autowords[j] = autoword
    manwords[j] = manword

  auto = " ".join(autowords)
  manual = " ".join(manwords)

  # Following code is copied from ru-pron.lua (and converted to Python).
  #
  # Front a and u between soft consonants. If between a soft and
  # optionally soft consonant (should only occur in that order, shouldn't
  # ever have a or u preceded by optionally soft consonant),
  # split the result into two. We only split into two even if there
  # happen to be multiple optionally fronted a's and u's to avoid
  # excessive numbers of possibilities (and it simplifies the code).
  # 1. First, temporarily add soft symbol to inherently soft consonants.
  manual = re.sub(u"([čǰɕӂj])", ur"\1ʲ", manual)
  # 2. Handle case of [au] between two soft consonants
  manual = re.sub(u"(ʲ[ː()]*)([auʊ])([ˈˌ]?.ʲ)",
      lambda m:m.group(1) + fronting[m.group(2)] + m.group(3),
      manual)
  # 3. Handle [au] between soft consonant and optional j, which is still fronted
  manual = re.sub(ur"(ʲ[ː()]*)([auʊ])([ˈˌ]?\(jʲ\))",
      lambda m:m.group(1) + fronting[m.group(2)] + m.group(3),
      manual)
  # 4. Handle case of [au] between soft and optionally soft consonant
  if re.search(u"ʲ[ː()]*[auʊ][ˈˌ]?.⁽ʲ⁾", manual) or re.search(ur"ʲ[ː()]*[auʊ][ˈˌ]?\(jʲ\)", manual):
    opt_hard = re.sub(u"(ʲ[ː()]*)([auʊ])([ˈˌ]?.)⁽ʲ⁾", r"\1\2\3", manual)
    opt_soft = re.sub(u"(ʲ[ː()]*)([auʊ])([ˈˌ]?.)⁽ʲ⁾",
      lambda m:m.group(1) + fronting[m.group(2)] + m.group(3) + u"ʲ",
      manual)
    manual = opt_hard + ", " + opt_soft
  # 5. Undo addition of soft symbol to inherently soft consonants.
  manual = re.sub(u"([čǰɕӂj])ʲ", r"\1", manual)

  if auto == manual:
    pagemsg("For headword %s, %s equal to %s" % (headword,
      "auto %s" % auto if auto == orig_auto else
        "canon auto %s (orig %s)" % (auto, orig_auto),
      "manual" if manual == orig_manual else
        "canon manual (orig %s)" % (orig_manual)))
    return True

  if u"ˈ" not in manual and re.sub(u"ˈ", "", auto) == manual:
    pagemsg("WARNING: For headword %s, missing stress mark in %s compared to %s, accepting" % (headword,
      "manual" if manual == orig_manual else
        "canon manual (orig %s)" % (orig_manual),
      "auto %s" % auto if auto == orig_auto else
        "canon auto %s (orig %s)" % (auto, orig_auto)))
    return True

  seqmatch = difflib.SequenceMatcher(None, auto, manual)
  changes = []
  for tag, i1, i2, j1, j2 in seqmatch.get_opcodes():
    if tag == "delete":
      changes.append("delete %s at %s" % (auto[i1:i2], i1))
    elif tag == "replace":
      changes.append("replace %s -> %s at %s" % (auto[i1:i2], manual[j1:j2],
        i1))
    elif tag == "insert":
      changes.append("insert %s at %s" % (manual[j1:j2], i1))
  return "WARNING: For headword %s, auto %s%s not same as manual %s%s: %s: %s" % (
    headword, auto, orig_auto != auto and " (%s)" % orig_auto or "",
    manual, orig_manual != manual and " (%s)" % orig_manual or "",
    ", ".join(changes), ipa_templates_msg)

def canonicalize_monosyllabic_pronun(pronun, tr):
  # Do nothing if there are multiple words
  if pronun not in accentless['pre'] and not re.search(r"[\s\-]", pronun):
    return ru.try_to_stress(pronun), ru.try_to_stress(tr)
  else:
    return pronun, tr

def remove_list_duplicates(l):
  newl = []
  for x in l:
    if x not in newl:
      newl.append(x)
  return newl

def printable_ru_tr(cyr, tr):
  if tr:
    return "%s//%s" % (cyr, tr)
  else:
    return cyr

def printable_ru_tr_list(values):
  return ",".join(printable_ru_tr(cyr, tr) for cyr, tr in values)

def ru_tr_as_pronun(cyr, tr):
  if tr:
    return "phon=%s" % ru_reverse_translit.reverse_translit(tr)
  else:
    return cyr

# Get a list of headword pronuns, a list of (HEAD, TRANSLIT) tuples.
def get_headword_pronuns(parsed, pagetitle, pagemsg, expand_text):
  # Get the headword pronunciation(s)
  headword_pronuns = []

  # Append headword to headword_pronuns, possibly with translit.
  # If translit is present, split on commas to handle cases like
  # {{ru-noun form|а́бвера|tr=ábvera, ábvɛra|m-in}}. Don't split regular
  # headwords on commas because sometimes they legitimately have commas
  # in them (idioms, phrases, etc.). When we have translit, check each
  # value against the headword to see if the translit is redundant
  # (e.g. as in {{ru-noun form|а́бвера|tr=ábvera, ábvɛra|m-in}}, where the
  # first is redundant).
  def append_headword(head, tr, trparam):
    if not tr:
      headword_pronuns.append((head, ""))
    else:
      pagemsg("WARNING: Using Latin for pronunciation, based on %s%s" %
          (trparam, tr))
      tr = ru.decompose(tr)
      # Split on commas, as described above; but don't do it if there's
      # a comma in the headword, e.g. in 'из-за того, что' (translit no
      # longer needed but formerly present).
      if "," in head:
        translits = [tr]
      else:
        translits = re.split(r"\s*,\s*", tr)
      for trval in translits:
        autotranslit = expand_text("{{xlit|ru|%s}}" % head)
        # Just in case, normalize both when comparing; not generally
        # necessary because we called ru.decompose() above
        if unicodedata.normalize("NFC", autotranslit) == unicodedata.normalize("NFC", trval):
          pagemsg("Ignoring redundant translit %s%s for headword %s" %
              (trparam, trval, head))
          headword_pronuns.append((head, ""))
        else:
          headword_pronuns.append((head, trval))

  for t in parsed.filter_templates():
    check_extra_heads = False
    tname = unicode(t.name)
    if tname == u"ru-noun-alt-ё":
      pagemsg("WARNING: Found %s template, skipping" % tname)
      return None
    elif tname in ["ru-noun", "ru-proper noun", "ru-adj", "ru-adv", "ru-verb"]:
      head = getparam(t, "1") or pagetitle
      tr = getparam(t, "tr")
      append_headword(head, tr, "tr=")
      check_extra_heads = True
    elif tname in ["ru-noun form", "ru-phrase"]:
      head = getparam(t, "head") or getparam(t, "1") or pagetitle
      tr = getparam(t, "tr")
      append_headword(head, tr, "tr=")
      check_extra_heads = True
    elif tname == "head" and getparam(t, "1") == "ru" and getparam(t, "2") == "letter":
      pagemsg("WARNING: Skipping page with letter headword")
      return None
    elif tname == "head" and getparam(t, "1") == "ru":
      head = getparam(t, "head") or pagetitle
      tr = getparam(t, "tr")
      append_headword(head, tr, "tr=")
      check_extra_heads = True
    elif tname in ["ru-noun+", "ru-proper noun+"]:
      if tname == "ru-noun+":
        generate_template = re.sub(r"^\{\{ru-noun\+",
            "{{ru-generate-noun-forms", unicode(t))
      else:
        generate_template = re.sub(r"^\{\{ru-proper noun\+",
            "{{ru-generate-noun-forms|ndef=sg", unicode(t))
      generate_result = expand_text(generate_template)
      if not generate_result:
        pagemsg("WARNING: Error generating noun forms")
        return None
      args = {}
      for arg in re.split(r"\|", generate_result):
        name, value = re.split("=", arg)
        args[name] = re.sub("<!>", "|", value)
      lemma = args["nom_sg"] if "nom_sg" in args else args["nom_pl"]
      for head in re.split(",", lemma):
        tr = None
        if "//" in head:
          head, tr = re.split("//", head)
        append_headword(head, tr, "translit ")

    if check_extra_heads:
      for i in xrange(2, 10):
        headn = getparam(t, "head" + str(i))
        trn = getparam(t, "tr" + str(i))
        if headn:
          append_headword(headn, trn, "tr%s=" % str(i))

  # Do the following two sections before adding semi-reduced inflection
  # since ru.* may not be aware of dot-under.

  # Canonicalize by removing links and final !, ?
  headword_pronuns = [(re.sub("[!?]$", "", blib.remove_links(x)), re.sub("[!?]$", "", blib.remove_links(tr))) for x, tr in headword_pronuns]
  for pronun, translit in headword_pronuns:
    if ru.remove_accents(pronun) != pagetitle:
      pagemsg("WARNING: Headword pronun %s doesn't match page title, skipping" % pronun)
      return None

  # Check for acronym/non-syllabic.
  for pronun, translit in headword_pronuns:
    if ru.is_nonsyllabic(pronun):
      pagemsg("WARNING: Pronunciation is non-syllabic, skipping: %s" % pronun)
      return None
    if re.search("[" + ru.uppercase + u"ЀЍ][" + ru.AC + ru.GR + "]?[" + ru.uppercase + u"ЀЍ]", pronun):
      pagemsg("WARNING: Pronunciation may be an acronym, please check: %s" % pronun)

  # Check for the need for semi-reduced inflection of я in 3rd plural -ят
  # or dat/ins/pre plural -ям, -ями, -ях.
  found_semireduced_inflection = False
  for t in parsed.filter_templates():
    tname = unicode(t.name)
    if tname == "inflection of" and getparam(t, "lang") == "ru":
      numparams = []
      for param in t.params:
        pname = unicode(param.name)
        if re.search(r"^[0-9]+$", pname):
          pnum = int(pname)
          if pnum >= 3:
            numparams.append(unicode(param.value))
        if "3" in numparams and ("p" in numparams or "pl" in numparams or "plural" in numparams):
          found_semireduced_inflection = True
        elif (("p" in numparams or "pl" in numparams or "plural" in numparams) and (
          "dat" in numparams or "dative" in numparams or
          "ins" in numparams or "instrumental" in numparams or
          "pre" in numparams or "prep" in numparams or "prepositional" in numparams)):
          found_semireduced_inflection = True
  if found_semireduced_inflection:
    def update_semireduced(pron, tr):
      if tr:
        tr = re.sub("([" + ru.translit_vowel + "][^" + ru.translit_vowel + " -]" + u"*(?:ja|ča))(t|tsja|m|mi|x)( |$)", r"\1" + DOTBELOW + r"\2\3", tr)
      pron = re.sub("([" + ru.vowel + "][^" + ru.vowel + " -]" + u"*(?:я|[щч]а))(т|тся|м|ми|х)( |$)", r"\1" + DOTBELOW + r"\2\3", pron)
      return pron, tr
    new_headword_pronuns = [update_semireduced(pron, tr) for pron, tr in headword_pronuns]
    if new_headword_pronuns != headword_pronuns:
      pagemsg("Using semi-reduced pronunciation: %s" % printable_ru_tr_list(new_headword_pronuns))
      headword_pronuns = new_headword_pronuns

  # Canonicalize headword pronuns. If a single monosyllabic word, add accent
  # unless it's in the list of unaccented words.
  headword_pronuns = [canonicalize_monosyllabic_pronun(x, tr) for x, tr in headword_pronuns]

  # Also, if two pronuns differ only in that one has an additional accent on a
  # word, remove the one without the accent.

  def headwords_same_but_first_maybe_lacks_accents(h1, h2):
    if ru.remove_accents(h1) == ru.remove_accents(h2) and len(h1) < len(h2):
      h1words = re.split(r"([\s\-]+)", h1)
      h2words = re.split(r"([\s\-]+)", h2)
      if len(h1words) == len(h2words):
        for i in xrange(len(h1words)):
          if not (h1words[i] == h2words[i] or ru.is_unaccented(h1words[i]) and ru.remove_accents(h2words[i]) == h1words[i]):
            return False
      return True
    return False
  def headword_should_be_removed_due_to_unaccent(hword, hwords):
    hwordru, hwordtr = hword
    for h in hwords:
      if hword != h:
        hru, htr = h
        if (headwords_same_but_first_maybe_lacks_accents(hwordru, hru) and
            headwords_same_but_first_maybe_lacks_accents(hwordtr, htr)):
          pagemsg("Removing headword %s because same as headword %s but lacking an accent" % (
            printable_ru_tr(hwordru, hwordtr), printable_ru_tr(hru, htr)))
          return True
    return False
  headword_pronuns = remove_list_duplicates(headword_pronuns)
  new_headword_pronuns = [x for x in headword_pronuns if not
      headword_should_be_removed_due_to_unaccent(x, headword_pronuns)]
  if len(new_headword_pronuns) <= len(headword_pronuns) - 2:
    pagemsg("WARNING: Removed two or more headword pronuns, check that something didn't go wrong: old=%s, new=%s" % (
      printable_ru_tr_list(headword_pronuns), printable_ru_tr_list(new_headword_pronuns)))
  headword_pronuns = new_headword_pronuns

  if len(headword_pronuns) < 1:
    pagemsg("WARNING: Can't find headword template")
    return None
  headword_pronuns = remove_list_duplicates(headword_pronuns)
  return headword_pronuns

def pronun_matches(hpron, foundpron, pagemsg):
  orighpron = hpron
  origfoundpron = foundpron
  foundpron = re.sub("^phon=", "", foundpron)
  if hpron == foundpron or not foundpron:
    return True
  foundpron = ru.remove_grave_accents(foundpron.replace(CFLEX, ""))
  if hpron == foundpron:
    pagemsg("Matching headword pronun %s to found pronun %s after removing circumflex and grave accents from the latter" %
      (orighpron, origfoundpron))
    return True
  foundpron = foundpron.replace(u"э", u"е")
  hpron = hpron.replace(u"э", u"е")
  if hpron == foundpron:
    pagemsg(u"Matching headword pronun %s to found pronun %s after converting э to е (and removing circumflex and grave accents)" %
      (orighpron, origfoundpron))
    return True
  foundpron = foundpron.lower()
  hpron = hpron.lower()
  if hpron == foundpron:
    pagemsg(u"Matching headword pronun %s to found pronun %s after lowercasing (and converting э to е and removing circumflex and grave accents)" %
      (orighpron, origfoundpron))
    return True
  hpron = hpron.replace(u"зж", u"ӂӂ")
  hpron = hpron.replace(u"жж", u"ӂӂ")
  if hpron == foundpron:
    pagemsg(u"Matching headword pronun %s to found pronun %s after converting зж and жж to ӂӂ (and lowercasing, converting э to е and removing circumflex and grave accents)" %
      (orighpron, origfoundpron))
    return True

  return False

# Match up the stems of headword pronunciations and found pronunciations.
# If able to do so, return a dictionary of all non-identity matchings, else
# return None. For each headword in the dictionary, the entry is a list of
# tuples of (STEM, FOUNDPRONSTEMS) where STEM is a possible stem of that
# headword and FOUNDPRONSTEMS is a list of the corresponding
# found-pronunciation stems. We return a list of stem tuples because
# there may be multiple stems to consider for each headword -- including
# reduced and dereduced variants (e.g. for автозапра́вка with corresponding
# pronunciation а̀втозапра́вка we need to consider both the regular stem
# автозапарвк- with stemmed pronunciation а̀втозапра́вк- and dereduced stem
# автозапра́вок- with stemmed pronunciation а̀втозапра́вок- in order to handle
# genitive plural автозаправок, or for амстерда́мец with pronun амстэрда́мец
# most forms require reduced амстерда́мц -> амстэрда́мц) and adjectival variants
# (e.g. for несовершенноле́тний with pronunciation несовершѐнноле́тний we need
# to consider the stem несовершенноле́тн-). FOUNDPRONSTEMS is a list because
# there may be multiple such pronunciations per headword stem, e.g. а́бвер
# has two corresponding pronunciations а́бвер and а́бвэр.
def match_headword_and_found_pronuns(headword_pronuns, found_pronuns, pagemsg,
    expand_text):
  matches = {}
  if not headword_pronuns:
    pagemsg("WARNING: No headword pronuns, possible error")
    # Error finding headword pronunciations, or something
    return None
  if not found_pronuns:
    pagemsg("WARNING: No found pronuns")
    return None
  # How many headword pronuns? If only one, automatically assign all found
  # pronuns to it.
  distinct_hprons = set(hpron for hpron, tr in headword_pronuns)
  if len(distinct_hprons) == 1:
    hpron = list(distinct_hprons)[0]
    for foundpron in found_pronuns:
      valtoadd = foundpron or hpron
      if hpron in matches:
        if valtoadd not in matches[hpron]:
          matches[hpron].append(valtoadd)
      else:
        matches[hpron] = [valtoadd]

  else:
    # Multiple headwords, need to match "the hard way"
    all_match = True
    unmatched_hpron = set()
    hpron_seen = set()
    for hpron, tr in headword_pronuns:
      if hpron in hpron_seen:
        pagemsg("Skipping already-seen headword pronun %s%s" % (
          hpron, tr and "//" + tr))
        continue
      hpron_seen.add(hpron)
      new_found_pronuns = []
      matched = False
      for foundpron in found_pronuns:
        if pronun_matches(hpron, foundpron, pagemsg):
          if tr and not "phon=" in foundpron:
            pagemsg("WARNING: Found translit %s for headword %s, but matched against ru-IPA pronun %s lacking phon=" % (
              tr, hpron, foundpron))
          valtoadd = foundpron or hpron
          if hpron in matches:
            if valtoadd not in matches[hpron]:
              matches[hpron].append(valtoadd)
          else:
            matches[hpron] = [valtoadd]
          matched = True
        else:
          new_found_pronuns.append(foundpron)
      found_prons = new_found_pronuns
      if not matched:
        all_match = False
        unmatched_hpron.add(hpron)
    if not all_match:
      pagemsg("WARNING: Unable to match headword pronuns %s against found pronuns %s" %
          (",".join(unmatched_hpron), ",".join(found_pronuns)))
      return None

  def get_reduced_stem(nom):
    # The stem for reduce_stem() should preserve -й
    stem_for_reduce = re.sub(u"[аяеоьыи]́?$", "", nom)
    epenthetic_vowel = nom.endswith(AC)
    if re.search(u"[аяеоыи]́?$", nom):
      reduced_stem = expand_text("{{#invoke:ru-common|dereduce_stem|%s||%s}}" %
        (stem_for_reduce, "y" if epenthetic_vowel else ""))
    else:
      reduced_stem = expand_text("{{#invoke:ru-common|reduce_stem|%s}}" %
          stem_for_reduce)
    return reduced_stem

  def get_dereduced_adj_stem(stem, epvowel):
    return expand_text("{{#invoke:ru-common|dereduce_stem|%s||%s}}" %
        (stem, "y" if epvowel else ""))

  # Apply a function to a list of found pronunciations. If a pronunciation
  # begins with phon=, strip it off before applying the function and then
  # add it back. Don't include results where the return value from the
  # function is logically false.
  def frob_foundprons(foundprons, fun):
    retval = []
    for foundpron in foundprons:
      if foundpron.startswith("phon="):
        funval = fun(re.sub("^phon=", "", foundpron))
        if funval:
          retval.append("phon=" + funval)
      else:
        funval = fun(foundpron)
        if funval:
          retval.append(funval)
    return retval

  # Remove cases where key just maps to itself (as a list)
  #return dict((k,v) for k,v in matches.iteritems() if v != [k])
  matches = dict((k,v) for k,v in matches.iteritems() if v != [k])
  matches_stems = {}

  for hpron,foundprons in matches.iteritems():
    stems = []
    def append_stem_foundstems(stem, foundpronunstems):
      if stem and foundpronunstems:
        stems.append((stem, foundpronunstems))
    append_stem_foundstems(re.sub(u"[аеиояыьй]́?$", "", hpron),
      frob_foundprons(foundprons, lambda x:re.sub(u"[аеиояыьй]́?$", "", x)))
    # Also compute reduced/unreduced stem
    append_stem_foundstems(get_reduced_stem(hpron),
      frob_foundprons(foundprons, get_reduced_stem))
    # Also check for adjectival stem
    adjstem = re.sub(u"([иыо]́?й|[ая]́?я|[ое]́?е|[ыи]́?е)$", "", hpron)
    if adjstem != hpron:
      foundpronstems = frob_foundprons(foundprons,
          lambda x:re.sub(u"([иыо]́?й|[ая]́?я|[ое]́?е|[ыи]́?е)$", "", x))
      append_stem_foundstems(adjstem, foundpronstems)
      pagemsg("Adding adjectival stem mapping %s->%s" % (
        adjstem, ",".join(foundpronstems)))
      # If adjectival, dereduce with both stressed and unstressed epenthetic
      # vowel
      for epvowel in [False, True]:
        deredstem = get_dereduced_adj_stem(adjstem, epvowel)
        deredfoundpronstems = frob_foundprons(foundpronstems,
            lambda x:get_dereduced_adj_stem(x, epvowel))
        append_stem_foundstems(deredstem, deredfoundpronstems)
        pagemsg("Adding adjectival dereduced stem mapping %s->%s" % (
          deredstem, ",".join(deredfoundpronstems)))
    matches_stems[hpron] = stems
  return matches_stems

def get_lemmas_of_form_page(parsed):
  lemmas = set()
  for t in parsed.filter_templates():
    tname = unicode(t.name)
    if (tname == "inflection of" and getparam(t, "lang") == "ru" or
        tname == "ru-participle of"):
      lemma = ru.remove_accents(blib.remove_links(getparam(t, "1")))
      lemmas.add(lemma)
  return lemmas

# Cache mapping page titles to a set of the gem= values found on the page.
lemma_gem_cache = {}
# Cache mapping page titles to a map from headwords to pronunciations
# found on the page.
lemma_headword_to_pronun_mapping_cache = {}

# Look up the lemmas of all inflection-of templates in PARSED (the contents
# of an etym section), and for each such lemma, do two things: (1) fetch the
# gem= values from the ru-IPA templates, (2) fetch a mapping from
# headword-derived stems to pronunciations as found in the ru-IPA templates.
# Return a tuple (GEMVALS, PRONUNMAPPING), where GEMVALS is the set of all
# gem= values found and PRONUNMAPPING is a map as described above.
def lookup_gem_values_and_pronun_mapping(parsed, verbose, pagemsg):
  lemmas = get_lemmas_of_form_page(parsed)
  all_gemvals = set()
  all_pronunmappings = {}
  final_geminate_in_lemma = False
  lemma_has_geminate_other_than_final = False
  for lemma in lemmas:
    # Need to create our own expand_text() with the page title set to the
    # lemma
    def expand_text(t):
      return blib.expand_text(t, lemma, pagemsg, verbose)

    if lemma in lemma_gem_cache:
      cached = True
      assert lemma in lemma_headword_to_pronun_mapping_cache
      gemval = lemma_gem_cache[lemma]
      pronunmapping = lemma_headword_to_pronun_mapping_cache[lemma]
    else:
      cached = False
      newpage = pywikibot.Page(site, lemma)
      gemval = set()
      try:
        parsed = blib.parse(newpage)
      except pywikibot.exceptions.InvalidTitle as e:
        pagemsg("WARNING: Invalid title, skipping")
        traceback.print_exc(file=sys.stdout)
        continue

      # Compute headword->pronun mapping
      headwords = get_headword_pronuns(parsed, lemma, pagemsg, expand_text)
      foundpronuns = []
      for t in parsed.filter_templates():
        if unicode(t.name) == "ru-IPA":
          phon = getparam(t, "phon")
          if phon:
            foundpronuns.append("phon=%s" % phon)
          else:
            foundpronuns.append(getparam(t, "1"))
      pronunmapping = match_headword_and_found_pronuns(headwords, foundpronuns,
          pagemsg, expand_text)
      lemma_headword_to_pronun_mapping_cache[lemma] = pronunmapping

      # Compute gemval
      for t in parsed.filter_templates():
        if unicode(t.name) == "ru-IPA":
          gemval.add(getparam(t, "gem"))

      # Now, see if we need to modify the gem= value to take into account final
      # geminates in the lemma or non-lemma forms.
      if re.search("([" + ru.cons + r"])\1( |,|$)", lemma):
        pagemsg("Found lemma %s with final geminate" % lemma)
        has_gemval = not not [x for x in gemval if x]
        if has_gemval:
          pagemsg("WARNING: Found final geminate in lemma %s and gem= present, can't set gem=n" %
              lemma)
        else:
          other_geminate_in_lemma = False
          for t in parsed.filter_templates():
            if unicode(t.name) == "ru-IPA":
              pronunval = expand_text(re.sub(r"\}\}$", "|raw=y}}", unicode(t)))
              # The following regex will trigger on C(ː), which is correct
              if re.search(u"[^ɕʑ]ː", pronunval):
                other_geminate_in_lemma = True
          # Also try the plain headwords as pronun, in case of missing pronun
          # or some reason where the stem->pronun mapping fails
          for headword, tr in headwords:
            pronunval = expand_text("{{ru-IPA|%s|raw=y}}" % headword)
            if re.search(u"[^ɕʑ]ː", pronunval):
              other_geminate_in_lemma = True
          if other_geminate_in_lemma:
            pagemsg("WARNING: Other geminate in lemma %s with final geminate, can't set gem=n" %
                lemma)
          else:
            pagemsg("Found final geminate in lemma %s, setting gem=n" % lemma)
            gemval = set(["n"])

      lemma_gem_cache[lemma] = gemval

    pagemsg("For lemma %s, found gem=%s%s" % (lemma, ",".join(gemval),
      cached and " (cached)" or ""))
    all_gemvals |= gemval
    # The output is HEADWORD->(STEMS_AND_PRONUNS),HEADWORD->(STEMS_AND_PRONUNS)...
    # where STEMS_AND_PRONUNS is STEM:PRONUNS,STEM:PRONUNS,...,
    # where PRONUNS is PRONUN/PRONUN/...
    pagemsg("For lemma %s, found pronun mapping %s%s" % (lemma, "None" if
      pronunmapping is None else "(empty)" if not pronunmapping else ",".join(
        "%s->(%s)" % (hpron, ",".join("%s:%s" % (stem, "/".join(foundprons))
          for stem, foundprons in stem_foundprons))
        for hpron, stem_foundprons in pronunmapping.iteritems()),
      cached and " (cached)" or ""))
    if pronunmapping:
      all_pronunmappings.update(pronunmapping)

  if len(all_gemvals) > 1:
    pagemsg("WARNING: Found multiple gem= values (gem=%s) corresponding to lemma %s, not using" %
        (",".join(all_gemvals), ",".join(lemmas)))
  return all_gemvals, all_pronunmappings

def process_section(section, indentlevel, headword_pronuns, override_ipa,
    pagetitle, verbose, pagemsg, expand_text):
  assert indentlevel in [3, 4]
  notes = []

  def compute_ipa():
    computed_ipa = {}
    for cyr, tr in headword_pronuns:
      pronun = tr or cyr
      result = expand_text("{{#invoke:ru-pron|ipa|%s}}" % pronun)
      if not result:
        return False
      computed_ipa[pronun] = result
    return computed_ipa

  parsed = blib.parse_text(section)

  # Find gem= param by looking up {{inflection of}} pages
  headword_gemparam = ""
  gemvals, pronunmapping = lookup_gem_values_and_pronun_mapping(parsed,
      verbose, pagemsg)
  if len(gemvals) == 1:
    gemval = list(gemvals)[0]
    if gemval:
      headword_gemparam = "|gem=%s" % gemval
      pagemsg("Adding %s to ru-IPA pronun(s)" % headword_gemparam)

  pronun_lines = []
  bad_char_msgs = []
  # Figure out how many headword variants there are, and if there is more
  # than one, add |ann=y to each one; but don't get confused by cases where
  # there are multiple translit variants of the same headword, as in
  # {{ru-noun form|а́бвера|tr=ábvera, ábvɛra|m-in}}.
  num_annotations = 0
  annotations_set = set()
  for cyr, tr in headword_pronuns:
    annotations_set.add(cyr)
  matched_hpron = set()
  manually_subbed_pronun = False
  for pronun, tr in headword_pronuns:
    if len(annotations_set) > 1:
      if tr:
        # Don't include DOTBELOW in the annotation param or it will be shown
        # to the user.
        headword_annparam = "|ann=%s" % pronun.replace(DOTBELOW, "")
      else:
        headword_annparam = "|ann=y"
    else:
      headword_annparam = ""
    if pronun.startswith("-") or pronun.endswith("-"):
      pagemsg("WARNING: Skipping prefix or suffix: %s" % pronun)
      return None
    if "." in pronun:
      pagemsg("WARNING: Pronunciation has dot in it, skipping: %s" % pronun)
      return None
    if ru.needs_accents(pronun, split_dash=True):
      pagemsg("WARNING: Pronunciation lacks accents, skipping: %s" % pronun)
      return None
    if contains_non_cyrillic_non_latin(pronun):
      bad_char_msgs.append(
          "WARNING: Pronunciation %s to be added contains non-Cyrillic non-Latin chars, skipping" %
            pronun)
    elif contains_latin(pronun):
      bad_char_msgs.append(
          "WARNING: Cyrillic pronunciation %s contains Latin characters, skipping" %
          pronun)
    def append_pronun_line(pronun):
      if (not pronun.startswith("phon=") and (
         ru.is_monosyllabic(pronun) and re.sub(AC, "", pronun) == pagetitle or
         re.search(u"ё", pronun) and pronun == pagetitle)):
        pronun = "* {{ru-IPA%s%s}}\n" % (headword_annparam, headword_gemparam)
      else:
        pronun = "* {{ru-IPA|%s%s%s}}\n" % (pronun, headword_annparam,
            headword_gemparam)
      if pronun not in pronun_lines:
        pronun_lines.append(pronun)

    subbed_pronun = False

    # Check for manual pronunciation mapping
    for regex, subvals in manual_pronun_mapping:
      if re.search(regex, pronun):
        applied_manual_pronun_mappings.add(regex)
        if type(subvals) is not list:
          subvals = [subvals]
        for subval in subvals:
          newpronun = re.sub(regex, subval, pronun)
          pagemsg("Replacing headword-based pronunciation %s with %s due to manual_pronun_mapping"
              % (pronun, newpronun))
          append_pronun_line(newpronun)
        subbed_pronun = True
        manually_subbed_pronun = True

    # If there is an automatically-derived headword->pronun mapping (e.g.
    # in case of secondary stress or phon=), try to apply it.
    if not subbed_pronun and pronunmapping:
      for hpron, stem_foundprons in pronunmapping.iteritems():
        outerbreak = False
        for stem, foundpronstems in stem_foundprons:
          assert stem
          assert foundpronstems
          if pronun.startswith(stem):
            for foundpronstem in foundpronstems:
              newpronun = re.sub("^" + re.escape(stem), foundpronstem, pronun)
              if newpronun != pronun:
                pagemsg("Replacing headword-based pronunciation %s with %s" %
                    (pronun, newpronun))
              append_pronun_line(newpronun)
            subbed_pronun = True
            matched_hpron.add(hpron)
            outerbreak = True
            break
        if outerbreak:
          break

    # Otherwise, reverse-translit if transliteration, or use headword pronun
    # unchanged.
    if subbed_pronun:
      pass
    elif tr:
      reverse_translit = ru_reverse_translit.reverse_translit(tr)
      pagemsg("WARNING: Reverse-transliterating %s to phon=%s" %
          (tr, reverse_translit))
      append_pronun_line("phon=%s" % reverse_translit)
    else:
      append_pronun_line(pronun)

  if pronunmapping and not manually_subbed_pronun:
    for hpron, stem_foundprons in pronunmapping.iteritems():
      if hpron not in matched_hpron:
        pagemsg("WARNING: Unable to match mapping %s->(%s) in non-lemma form(s)"
          % (hpron, ",".join("%s:%s" % (stem, "/".join(foundprons))
            for stem, foundprons in stem_foundprons)))

  # Check for indications of pre-reform spellings
  for cat in [u"Russian spellings with е instead of ё",
      u"Russian terms spelled with Ѣ",
      u"Russian terms spelled with Ѳ",
      u"Russian pre-1918 spellings"]:
    if re.search(ur"\[\[Category:%s]]" % cat, section):
      pagemsg(u"WARNING: Found [[Category:%s]], skipping" % cat)
      return None

  ipa_templates = []
  for t in parsed.filter_templates():
    tname = unicode(t.name)
    if tname in ["ru-pre-reform", u"ru-noun-alt-ё", u"ru-alt-ё", "ru-IPA-manual"]:
      pagemsg("WARNING: Found %s template, skipping" % tname)
      return None
    if tname in ["alternative form of", "alternative spelling of"] and getparam(t, "lang") == "ru":
      # Check if word spelled with е instead of ё, without using
      # [[Category:Russian spellings with е instead of ё]], which we
      # catch above.
      target = getparam(t, "1")
      if u"ё" in target and re.sub(u"ё", u"е", target) == pagetitle:
        pagemsg(u"WARNING: Found apparent alternative form using е in place of ё without explicit category, skipping: %s" %
            unicode(t))
        return None
    if tname == "IPA" and getparam(t, "lang") == "ru":
      ipa_templates.append(t)
  if (re.search(r"[Aa]bbreviation", section) and not
      re.search("==Abbreviations==", section) or
      re.search("ru-(etym )?abbrev of", section)):
    pagemsg("WARNING: Found the word 'abbreviation', please check")
  if (re.search(r"[Aa]cronym", section) and not
      re.search("==Acronyms==", section)):
    pagemsg("WARNING: Found the word 'acronym', please check")
  if (re.search(r"[Ii]nitialism", section) and not
      re.search("==Initialisms==", section)):
    pagemsg("WARNING: Found the word 'initialism', please check")
  if ipa_templates:
    ipa_templates_msg = (
      "Processing raw IPA %s for headword(s) %s" % (
      "++".join([unicode(x) for x in ipa_templates]),
      "++".join(printable_ru_tr(cyr, ru) for cyr, ru in headword_pronuns)))
    pagemsg(ipa_templates_msg)
    computed_ipa = compute_ipa()
    num_replaced = 0
    if not computed_ipa:
      # Error occurred computing IPA of headwords
      return None
    for ipa_template in ipa_templates:
      mismatch_msgs = []
      computed_ipa_items = computed_ipa.items()
      for headword, autoipa in computed_ipa_items:
        if contains_latin(headword):
          pagemsg("WARNING: Headword %s to be used to replace manual IPA contains Latin chars, skipping" %
                headword)
          return None
        elif contains_non_cyrillic(headword):
          pagemsg("WARNING: Headword %s to be used to replace manual IPA contains non-Cyrillic non-Latin chars, skipping" %
                headword)
          return None
        retval = ipa_matches(headword, getparam(ipa_template, "1"), autoipa,
            ipa_templates_msg, pagemsg)
        if retval != True and override_ipa and (len(ipa_templates) > 1 or
            len(computed_ipa_items) > 1):
          pagemsg("WARNING: Can't override IPA because multiple IPA templates or headwords: %s template(s), %s headword(s)" % (
            len(ipa_templates), len(computed_ipa_items)))
        elif retval == True or override_ipa:
          orig_ipa_template = unicode(ipa_template)
          rmparam(ipa_template, "lang")
          rmparam(ipa_template, "1")
          if len(ipa_template.params) > 0:
            pagemsg("WARNING: IPA template has extra parameters, skipping: %s" %
                orig_ipa_template)
            return None
          ipa_template.name = "ru-IPA"
          ipa_template.add("1", headword)
          if retval != True and override_ipa:
            pagemsg("WARNING: Overriding IPA despite pronunciation mismatch")
            mismatch_msgs.append(retval)
            for m in mismatch_msgs:
              pagemsg(re.sub("^WARNING:", "WARNING (IGNORED):", m))
          pagemsg("Replaced %s with %s" % (
            orig_ipa_template, unicode(ipa_template)))
          num_replaced += 1
          mismatch_msgs = []
          notes.append("replace {{IPA|...}} with {{ru-IPA|...}} for %s%s" % (
            headword, " (IPA override)" if retval != True and override_ipa else ""))
          break
        mismatch_msgs.append(retval)
      if mismatch_msgs:
        for m in mismatch_msgs:
          pagemsg(m)
    if num_replaced > 0:
      section = unicode(parsed)
    if num_replaced < len(ipa_templates):
      pagemsg("Unable to replace %s of %s raw IPA template(s)" % (
        len(ipa_templates) - num_replaced, len(ipa_templates)))
    return section, notes

  def canonicalize_pronun(pron, paramname):
    newpron = re.sub(u"ё́", u"ё", pron)
    newpron = re.sub(AC + "+", AC, newpron)
    if newpron != pron:
      notes.append("remove extra accents from %s= (ru-IPA)" % paramname)
      pron = newpron
    # We want to go word-by-word and check to see if the headword word is
    # the same as the ru-IPA word but has additional accents in it, and
    # if so copy the headword word to the ru-IPA word. One way to do that
    # is to check that the ru-IPA word has no accents and that the headword
    # word minus accents is the same as the ru-IPA word.
    if not bad_char_msgs and len(headword_pronuns) == 1 and not headword_pronuns[0][1]:
      # FIXME, handle translit here
      hwords = re.split(r"([\s\-]+)", headword_pronuns[0][0])
      pronwords = re.split(r"([\s\-]+)", pron)
      changed = False
      if len(hwords) == len(pronwords):
        for i in xrange(len(hwords)):
          hword = hwords[i]
          pronword = pronwords[i]
          if (len(hword) > len(pronword) and ru.is_unaccented(pronword) and
              ru.remove_accents(hword) == pronword):
            changed = True
            pronwords[i] = hword
      if changed:
        pron = "".join(pronwords)
        notes.append("copy accents from headword to %s= (ru-IPA)" % paramname)
    return pron

  parsed = blib.parse_text(section)
  for t in parsed.filter_templates():
    if unicode(t.name) == "ru-IPA":
      origt = unicode(t)
      phon = getparam(t, "phon")
      if phon:
        if t.has("1"):
          rmparam(t, "1")
          notes.append("remove 1= when phon= present (ru-IPA)")
        newphon = canonicalize_pronun(phon, "phon")
        if phon != newphon:
          t.add("phon", newphon)
      arg1 = getparam(t, "1")
      if arg1:
        newarg1 = canonicalize_pronun(arg1, "1")
        if arg1 != newarg1:
          t.add("1", newarg1)
          arg1 = newarg1
        if ru.is_monosyllabic(arg1) and re.sub(AC, "", arg1) == pagetitle:
          notes.append("remove 1= because monosyllabic and same as pagetitle modulo accents (ru-IPA)")
          rmparam(t, "1")
        elif re.search(u"ё", arg1) and arg1 == pagetitle:
          notes.append(u"remove 1= because same as pagetitle and has ё (ru-IPA)")
          rmparam(t, "1")
      if t.has("adj"):
        notes.append("remove unused adj= (ru-IPA)")
        rmparam(t, "adj")
      newt = unicode(t)
      if newt != origt:
        pagemsg("Replaced %s with %s" % (origt, newt))
  section = unicode(parsed)

  foundpronuns = []
  for m in re.finditer(r"(\{\{ru-IPA(?:\|([^}]*))?\}\})", section):
    template_text = m.group(1)
    pagemsg("Already found pronunciation template: %s" % template_text)
    template = blib.parse_text(template_text).filter_templates()[0]
    phonparam = getparam(template, "phon")
    foundpronun = phonparam or getparam(template, "1") or pagetitle
    foundpronun = canonicalize_monosyllabic_pronun(foundpronun, "")[0]
    if phonparam:
      foundpronun = "phon=" + foundpronun
    # FIXME, not clear if we want to do this
    gemparam = getparam(template, "gem")
    if gemparam:
      foundpronun += "|gem=" + gemparam
    foundpronuns.append(foundpronun)
  if foundpronuns:
    joined_foundpronuns = ",".join(foundpronuns)
    headword_pronuns_as_pronuns_no_gem = [ru_tr_as_pronun(cyr, tr) for cyr, tr in headword_pronuns]
    headword_pronuns_as_pronuns = [x + headword_gemparam for x in headword_pronuns_as_pronuns_no_gem]
    joined_headword_pronuns = ",".join(headword_pronuns_as_pronuns)
    if "phon=" not in joined_foundpronuns and "phon=" in joined_headword_pronuns:
      pagemsg("WARNING: Existing pronunciation template %s probably needs phon= because headword-derived pronunciation %s contains Latin" %
        (joined_foundpronuns, joined_headword_pronuns))
    if "phon=" in joined_foundpronuns and "phon=" not in joined_headword_pronuns:
      pagemsg("WARNING: Existing pronunciation template has pronunciation %s with phon=, headword-derived pronunciation %s isn't Latin, probably need manual translit in headword and decl" %
        (joined_foundpronuns, joined_headword_pronuns))
    if "gem=" not in joined_foundpronuns and "gem=" in joined_headword_pronuns:
      pagemsg("WARNING: Existing pronunciation template %s probably needs gem= because headword-derived pronunciation %s has it" %
        (joined_foundpronuns, joined_headword_pronuns))

    if len(foundpronuns) < len(headword_pronuns_as_pronuns):
      pagemsg("WARNING: Fewer existing pronunciations (%s) than headword-derived pronunciations (%s): existing %s, headword-derived %s" % (
        len(foundpronuns), len(headword_pronuns_as_pronuns),
        joined_foundpronuns, joined_headword_pronuns))
    headword_pronuns_as_pronuns_no_gem_or_grave = [ru.remove_grave_accents(x) for x in headword_pronuns_as_pronuns_no_gem]
    headword_pronuns_as_pronuns_no_gem_grave_or_dotbelow = [x.replace(DOTBELOW, "") for x in headword_pronuns_as_pronuns_no_gem_or_grave]
    foundpronuns_no_gem = [re.sub(r"\|gem=[^|]*", "", x) for x in foundpronuns]
    foundpronuns_no_gem_or_grave = [ru.remove_grave_accents(x) for x in foundpronuns_no_gem]
    foundpronuns_no_gem_grave_or_dotbelow = [x.replace(DOTBELOW, "") for x in foundpronuns_no_gem_or_grave]
    if set(foundpronuns_no_gem_grave_or_dotbelow) != set(headword_pronuns_as_pronuns_no_gem_grave_or_dotbelow):
      pagemsg("WARNING: Existing pronunciation template (w/o gem=, grave accent or dotbelow) has different pronunciation %s from headword-derived pronunciation %s" %
            (joined_foundpronuns, joined_headword_pronuns))
    elif set(foundpronuns_no_gem_or_grave) != set(headword_pronuns_as_pronuns_no_gem_or_grave):
      pagemsg("WARNING: Existing pronunciation template (w/o gem= or grave accent) has different pronunciation %s from headword-derived pronunciation %s, but only in dotbelow" %
            (joined_foundpronuns, joined_headword_pronuns))
    elif set(foundpronuns_no_gem) != set(headword_pronuns_as_pronuns_no_gem):
      pagemsg("WARNING: Existing pronunciation template (w/o gem=) has different pronunciation %s from headword-derived pronunciation %s, but only in grave accents" %
            (joined_foundpronuns, joined_headword_pronuns))
    elif set(foundpronuns) != set(headword_pronuns_as_pronuns):
      pagemsg("WARNING: Existing pronunciation template has different pronunciation %s from headword-derived pronunciation %s, but only in gem=" %
            (joined_foundpronuns, joined_headword_pronuns))

    return section, notes

  pronunsection = "%sPronunciation%s\n%s\n" % ("="*indentlevel, "="*indentlevel,
      "".join(pronun_lines))

  if bad_char_msgs:
    for badmsg in bad_char_msgs:
      pagemsg(badmsg)
      return None

  origsection = section
  # If pronunciation section already present, insert pronun into it; this
  # could happen when audio but not IPA is present
  if re.search(r"^===+Pronunciation===+$", section, re.M):
    pagemsg("Found pronunciation section without ru-IPA or IPA")
    section = re.sub(r"^(===+Pronunciation===+)\n+", r"\1\n%s" %
        "".join(pronun_lines), section, 1, re.M)
  else:
    # Otherwise, skip past any ===Etymology=== or ===Alternative forms===
    # sections at the beginning. This requires us to split up the subsections,
    # find the right subsection to insert before, and then rejoin.
    subsections = re.split("(^===.*?===\n)", section, 0, re.M)

    insert_before = 1
    while True:
      if insert_before >= len(subsections):
        pagemsg("WARNING: Malformatted headers, no level-3/4 POS header")
        return None
      if ("===Alternative forms===" not in subsections[insert_before] and
          "===Etymology===" not in subsections[insert_before]):
        break
      insert_before += 2
    subsections[insert_before] = re.sub(r"(^===)", r"%s\1" % pronunsection, subsections[insert_before], 1, re.M)
    section = "".join(subsections)

  # Make sure there's a blank line before an initial header (even if there
  # wasn't one before).
  section = re.sub("^===", "\n===", section, 1)

  if section == origsection:
    pagemsg("WARNING: Something wrong, couldn't sub in pronunciation section")
    return None

  notes.append("add pronunciation %s" % printable_ru_tr_list(headword_pronuns))

  return section, notes

def process_page_text(index, text, pagetitle, verbose, override_ipa):
  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  def expand_text(tempcall):
    return blib.expand_text(tempcall, pagetitle, pagemsg, verbose)

  notes = []

  foundrussian = False
  sections = re.split("(^==[^=]*==\n)", text, 0, re.M)
  orig_text = text
  for j in xrange(2, len(sections), 2):
    if sections[j-1] == "==Russian==\n":
      if foundrussian:
        pagemsg("WARNING: Found multiple Russian sections")
        return None
      foundrussian = True

      need_l3_pronun = False
      if "===Pronunciation 1===" in sections[j]:
        pagemsg("WARNING: Found ===Pronunciation 1===, should convert page to multiple etymologies")
        return None
      if "===Etymology 1===" in sections[j]:

        # If multiple etymologies, things are more complicated. We may have to
        # process each section individually. We fetch the headwords from each
        # section to see whether the etymologies should be in split or
        # combined form. If they should be in split form, we remove any
        # combined pronunciation and add pronunciations to each section if
        # not already present. If they should be in combined form, we
        # remove pronunciations from individual sections (PARTLY IMPLEMENTED)
        # and add a combined pronunciation at the top.

        etymsections = re.split("(^ *=== *Etymology +[0-9]+ *=== *\n)", sections[j], 0, re.M)
        # Make sure there are multiple etymologies, otherwise page is malformed
        pagemsg("Found multiple etymologies (%s)" % (len(etymsections)//2))
        if len(etymsections) < 5:
          pagemsg("WARNING: Misformatted page with multiple etymologies (too few etymologies, skipping)")
          return None

        # Check for misnumbered etymology sections
        # FIXME, this should be a separate script
        expected_etym_num = 0
        l3split = re.split(r"^(===[^=\n].*===\n)", sections[j], 0, re.M)
        seen_etym_1 = False
        for k in xrange(1, len(l3split), 2):
          if not seen_etym_1 and l3split[k] != "===Etymology 1===\n":
            continue
          seen_etym_1 = True
          expected_etym_num += 1
          if l3split[k] != "===Etymology %s===\n" % expected_etym_num:
            pagemsg("WARNING: Misformatted page with multiple etymologies, expected ===Etymology %s=== but found %s" % (
              expected_etym_num, l3split[k].replace("\n", "")))
            break

        # Check if all per-etym-section headwords are the same
        etymparsed2 = blib.parse_text(etymsections[2])
        etym_headword_pronuns = {}
        # Fetch the headword pronuns of the ===Etymology 1=== section.
        # We don't check for None here so that an error in an individual
        # section doesn't cause us to bow out entirely; instead, we treat
        # any comparison with None as False so we will always end up with
        # per-section pronunciations.
        etym_headword_pronuns[2] = get_headword_pronuns(etymparsed2, pagetitle, pagemsg, expand_text)
        need_per_section_pronuns = False
        for k in xrange(4, len(etymsections), 2):
          etymparsed = blib.parse_text(etymsections[k])
          # Fetch the headword pronuns of the ===Etymology N=== section.
          # We don't check for None here; see above.
          etym_headword_pronuns[k] = get_headword_pronuns(etymparsed, pagetitle, pagemsg, expand_text)
          # Treat any comparison with None as False.
          if not etym_headword_pronuns[2] or not etym_headword_pronuns[k] or set(etym_headword_pronuns[k]) != set(etym_headword_pronuns[2]):
            pagemsg("WARNING: Etym section %s pronuns %s different from etym section 1 pronuns %s" % (
              k//2, printable_ru_tr_list(etym_headword_pronuns[k] or [("none", "")]), printable_ru_tr_list(etym_headword_pronuns[2] or [("none", "")])))
            need_per_section_pronuns = True
        numpronunsecs = len(re.findall("^===Pronunciation===$", etymsections[0], re.M))
        if numpronunsecs > 1:
          pagemsg("WARNING: Multiple ===Pronunciation=== sections in preamble to multiple etymologies, needs to be fixed")
          return None

        if need_per_section_pronuns:
          pagemsg("Multiple etymologies, split pronunciations needed")
        else:
          pagemsg("Multiple etymologies, combined pronunciation possible")

        # If need split pronunciations and there's a combined pronunciation,
        # delete it if possible.
        if need_per_section_pronuns and numpronunsecs == 1:
          pagemsg("Multiple etymologies, converting combined pronunciation to split pronunciation (deleting combined pronun)")
          # Remove existing pronunciation section; but make sure it's safe
          # to do so (must have nothing but ru-IPA templates in it, and the
          # pronunciations in them must match what's expected)
          m = re.search(r"(^===Pronunciation===\n)(.*?)(^==|\Z)", etymsections[0], re.M | re.S)
          if not m:
            pagemsg("WARNING: Can't find ===Pronunciation=== section when it should be there, logic error?")
            return None
          if not re.search(r"^(\* \{\{ru-IPA(?:\|([^}]*))?\}\}\n)*$", m.group(2)):
            pagemsg("WARNING: Pronunciation section to be removed contains extra stuff (e.g. manual IPA or audio), can't remove: <%s>\n" % (
              m.group(1) + m.group(2)))
            return None
          foundpronuns = []
          for m in re.finditer(r"(\{\{ru-IPA(?:\|([^}]*))?\}\})", m.group(2)):
            # FIXME, not right, should do what we do above with foundpronuns
            # where we work with the actual parsed template
            foundpronuns.append(m.group(2) or pagetitle)
          # FIXME, this may be wrong with translit
          foundpronuns = remove_list_duplicates([canonicalize_monosyllabic_pronun(x, "")[0] for x in foundpronuns])
          if foundpronuns:
            joined_foundpronuns = ",".join(foundpronuns)
            # Combine headword pronuns while preserving order. To do this,
            # we sort by numbered etymology sections and then flatten.
            combined_headword_pronuns = remove_list_duplicates([y for k,v in sorted(etym_headword_pronuns.iteritems(), key=lambda x:x[0]) for y in (v or [])])
            joined_headword_pronuns = ",".join(ru_tr_as_pronun(cyr, tr) for cyr, tr in combined_headword_pronuns)
            if not (set(foundpronuns) <= set(combined_headword_pronuns)):
              pagemsg("WARNING: When trying to delete pronunciation section, existing pronunciation %s not subset of headword-derived pronunciation %s, unable to delete" %
                    (joined_foundpronuns, joined_headword_pronuns))
              return None
          etymsections[0] = re.sub(r"(^===Pronunciation===\n)(.*?)(\Z|^==|^\[\[|^--)", r"\3", etymsections[0], 1, re.M | re.S)
          sections[j] = "".join(etymsections)
          text = "".join(sections)
          notes.append("remove combined pronun section")
          pagemsg("Removed pronunciation section because combined pronunciation with multiple etymologies needs to be split")

        # If need combined pronunciations, check for split pronunciations and
        # remove them. As a special case, if there's only one split
        # pronunciation, just move the whole section to the top. We do this
        # so we move audio, homophones, etc. This situation will frequently
        # happen when a script adds a non-lemma form to an existing page
        # without split etymologies, because it wraps everything in an
        # "Etymology 1" section.
        # FIXME: When we move the whole section to the top, it could be
        # incorrect to do so if the ru-IPA isn't just the headword, e.g. if
        # it has a strange spelling, or phon= or gem=, etc. We should probably
        # check for this.
        if not need_per_section_pronuns:
          # Check for a single pronunciation section that we can move
          num_secs_with_pronun = 0
          first_sec_with_pronun = 0
          for k in xrange(2, len(etymsections), 2):
            if "===Pronunciation===" in etymsections[k]:
              num_secs_with_pronun += 1
              if not first_sec_with_pronun:
                first_sec_with_pronun = k
          if num_secs_with_pronun == 1:
            # Section ends with another section start, end of text, a wikilink
            # or category link, or section divider. (Normally there should
            # always be another section following.)
            m = re.search(r"(^===+Pronunciation===+\n.*?)(\Z|^==|^\[\[|^--)",
                etymsections[first_sec_with_pronun], re.M | re.S)
            if not m:
              pagemsg("WARNING: Can't find ====Pronunciation==== section when it should be there, logic error?")
            else:
              # Set indentation of Pronunciation to 3
              pronunsec = re.sub(r"===+Pronunciation===+",
                  "===Pronunciation===", m.group(1))
              etymsections[first_sec_with_pronun] = re.sub(
                  r"^(===+Pronunciation===+\n.*?)(\Z|^==|^\[\[|^--)", r"\2",
                  etymsections[first_sec_with_pronun], 1, re.M | re.S)
              etymsections[0] = ensure_two_trailing_nl(etymsections[0])
              etymsections[0] += pronunsec
              sections[j] = "".join(etymsections)
              text = "".join(sections)
              notes.append("move split pronun section to top to make combined")
              pagemsg("Moved split pronun section for ===Etymology %s=== to top" % (k//2))
          elif num_secs_with_pronun > 1:
            pagemsg("WARNING: need combined pronunciation section, but there are multiple split pronunciation sections, code to delete them not implemented; delete manually)")
              # FIXME: Implement me

        # Now add the per-section or combined pronunciation
        if need_per_section_pronuns:
          for k in xrange(2, len(etymsections), 2):
            # Skip processing if pronuns are None.
            if not etym_headword_pronuns[k]:
              continue
            result = process_section(etymsections[k], 4,
                etym_headword_pronuns[k], override_ipa, pagetitle,
                verbose, pagemsg, expand_text)
            if result is None:
              continue
            etymsections[k], etymsection_notes = result
            notes.extend(etymsection_notes)
          sections[j] = "".join(etymsections)
          text = "".join(sections)
        else:
          need_l3_pronun = True

      else:
        need_l3_pronun = True

      if need_l3_pronun:
        # Get the headword pronunciations for the whole page.
        # NOTE: Perhaps when we've already computed per-section headword
        # pronunciations, as with multiple etymologies, we should combine
        # them rather than checking the whole page. This will make a
        # difference if there are headwords outside of the etymology sections,
        # but that shouldn't happen and is a malformed page if so.
        # NOTE NOTE: If we combine headword pronunciations with multiple
        # etymologies, we need to preserve the order as found on the page.
        headword_pronuns = get_headword_pronuns(blib.parse_text(text), pagetitle, pagemsg, expand_text)
        # If error, skip page.
        if headword_pronuns is None:
          return None

        # Process the section
        result = process_section(sections[j], 3, headword_pronuns,
            override_ipa, pagetitle, verbose, pagemsg, expand_text)
        if result is None:
          continue
        sections[j], section_notes = result
        notes.extend(section_notes)
        text = "".join(sections)

  if not foundrussian:
    pagemsg("WARNING: Can't find Russian section")
    return None

  comment = None
  if notes:
    # Group identical notes together and append the number of such identical
    # notes if > 1
    # 1. Count items in notes[] and return a key-value list in descending order
    notescount = Counter(notes).most_common()
    # 2. Recreate notes
    def fmt_key_val(key, val):
      if val == 1:
        return "%s" % key
      else:
        return "%s (%s)" % (key, val)
    notes = [fmt_key_val(x, y) for x, y in notescount]
    comment = "; ".join(notes)

  return text, comment

def process_page(index, page, save, verbose, override_ipa):
  pagetitle = unicode(page.title())

  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  pagemsg("Processing")

  if ":" in pagetitle:
    pagemsg("WARNING: Colon in page title, skipping")
    return

  if pagetitle in skip_pages:
    pagemsg("WARNING: Skipping because page in skip_pages list")
    return

  if not page.exists():
    pagemsg("WARNING: Page doesn't exist")
    return

  text = unicode(page.text)
  result = process_page_text(index, text, pagetitle, verbose, override_ipa)
  if result is None:
    return

  newtext, comment = result

  if newtext != text:
    assert comment

  # Eliminate sequences of 3 or more newlines, which may come from
  # ensure_two_trailing_nl(). Add comment if none, in case of existing page
  # with extra newlines.
  newnewtext = re.sub(r"\n\n\n+", r"\n\n", newtext)
  if newnewtext != newtext and not comment:
    comment = "eliminate sequences of 3 or more newlines"
  newtext = newnewtext

  if newtext != text:
    if verbose:
      pagemsg("Replacing <%s> with <%s>" % (text, newtext))

    if save:
      pagemsg("Saving with comment = %s" % comment)
      page.text = newtext
      page.save(comment=comment)
    else:
      pagemsg("Would save with comment = %s" % comment)

parser = argparse.ArgumentParser(description="Add pronunciation sections to Russian Wiktionary entries")
parser.add_argument('--pagefile', help="File containing pages to process, one per line")
parser.add_argument('--tempfile', help="File containing templates and headwords for quick offline reprocessing, one per line")
parser.add_argument('start', help="Starting page index", nargs="?")
parser.add_argument('end', help="Ending page index", nargs="?")
parser.add_argument('--save', action="store_true", help="Save results")
parser.add_argument('--verbose', action="store_true", help="More verbose output")
parser.add_argument('--override-IPA', action="store_true", help="Change IPA to ru-IPA even when pronunciations can't be reconciled")
parser.add_argument('--cats', default="lemma,nonlemma", help="Categories to do (lemma, nonlemma or comma-separated list)")
args = parser.parse_args()
start, end = blib.get_args(args.start, args.end)

if args.pagefile:
  lines = [x.strip() for x in codecs.open(args.pagefile, "r", "utf-8")]
  for i, line in blib.iter_items(lines, start, end):
    if line.startswith("#"):
      continue
    m = re.search(r"^\* Page ([0-9]+) \[\[(.*?)\]\]: ", line)
    if m:
      page = m.group(2)
    else:
      m = re.search(r"^Page ([0-9]+) (.*?): ", line)
      if m:
        page = m.group(2)
      else:
        page = line
    msg("Page %s %s: Processing" % (i, page))
    process_page(i, pywikibot.Page(site, page), args.save, args.verbose,
        args.override_IPA)

elif args.tempfile:
  lines = [x.strip() for x in codecs.open(args.tempfile, "r", "utf-8")]
  for line in lines:
    m = re.search(r"^Page ([0-9]+) (.*?): .*Processing raw IPA (.*) for headword\(s\) (.*)$", line)
    if not m:
      msg("WARNING: Unrecognized line: %s" % line)
    else:
      index, pagetitle, ipa_templates, headwords = m.groups()
      ipa_templates = re.split(r"\+\+", ipa_templates)
      headwords = re.split(r"\+\+", headwords)
      headword_args = []
      for i in xrange(len(headwords)):
        if i == 0:
          headword_args.append(headwords[i])
        else:
          headword_args.append("head%s=%s" % (i+1, headwords[i]))
      text = """
==Russian==

%s

{{ru-noun|%s}}
""" % ("\n".join(ipa_templates), "|".join(headword_args))
      process_page_text(index, text, pagetitle, args.verbose, args.override_IPA)

else:
  categories = []
  for cattype in re.split(",", args.cats):
    if cattype == "lemma":
      categories.append("Russian lemmas")
    elif cattype == "nonlemma":
      categories.append("Russian non-lemma forms")
    else:
      raise RuntimeError("Invalid value %s, should be 'lemma' or 'nonlemma'" %
          cattype)
  for category in categories:
    msg("Processing category: %s" % category)
    for i, page in blib.cat_articles(category, start, end):
      process_page(i, page, args.save, args.verbose, args.override_IPA)

for regex, subvals in manual_pronun_mapping:
  if regex not in applied_manual_pronun_mappings:
    msg("WARNING: Unapplied manual_pronun_mapping %s->%s" % (regex,
      ",".join(subvals) if type(subvals) is list else subvals))
