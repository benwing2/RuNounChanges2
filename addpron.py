#!/usr/bin/env python
# -*- coding: utf-8 -*-

# FIXME:

# 1. Check that headword matches page name

import pywikibot, re, sys, codecs, argparse
import difflib

import blib
from blib import getparam, rmparam

import rulib as ru

site = pywikibot.Site()

vowel_list = u"aeiouyɛəäëöü"
ipa_vowel_list = vowel_list + u"ɐɪʊɨæɵʉ"
ipa_vowels_re = "[" + ipa_vowel_list + "]"
ipa_vowels_c = "([" + ipa_vowel_list + "])"
non_ipa_vowels_re = "[^ " + ipa_vowel_list + "]"
non_ipa_vowels_non_accent_re = u"[^ ˈˌ" + ipa_vowel_list + "]"

cons_assim_palatal = {
  'compulsory':set([u'stʲ', u'zdʲ', u'nt͡ɕ', u'nɕ', u'ntʲ', u'ndʲ', u'xkʲ',
    u't͡ssʲ', u'd͡zzʲ']),
  'optional':set([u'slʲ', u'zlʲ', u'snʲ', u'znʲ', u'nsʲ', u'nzʲ',
    u'mpʲ', u'mbʲ', u'mfʲ', u'fmʲ'])
}

fronting = {
  'a': u'æ',
  u'u': u'ʉ',
  u'ʊ': u'ʉ',
}

skip_pages = [
    u"г-жа"
]

def msg(text):
  print text.encode("utf-8")

def errmsg(text):
  print >>sys.stderr, text.encode("utf-8")

def contains_latin(text):
  return re.search(u"[0-9a-zščžáéíóúýàèìòùỳɛ]", text.lower())

def contains_non_cyrillic(text):
  # 0300 = grave, 0301 = acute, 0302 = circumflex, 0308 = diaeresis
  # We also include basic punctuation as well as IPA chars ɣ ɕ ʑ, which
  # we allow in Cyrillic pronunciation
  return re.sub(ur"[\u0300\u0301\u0302\u0308 \-,.?!ɣɕʑЀ-џҊ-ԧꚀ-ꚗ]", "", text) != ""

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

    # consonant assimilative palatalisation of tn/dn, depending on
    # whether [rl] precedes
    manword = re.sub(u"([rl]?)([ˈˌ]?[dt])ʲ?([ˈˌ]?nʲ)",
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

def get_headword_pronuns(parsed, pagetitle, pagemsg, expand_text):
  # Get the headword pronunciation(s)
  headword_pronuns = set()
  headword_translit = set()

  for t in parsed.filter_templates():
    found_template = False
    if unicode(t.name) in ["ru-noun", "ru-proper noun", "ru-adj", "ru-adv", "ru-verb"]:
      tr = getparam(t, "tr")
      if tr:
        pagemsg("WARNING: Using Latin for pronunciation, based on tr=%s" % (
          tr))
        headword_translit.add(tr)
      else:
        headword_pronuns.add(getparam(t, "1") or pagetitle)
      found_template = True
    elif unicode(t.name) in ["ru-noun form", "ru-phrase"]:
      tr = getparam(t, "tr")
      if tr:
        pagemsg("WARNING: Using Latin for pronunciation, based on tr=%s" % (
          tr))
        headword_translit.add(tr)
      else:
        headword_pronuns.add(getparam(t, "head") or getparam(t, "1") or pagetitle)
      found_template = True
    elif unicode(t.name) == "head" and getparam(t, "1") == "ru" and getparam(t, "2") == "letter":
      pagemsg("WARNING: Skipping page with letter headword")
      return None
    elif unicode(t.name) == "head" and getparam(t, "1") == "ru":
      tr = getparam(t, "tr")
      if tr:
        pagemsg("WARNING: Using Latin for pronunciation, based on tr=%s" % (
          tr))
        headword_translit.add(tr)
      else:
        headword_pronuns.add(getparam(t, "head") or pagetitle)
      found_template = True
    elif unicode(t.name) in ["ru-noun+", "ru-proper noun+"]:
      if unicode(t.name) == "ru-noun+":
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
        if "//" in head:
          _, tr = re.split("//", head)
          pagemsg("WARNING: Using Latin for pronunciation, based on translit %s" % tr)
          headword_translit.add(tr)
        else:
          headword_pronuns.add(head)

    if found_template:
      for i in xrange(2, 10):
        trn = getparam(t, "tr" + str(i))
        if trn:
          pagemsg("WARNING: Using Latin for pronunciation, based on tr%s=%s" % (
            str(i), trn))
          headword_translit.add(trn)
        else:
          headn = getparam(t, "head" + str(i))
          if headn:
            headword_pronuns.add(headn)

  # Do the following two sections before adding semi-reduced inflection
  # since ru.* may not be aware of dot-under.

  # Canonicalize by removing links and final !, ?
  headword_pronuns = set(re.sub("[!?]$", "", blib.remove_links(x)) for x in headword_pronuns)
  for pronun in headword_pronuns:
    if ru.remove_accents(pronun) != pagetitle:
      pagemsg("WARNING: Headword pronun %s doesn't match page title, skipping" % pronun)
      return None

  # Check for acronym/non-syllabic.
  for pronun in headword_pronuns:
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
    def update_semireduced(pron):
      return re.sub(ur"я(т|м|ми|х)( |$)", ur"я̣\1\2", pron)
    new_headword_pronuns = set(update_semireduced(x) for x in headword_pronuns)
    if new_headword_pronuns != headword_pronuns:
      pagemsg("Using semi-reduced pronunciation: %s" % ",".join(new_headword_pronuns))
      headword_pronuns = new_headword_pronuns

  headword_pronuns.update(headword_translit)
  if len(headword_pronuns) < 1:
    pagemsg("WARNING: Can't find headword template")
    return None
  headword_pronuns = sorted(list(headword_pronuns))
  return headword_pronuns

def process_section(section, headword_pronuns, override_ipa, pagetitle, pagemsg, expand_text):
  subbed_ipa_pronuns = []
  overrode_ipa = False

  def compute_ipa():
    computed_ipa = {}
    for pronun in headword_pronuns:
      result = expand_text("{{#invoke:ru-pron|ipa|%s}}" % pronun)
      if not result:
        return False
      computed_ipa[pronun] = result
    return computed_ipa

  pronun_lines = []
  latin_char_msgs = []
  for pronun in headword_pronuns:
    if pronun.startswith("-") or pronun.endswith("-"):
      pagemsg("WARNING: Skipping prefix or suffix: %s" % pronun)
      return None
    if "." in pronun:
      pagemsg("WARNING: Pronunciation has dot in it, skipping: %s" % pronun)
      return None
    if ru.needs_accents(pronun, split_dash=True):
      pagemsg("WARNING: Pronunciation lacks accents, skipping: %s" % pronun)
      return None
    if contains_latin(pronun):
      latin_char_msgs.append(
          "WARNING: Pronunciation %s to be added contains Latin chars, skipping" %
            pronun)
    elif contains_non_cyrillic(pronun):
      latin_char_msgs.append(
          "WARNING: Pronunciation %s to be added contains non-Cyrillic non-Latin chars, skipping" %
            pronun)
    pronun_lines.append("* {{ru-IPA|%s}}\n" % pronun)


  # Check for indications of pre-reform spellings
  for cat in [u"Russian spellings with е instead of ё",
      u"Russian terms spelled with Ѣ",
      u"Russian terms spelled with Ѳ",
      u"Russian pre-1918 spellings"]:
    if re.search(ur"\[\[Category:%s]]" % cat, section):
      pagemsg(u"WARNING: Found [[Category:%s]], skipping" % cat)
      return None

  parsed = blib.parse_text(section)
  ipa_templates = []
  for t in parsed.filter_templates():
    if unicode(t.name) == "ru-pre-reform":
      pagemsg("WARNING: Found pre-reform template, skipping")
      return None
    if unicode(t.name) == u"ru-noun-alt-ё":
      pagemsg(u"WARNING: Found ru-noun-alt-ё template, skipping")
      return None
    if unicode(t.name) == "ru-IPA-manual":
      pagemsg("WARNING: Found ru-IPA-manual template, skipping")
      return None
    if unicode(t.name) in ["alternative form of", "alternative spelling of"] and getparam(t, "lang") == "ru":
      # Check if word spelled with е instead of ё, without using
      # [[Category:Russian spellings with е instead of ё]], which we
      # catch above.
      target = getparam(t, "1")
      if u"ё" in target and re.sub(u"ё", u"е", target) == pagetitle:
        pagemsg(u"WARNING: Found apparent alternative form using е in place of ё without explicit category, skipping: %s" %
            unicode(t))
        return None
    if unicode(t.name) == "IPA" and getparam(t, "lang") == "ru":
      ipa_templates.append(t)
  if (re.search(r"[Aa]bbreviation", section) and not
      re.search("==Abbreviations==", section)):
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
      "++".join(headword_pronuns)))
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
            overrode_ipa = True
          pagemsg("Replaced %s with %s" % (
            orig_ipa_template, unicode(ipa_template)))
          num_replaced += 1
          mismatch_msgs = []
          subbed_ipa_pronuns.append(headword)
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
    return None

  foundpronuns = []
  for m in re.finditer(r"(\{\{ru-IPA(?:\|([^}]*))?\}\})", section):
    pagemsg("Already found pronunciation template: %s" % m.group(1))
    foundpronuns.append(m.group(2) or pagetitle)
  foundpronuns = sorted(list(set(foundpronuns)))
  if foundpronuns:
    joined_foundpronuns = ",".join(foundpronuns)
    joined_headword_pronuns = ",".join(headword_pronuns)
    if "phon=" not in joined_foundpronuns and contains_latin(joined_headword_pronuns):
      pagemsg("WARNING: Existing pronunciation template %s probably needs phon= because headword-derived pronunciation %s contains Latin" % (
        joined_foundpronuns, joined_headword_pronuns))
    if "phon=" in joined_foundpronuns and not contains_latin(joined_headword_pronuns):
      pagemsg("WARNING: Existing pronunciation template has pronunciation %s with phon=, headword-derived pronunciation %s isn't Latin, probably need manual translit in headword and decl" %
          (joined_foundpronuns, joined_headword_pronuns))
    if foundpronuns != headword_pronuns:
      pagemsg("WARNING: Existing pronunciation template has different pronunciation %s from headword-derived pronunciation %s" %
            (joined_foundpronuns, joined_headword_pronuns))
    return None
  pronunsection = "===Pronunciation===\n%s\n" % "".join(pronun_lines)

  if latin_char_msgs:
    for latinmsg in latin_char_msgs:
      pagemsg(latinmsg)
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

  return section, subbed_ipa_pronuns, overrode_ipa

def process_page_text(index, text, pagetitle, verbose, override_ipa):
  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  def expand_text(tempcall):
    if verbose:
      pagemsg("Expanding text: %s" % tempcall)
    result = site.expand_text(tempcall, title=pagetitle)
    if verbose:
      pagemsg("Raw result is %s" % result)
    if result.startswith('<strong class="error">'):
      result = re.sub("<.*?>", "", result)
      pagemsg("WARNING: Got error: %s" % result)
      return False
    return result

  parsed = blib.parse_text(text)

  # Get the headword pronunciation(s)
  headword_pronuns = get_headword_pronuns(parsed, pagetitle, pagemsg, expand_text)
  if headword_pronuns is None:
    return None

  subbed_ipa_pronuns = []
  overrode_ipa = False

  foundrussian = False
  sections = re.split("(^==[^=]*==\n)", text, 0, re.M)
  orig_text = text
  for j in xrange(2, len(sections), 2):
    if sections[j-1] == "==Russian==\n":
      if foundrussian:
        pagemsg("WARNING: Found multiple Russian sections")
        return None
      foundrussian = True

      if "===Pronunciation 1===" in sections[j]:
        pagemsg("WARNING: Found ===Pronunciation 1===, should convert page to multiple etymologies")
        return None
      if "===Etymology 1===" in sections[j]:
        etymsections = re.split("(^===Etymology [0-9]+===\n)", sections[j], 0, re.M)
        pagemsg("Found multiple etymologies (%s)" % (len(etymsections)//2))
        if len(etymsections) < 5:
          pagemsg("WARNING: Misformatted page with multiple etymologies")
          return None
        # Check if all per-etym-section headwords are the same
        etymparsed2 = blib.parse_text(etymsections[2])
        etym_headword_pronuns = {}
        etym_headword_pronuns[2] = get_headword_pronuns(etymparsed2, pagetitle, pagemsg, expand_text)
        need_per_section_pronuns = False
        for k in xrange(4, len(etymsections), 2):
          etymparsed = blib.parse_text(etymsections[k])
          etym_headword_pronuns[k] = get_headword_pronuns(etymparsed, pagetitle, pagemsg, expand_text)
          if etym_headword_pronuns[k] != etym_headword_pronuns[2]:
            pagemsg("WARNING: Etym section %s pronuns %s different from etym section 1 pronuns %s" % (
              k//2, ",".join(etym_headword_pronuns[k]), ",".join(etym_headword_pronuns[2])))
            need_per_section_pronuns = True
        numpronunsecs = len(re.findall("^===Pronunciation===$", etymsections[0], re.M))
        if numpronunsecs > 1:
          pagemsg("WARNING: Multiple ===Pronunciation=== sections in preamble to multiple etymologies, needs to be fixed")
          return None
        if need_per_section_pronuns:
          pagemsg("Multiple etymologies, split pronunciations needed")
        else:
          pagemsg("Multiple etymologies, combined pronunciation possible")
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
            foundpronuns.append(m.group(2) or pagetitle)
          foundpronuns = set(foundpronuns)
          if foundpronuns:
            joined_foundpronuns = ",".join(sorted(list(foundpronuns)))
            joined_headword_pronuns = ",".join(sorted(list(headword_pronuns)))
            if foundpronuns != headword_pronuns:
              pagemsg("WARNING: When trying to delete pronunciation section, existing pronunciation template has different pronunciation %s from headword-derived pronunciation %s" %
                    (joined_foundpronuns, joined_headword_pronuns))
              return None
          etymsections[0] = re.sub(r"(^===Pronunciation===\n)(.*?)(^==|\Z)", r"\3", etymsections[0], 1, re.M | re.S)
          pagemsg("Removed pronunciation section because combined pronunciation with multiple etymologies needs to be split")
        if need_per_section_pronuns:
          for k in xrange(2, len(etymsections), 2):
            result = process_section(etymsections[k], etym_headword_pronuns[k],
                override_ipa, pagetitle, pagemsg, expand_text)
            if result is None:
              continue
            etymsections[k], etymsection_subbed_ipa_pronuns, this_overrode_ipa = result
            subbed_ipa_pronuns.extend(etymsection_subbed_ipa_pronuns)
            overrode_ipa = overrode_ipa or this_overrode_ipa
          sections[j] = "".join(etymsections)
          text = "".join(sections)
        else:
          result = process_section(sections[j], headword_pronuns,
              override_ipa, pagetitle, pagemsg, expand_text)
          if result is None:
            continue
          sections[j], section_subbed_ipa_pronuns, this_overrode_ipa = result
          subbed_ipa_pronuns.extend(section_subbed_ipa_pronuns)
          overrode_ipa = overrode_ipa or this_overrode_ipa
          text = "".join(sections)

      else:
        result = process_section(sections[j], headword_pronuns,
            override_ipa, pagetitle, pagemsg, expand_text)
        if result is None:
          continue
        sections[j], section_subbed_ipa_pronuns, this_overrode_ipa = result
        subbed_ipa_pronuns.extend(section_subbed_ipa_pronuns)
        overrode_ipa = overrode_ipa or this_overrode_ipa
        text = "".join(sections)

  if not foundrussian:
    pagemsg("WARNING: Can't find Russian section")
    return None

  if subbed_ipa_pronuns:
    comment = "Replace {{IPA|...}} with {{ru-IPA|...}} for %s%s" % (
        ",".join(subbed_ipa_pronuns),
        " (IPA override)" if overrode_ipa else "")
  else:
    comment = "Add pronunciation %s" % ",".join(headword_pronuns)
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
  for category in ["Russian lemmas", "Russian non-lemma forms"]:
    msg("Processing category: %s" % category)
    for i, page in blib.cat_articles(category, start, end):
      process_page(i, page, args.save, args.verbose, args.override_IPA)
