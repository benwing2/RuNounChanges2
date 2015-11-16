#!/usr/bin/env python
# -*- coding: utf-8 -*-

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
    'compulsory':set([u'stʲ', u'zdʲ', u'nt͡ɕ', u'nɕ', u'ntʲ', u'ndʲ',
      u't͡ssʲ', u'd͡zzʲ']),
    'optional':set([u'slʲ', u'zlʲ', u'snʲ', u'znʲ', u'nsʲ', u'nzʲ',
      u'mpʲ', u'mbʲ', u'mfʲ', u'fmʲ'])
}

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

def process_page_text(index, text, pagetitle, verbose):
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
  headword_pronuns = set()
  for t in parsed.filter_templates():
    found_template = False
    if unicode(t.name) in ["ru-noun", "ru-proper noun", "ru-adj", "ru-adv", "ru-verb"]:
      tr = getparam(t, "tr")
      if tr:
        pagemsg("WARNING: Using Latin for pronunciation, based on tr=%s" % (
          tr))
        headword_pronuns.add(tr)
      else:
        headword_pronuns.add(blib.remove_links(getparam(t, "1") or pagetitle))
      found_template = True
    elif unicode(t.name) in ["ru-noun form", "ru-phrase"]:
      tr = getparam(t, "tr")
      if tr:
        pagemsg("WARNING: Using Latin for pronunciation, based on tr=%s" % (
          tr))
        headword_pronuns.add(tr)
      else:
        headword_pronuns.add(blib.remove_links(getparam(t, "head") or getparam(t, "1") or pagetitle))
      found_template = True
    elif unicode(t.name) == "head" and getparam(t, "1") == "ru" and getparam(t, "2") != "letter":
      tr = getparam(t, "tr")
      if tr:
        pagemsg("WARNING: Using Latin for pronunciation, based on tr=%s" % (
          tr))
        headword_pronuns.add(tr)
      else:
        headword_pronuns.add(blib.remove_links(getparam(t, "head") or pagetitle))
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
        return None, None
      args = {}
      for arg in re.split(r"\|", generate_result):
        name, value = re.split("=", arg)
        args[name] = re.sub("<!>", "|", value)
      lemma = args["nom_sg"] if "nom_sg" in args else args["nom_pl"]
      for head in re.split(",", lemma):
        if "//" in head:
          _, tr = re.split("//", head)
          pagemsg("WARNING: Using Latin for pronunciation, based on translit %s" % tr)
          headword_pronuns.add(tr)
        else:
          headword_pronuns.add(head)

    if found_template:
      for i in xrange(2, 10):
        trn = getparam(t, "tr" + str(i))
        if trn:
          pagemsg("WARNING: Using Latin for pronunciation, based on tr%s=%s" % (
            str(i), trn))
          headword_pronuns.add(trn)
        else:
          headn = getparam(t, "head" + str(i))
          if headn:
            headword_pronuns.add(blib.remove_links(headn))
  if len(headword_pronuns) < 1:
    pagemsg("WARNING: Can't find headword template")
    return None, None
  headword_pronuns = sorted(list(headword_pronuns))
  subbed_ipa_pronuns = []
  pronun_lines = []
  latin_char_msgs = []
  for pronun in headword_pronuns:
    if pronun.startswith("-") or pronun.endswith("-"):
      pagemsg("Skipping prefix or suffix: %s" % pronun)
      return None, None
    if ru.needs_accents(pronun):
      pagemsg("WARNING: Pronunciation lacks accents, skipping: %s" % pronun)
      return None, None
    if contains_latin(pronun):
      latin_char_msgs.append(
          "WARNING: Pronunciation %s to be added contains Latin chars, skipping" %
            pronun)
    elif contains_non_cyrillic(pronun):
      latin_char_msgs.append(
          "WARNING: Pronunciation %s to be added contains non-Cyrillic non-Latin chars, skipping" %
            pronun)
    pronun_lines.append("* {{ru-IPA|%s}}\n" % pronun)

  def compute_ipa():
    computed_ipa = {}
    for pronun in headword_pronuns:
      result = expand_text("{{#invoke:ru-pron|ipa|%s}}" % pronun)
      if not result:
        return False
      computed_ipa[pronun] = result
    return computed_ipa

  def ipa_matches(headword, manual, auto, ipa_templates_msg):
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
    manual = re.sub(u"ʌ", u"ɐ", manual)
    manual = re.sub(u"'", u"ˈ", manual)
    manual = re.sub(u"ˈˈ", u"ˈ", manual)
    manual = re.sub(u"ɫ", "l", manual)
    manual = re.sub(u"t͡ʃ|tʃ", u"t͡ɕ", manual)
    # Convert regular g to IPA ɡ (looks same but different char)
    manual = re.sub("g", u"ɡ", manual)
    # Both ɡ's below are IPA ɡ's
    manual = re.sub(u"ŋɡ", u"nɡ", manual)
    manual = re.sub(u"nt͡sk", u"n(t)sk", manual)
    manual = re.sub(u"ntsk", u"n(t)sk", manual)
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

      # Fix bug in auto
      autoword = re.sub(u"ɕ(ː?)ʲə", ur"ɕ\1ə", autoword)

      # palatalization and gemination need to be in the right order
      # Fix bug in auto
      autoword = re.sub(u"ːʲ", u"ʲː", autoword)
      manword = re.sub(u"ːʲ", u"ʲː", manword)

      # front vowel variants
      manword = re.sub(u"([ʲjɕ]ː?)a", ur"\1æ", manword)
      manword = re.sub(u"([ʲjɕ]ː?)[ʊu]", ur"\1ʉ", manword)
      manword = re.sub(u"([ʲjɕ]ː?)o", ur"\1ɵ", manword)

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
          return a + b + c

      #apply general consonant assimilative palatalisation, repeatedly for
      #recursive assimilation
      while True:
        new_manword = re.sub(u'(t͡s|d͡z|[szntdmbpf])ʲ?([ˈˌ]?)(t͡ɕ|[tdǰɕlnszmpbf]ʲ)',
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

  foundrussian = False
  sections = re.split("(^==[^=]*==\n)", text, 0, re.M)
  orig_text = text
  for j in xrange(2, len(sections), 2):
    if sections[j-1] == "==Russian==\n":
      if foundrussian:
        pagemsg("WARNING: Found multiple Russian sections")
        return None, None
      if re.search(ur"\[\[Category:Russian spellings with е instead of ё]]",
          sections[j]):
        pagemsg(u"Found [[Category:Russian spellings with е instead of ё]], skipping")
        return None, None

      foundrussian = True
      foundpronuns = []
      parsed = blib.parse_text(sections[j])
      ipa_templates = []
      for t in parsed.filter_templates():
        if unicode(t.name) == "ru-pre-reform":
          pagemsg("Found pre-reform template, skipping")
          return None, None
        if unicode(t.name) == "alternative form of" and getparam(t, "lang") == "ru":
          # For words spelled with е instead of ё, etc.
          pagemsg("Found alternative form, skipping")
          return None, None
        if unicode(t.name) == "IPA" and getparam(t, "lang") == "ru":
          ipa_templates.append(t)
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
          return None, None
        for ipa_template in ipa_templates:
          mismatch_msgs = []
          for headword, autoipa in computed_ipa.items():
            retval = ipa_matches(headword, getparam(ipa_template, "1"), autoipa,
                ipa_templates_msg)
            if retval == True:
              orig_ipa_template = unicode(ipa_template)
              rmparam(ipa_template, "lang")
              rmparam(ipa_template, "1")
              if len(ipa_template.params) > 0:
                pagemsg("WARNING: IPA template has extra parameters, skipping: %s" %
                    orig_ipa_template)
                return None, None
              if contains_latin(headword):
                pagemsg("WARNING: Headword %s to be subbed into ru-IPA contains Latin chars, skipping" %
                      headword)
                return None, None
              elif contains_non_cyrillic(headword):
                pagemsg("WARNING: Headword %s to be subbed into ru-IPA contains non-Cyrillic non-Latin chars, skipping" %
                      headword)
                return None, None
              ipa_template.name = "ru-IPA"
              ipa_template.add("1", headword)
              pagemsg("Replaced %s with %s" % (
                orig_ipa_template, unicode(ipa_template)))
              num_replaced += 1
              mismatch_msgs = []
              subbed_ipa_pronuns.append(headword)
              break
            else:
              mismatch_msgs.append(retval)
          if mismatch_msgs:
            for m in mismatch_msgs:
              pagemsg(m)
        if num_replaced > 0:
          sections[j] = unicode(parsed)
          text = "".join(sections)
        if num_replaced < len(ipa_templates):
          pagemsg("Unable to replace %s of %s raw IPA template(s)" % (
            len(ipa_templates) - num_replaced, len(ipa_templates)))
        continue

      for m in re.finditer(r"(\{\{ru-IPA(?:\|([^}]*))?\}\})", sections[j]):
        pagemsg("Already found pronunciation template: %s" % m.group(1))
        foundpronuns.append(m.group(2) or pagetitle)
      foundpronuns = sorted(foundpronuns)
      if foundpronuns:
        if foundpronuns != headword_pronuns:
          joined_foundpronuns = ",".join(foundpronuns)
          joined_headword_pronuns = ",".join(headword_pronuns)
          pagemsg("WARNING: Existing pronunciation template has different pronunciation %s from headword-derived pronunciation %s" %
                (joined_foundpronuns, joined_headword_pronuns))
          if "phon=" in joined_foundpronuns and not contains_latin(joined_headword_pronuns):
            pagemsg("WARNING: Existing pronunciation template has pronunciation %s with phon=, headword-derived pronunciation %s isn't Latin, probably need manual translit in headword and decl" %
                (joined_foundpronuns, joined_headword_pronuns))
        return None, None
      if re.search(r"^===+Pronunciation===+$", sections[j], re.M):
        pagemsg("WARNING: Found pronunciation section without ru-IPA")
        return None, None
      pronunsection = "===Pronunciation===\n%s\n" % "".join(pronun_lines)
      if re.search(r"^===Etymology [0-9]+===$", sections[j], re.M):
        pagemsg("WARNING: Found multiple etymology sections, can't handle yet")
        return None, None

      if latin_char_msgs:
        for latinmsg in latin_char_msgs:
          pagemsg(latinmsg)
          return None, None

      if re.search(r"^===Etymology===$", sections[j], re.M):
        sections[j] = re.sub(r"(^===Etymology===\n.*?\n)(==)", r"\1%s\2" % pronunsection, sections[j], 1, re.M | re.S)
      elif re.search(r"^===Alternative forms===$", sections[j], re.M):
        sections[j] = re.sub(r"(^===Alternative forms===\n.*?\n)(==)", r"\1%s\2" % pronunsection, sections[j], 1, re.M | re.S)
      else:
        sections[j] = re.sub(r"(^===)", r"%s\1" % pronunsection, sections[j], 1, re.M)
      sections[j] = re.sub("^===", "\n===", sections[j], 1)
      newtext = "".join(sections)
      if newtext == text:
        pagemsg("WARNING: Something wrong, couldn't sub in pronunciation section")
        return None, None
      text = newtext

  if not foundrussian:
    pagemsg("WARNING: Can't find Russian section")
    return None, None

  if subbed_ipa_pronuns:
    comment = "Replace {{IPA|...}} with {{ru-IPA|...}} for %s" % (
        ",".join(subbed_ipa_pronuns))
  else:
    comment = "Add pronunciation %s" % ",".join(headword_pronuns)
  return text, comment

def process_page(index, page, save, verbose):
  pagetitle = unicode(page.title())

  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  if not page.exists():
    pagemsg("WARNING: Page doesn't exist")
    return

  text = unicode(page.text)
  newtext, comment = process_page_text(index, text, pagetitle, verbose)

  if newtext and newtext != text:
    if verbose:
      pagemsg("Replacing [[%s]] with [[%s]]" % (text, newtext))

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
args = parser.parse_args()
start, end = blib.get_args(args.start, args.end)

if args.pagefile:
  pages = [x.strip() for x in codecs.open(args.pagefile, "r", "utf-8")]
  for i, page in blib.iter_items(pages, start, end):
    msg("Page %s %s: Processing" % (i, page))
    process_page(i, pywikibot.Page(site, page), args.save, args.verbose)

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
      process_page_text(index, text, pagetitle, args.verbose)

else:
  for category in ["Russian lemmas", "Russian non-lemma forms"]:
    msg("Processing category: %s" % category)
    for i, page in blib.cat_articles(category, start, end):
      msg("Page %s %s: Processing" % (i, unicode(page.title())))
      process_page(i, page, args.save, args.verbose)
