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
non_ipa_vowels_re = "[^" + ipa_vowel_list + "]"

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

def process_page(index, page, save, verbose):
  pagetitle = unicode(page.title())
  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  if not page.exists():
    pagemsg("WARNING: Page doesn't exist")
    return

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

  text = unicode(page.text)
  parsed = blib.parse_text(text)

  # Get the headword pronunciation(s)
  headword_pronuns = set()
  for t in parsed.filter_templates():
    found_template = False
    if unicode(t.name) in ["ru-noun", "ru-proper noun", "ru-adj", "ru-adv", "ru-verb", "ru-phrase"]:
      tr = getparam(t, "tr")
      if tr:
        pagemsg("WARNING: Using Latin for pronunciation, based on tr=%s" % (
          tr))
        headword_pronuns.add(tr)
      else:
        headword_pronuns.add(blib.remove_links(getparam(t, "1") or pagetitle))
      found_template = True
    elif unicode(t.name) == "head" and getparam(t, "1") == "ru":
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
        return
      args = {}
      for arg in re.split(r"\|", generate_result):
        name, value = re.split("=", arg)
        args[name] = re.sub("<!>", "|", value)
      lemma = args["nom_sg"] if "nom_sg" in args else args["nom_pl"]
      for head in re.split(",", lemma):
        if "//" in head:
          _, tr = re.split("//", head)
          pagemsg("WARNING: Using Latin for pronunciation, based on transit %s" % tr)
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
    return
  headword_pronuns = sorted(list(headword_pronuns))
  subbed_ipa_pronuns = []
  pronun_lines = []
  latin_char_msgs = []
  for pronun in headword_pronuns:
    if ru.needs_accents(pronun):
      pagemsg("WARNING: Pronunciation lacks accents, skipping: %s" % pronun)
      return
    if contains_latin(pronun):
      latin_char_msgs.append(
          "WARNING: Pronunciation %s to be added contains Latin chars" %
            pronun)
    elif contains_non_cyrillic(pronun):
      latin_char_msgs.append(
          "WARNING: Pronunciation %s to be added contains non-Cyrillic non-Latin chars" %
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

  def ipa_matches(headword, manual, auto):
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
    manual = re.sub(u"(^| )ə", ur"\1ɐ", manual)
    manual = re.sub(u"ɐː", u"ɐɐ", manual)
    manual = re.sub(u"ɛ̝", u"ɛ", manual)
    manual = re.sub(u"e̞", u"e", manual)
    # Convert regular g to IPA ɡ (looks same but different char)
    manual = re.sub("g", u"ɡ", manual)
    # Both ɡ's below are IPA ɡ's
    manual = re.sub(u"ŋɡ", u"nɡ", manual)
    manual = re.sub(u"nt͡sk", u"n(t)sk", manual)
    # If both auto and manual are monosyllabic, canonicalize by
    # removing primary accent
    if (len(re.sub(non_ipa_vowels_re, "", manual)) == 1 and
        len(re.sub(non_ipa_vowels_re, "", auto)) == 1):
      auto = re.sub(u"ˈ", "", auto)
      manual = re.sub(u"ˈ", "", manual)
    # Canonicalize by moving stress at the beginning of all consonant
    # clusters
    auto = re.sub("(" + non_ipa_vowels_re + u"+)([ˈˌ])", r"\2\1", auto)
    manual = re.sub("(" + non_ipa_vowels_re + u"+)([ˈˌ])", r"\2\1", manual)
    if auto == manual:
      pagemsg("For headword %s, %s equal to %s" % (headword,
        "auto %s" % auto if auto == orig_auto else
          "canon auto %s (orig %s)" % (auto, orig_auto),
        "manual" if manual == orig_manual else
          "canon manual (orig %s)" % (orig_manual)))
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
    return "WARNING: For headword %s, auto %s%s not same as manual %s%s: %s" % (
      headword, auto, orig_auto != auto and " (%s)" % orig_auto or "",
      manual, orig_manual != manual and " (%s)" % orig_manual or "",
      ", ".join(changes))

  foundrussian = False
  sections = re.split("(^==[^=]*==\n)", text, 0, re.M)
  orig_text = text
  for j in xrange(2, len(sections), 2):
    if sections[j-1] == "==Russian==\n":
      if foundrussian:
        pagemsg("WARNING: Found multiple Russian sections")
        return
      foundrussian = True
      foundpronuns = []
      parsed = blib.parse_text(sections[j])
      ipa_templates = []
      for t in parsed.filter_templates():
        if unicode(t.name) == "ru-pre-reform":
          pagemsg("Found pre-reform template, skipping")
          return
        if unicode(t.name) == "IPA" and getparam(t, "lang") == "ru":
          ipa_templates.append(t)
      if ipa_templates:
        pagemsg("Processing raw IPA %s for headword(s) %s" % (
          "++".join([unicode(x) for x in ipa_templates]),
          "++".join(headword_pronuns)))
        computed_ipa = compute_ipa()
        num_replaced = 0
        if not computed_ipa:
          # Error occurred computing IPA of headwords
          return
        for ipa_template in ipa_templates:
          mismatch_msgs = []
          for headword, autoipa in computed_ipa.items():
            retval = ipa_matches(headword, getparam(ipa_template, "1"), autoipa)
            if retval == True:
              orig_ipa_template = unicode(ipa_template)
              rmparam(ipa_template, "lang")
              rmparam(ipa_template, "1")
              if len(ipa_template.params) > 0:
                pagemsg("WARNING: IPA template has extraneous parameters, skipping: %s" %
                    orig_ipa_template)
                return
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

      for m in re.finditer(r"(\{\{ru-IPA\|([^}]*)\}\})", sections[j]):
        pagemsg("Already found pronunciation template: %s" % m.group(1))
        foundpronuns.append(m.group(2))
      foundpronuns = sorted(foundpronuns)
      if foundpronuns:
        if foundpronuns != headword_pronuns:
          joined_foundpronuns = ",".join(foundpronuns)
          joined_headword_pronuns = ",".join(headword_pronuns)
          pagemsg("WARNING: Existing pronunciation template has different pronunciation %s from headword-derived pronunciation %s" %
                (joined_foundpronuns, joined_headword_pronuns))
          if "phon=" in joined_foundpronuns and not contains_latin(joined_headword_pronuns):
            pagemsg("WARNING: Existing pronunciation template has pronunciation %s with phon=, headword-derived pronunciation %s isn't Latin, probably need manual translit" %
                (joined_foundpronuns, joined_headword_pronuns))
        return
      if re.search(r"^===+Pronunciation===+$", sections[j], re.M):
        pagemsg("WARNING: Found pronunciation section without ru-IPA")
        return
      pronunsection = "===Pronunciation===\n%s\n" % "".join(pronun_lines)
      if re.search(r"^===Etymology [0-9]+===$", sections[j], re.M):
        pagemsg("WARNING: Found multiple etymology sections, can't handle yet")
        return

      if latin_char_msgs:
        for latinmsg in latin_char_msgs:
          pagemsg(latinmsg)
          return

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
        return
      text = newtext

  if not foundrussian:
    pagemsg("WARNING: Can't find Russian section")
    return

  if orig_text != text:
    if verbose:
      pagemsg("Replacing [[%s]] with [[%s]]" % (orig_text, text))

    if subbed_ipa_pronuns:
      comment = "Replace {{IPA|...}} with {{ru-IPA|...}} for %s" % (
          ",".join(subbed_ipa_pronuns))
    else:
      comment = "Add pronunciation %s" % ",".join(headword_pronuns)
    if save:
      pagemsg("Saving with comment = %s" % comment)
      page.text = text
      page.save(comment=comment)
    else:
      pagemsg("Would save with comment = %s" % comment)

parser = argparse.ArgumentParser(description="Add pronunciation sections to Russian Wiktionary entries")
parser.add_argument('--pagefile', help="File containing pages to process, one per line")
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
else:
  for category in ["Russian lemmas", "Russian non-lemma forms"]:
    msg("Processing category: %s" % category)
    for i, page in blib.cat_articles(category, start, end):
      msg("Page %s %s: Processing" % (i, unicode(page.title())))
      process_page(i, page, args.save, args.verbose)
