#!/usr/bin/env python
# -*- coding: utf-8 -*-

import pywikibot, re, sys, codecs, argparse

import blib
from blib import getparam, rmparam

import rulib as ru

site = pywikibot.Site()

def msg(text):
  print text.encode("utf-8")

def errmsg(text):
  print >>sys.stderr, text.encode("utf-8")

def contains_latin(text):
  return re.search(u"[0-9a-zščžáéíóúýàèìòùỳɛ]", text.lower())

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
    pronun_lines.append("* {{ru-IPA|%s}}\n" % pronun)

  foundrussian = False
  sections = re.split("(^==[^=]*==\n)", text, 0, re.M)
  newtext = text
  for j in xrange(2, len(sections), 2):
    if sections[j-1] == "==Russian==\n":
      if foundrussian:
        pagemsg("WARNING: Found multiple Russian sections")
        return
      foundrussian = True
      foundpronuns = []
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
      if re.search(r"^===Etymology===$", sections[j], re.M):
        sections[j] = re.sub(r"(^===Etymology===\n.*?\n)(==)", r"\1%s\2" % pronunsection, sections[j], 1, re.M | re.S)
      elif re.search(r"^===Alternative forms===$", sections[j], re.M):
        sections[j] = re.sub(r"(^===Alternative forms===\n.*?\n)(==)", r"\1%s\2" % pronunsection, sections[j], 1, re.M | re.S)
      else:
        sections[j] = re.sub(r"(^===)", r"%s\1" % pronunsection, sections[j], 1, re.M)
      sections[j] = re.sub("^===", "\n===", sections[j], 1)
      newtext = "".join(sections)
      if latin_char_msgs:
        for latinmsg in latin_char_msgs:
          pagemsg(latinmsg)

  if not foundrussian:
    pagemsg("WARNING: Can't find Russian section")
    return

  if text == newtext:
    pagemsg("WARNING: Something wrong, couldn't sub in pronunciation section")
    return

  if verbose:
    pagemsg("Replacing [[%s]] with [[%s]]" % (text, newtext))

  comment = "Add pronunciation %s" % ",".join(headword_pronuns)
  if save:
    pagemsg("Saving with comment = %s" % comment)
    page.text = newtext
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
