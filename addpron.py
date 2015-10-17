#!/usr/bin/env python
# -*- coding: utf-8 -*-

import pywikibot, mwparserfromhell, re, string, sys, codecs, urllib2, datetime, json

import blib
from blib import getparam, rmparam

import rulib as ru

site = pywikibot.Site()
save = False
verbose = True

def msg(text):
  print text.encode("utf-8")

def errmsg(text):
  print >>sys.stderr, text.encode("utf-8")

def process_page(index, page):
  pagetitle = unicode(page.title())
  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  if not page.exists():
    pagemsg("WARNING: Page doesn't exist")
    return

  text = unicode(page.text)
  parsed = blib.parse_text(text)

  # Get the headword pronunciation(s)
  headword_pronuns = set()
  for t in parsed.filter_templates():
    found_template = False
    if unicode(t.name) in ["ru-noun", "ru-proper noun", "ru-adj", "ru-adv", "ru-verb", "ru-phrase"]:
      headword_pronuns.add(blib.remove_links(getparam(t, "1") or pagetitle))
      found_template = True
    elif unicode(t.name) == "head" and getparam(t, "1") == "ru":
      headword_pronuns.add(blib.remove_links(getparam(t, "head") or pagetitle))
      found_template = True
    elif unicode(t.name) == "ru-noun+":
      pagemsg("WARNING: Don't know how to handle ru-noun+ yet: %s" % unicode(t))
      # FIXME, we should use ru-generate-form to extract the lemma; this
      # means we need to create ru-generate-form and add "lemma" as a possible
      # value
      return
    if found_template:
      for i in xrange(2, 10):
        headn = getparam(t, "head" + str(i))
        if headn:
          headword_pronuns.add(blib.remove_links(headn))
      if getparam(t, "tr"):
        # FIXME, ru-IPA should take a tr parameter and use it in preference to
        # the Cyrillic
        pagemsg("WARNING: Don't know how to handle tr= param yet: %s" % unicode(t))
        return
      for i in xrange(2, 10):
        trn = getparam(t, "tr" + str(i))
        if trn:
          pagemsg("WARNING: Don't know how to handle tr%s= param yet: %s" % (
            i, unicode(t)))
          return
  if len(headword_pronuns) < 1:
    pagemsg("WARNING: Can't find headword template")
    return
  headword_pronuns = sorted(list(headword_pronuns))
  pronun_lines = []
  for pronun in headword_pronuns:
    if ru.needs_accents(pronun):
      pagemsg("WARNING: Pronunciation lacks accents, skipping: %s" % pronun)
      return
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
          pagemsg("WARNING: Existing pronunciation template has different pronunciation %s from headword-derived pronunciation %s" %
                (",".join(foundpronuns), ",".join(headword_pronuns)))
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
        sections[j] = re.sub(r"(^\n*)", r"\1%s" % pronunsection, sections[j])
      newtext = "".join(sections)

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


assert sys.argv[1]
pages = [x.strip() for x in codecs.open(sys.argv[1], "r", "utf-8")]
i = 0
for page in pages:
  i += 1
  msg("Page %s %s: Processing" % (i, page))
  if i % 50 == 0:
    errmsg("[%s]" % i)
  process_page(i, pywikibot.Page(site, page))
