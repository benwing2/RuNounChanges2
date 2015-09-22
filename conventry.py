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

def arg1_is_stress(arg1):
  if not arg1:
    return False
  for arg in re.split(",", arg1):
    if not (re.search("^[a-f]'?'?$", arg) or re.search(r"^[1-6]\*?$", arg)):
      return False
  return True
end

conv_decl = {
    #u"":[u""], # should be caught earlier
    u"ь-m":[u"ь", "m"],
    u"ь-f":[u"ь", "f"],
    u"й":[u"й"],
    u"ъ":[u"ъ"],
    u"-а":[u"", "(1)"],
    u"ъ-а":[u"ъ", "(1)"],
    u"ь-я":[u"ь", ["m", "(1)"]],
    u"й-я":[u"й", "(1)"],
    # u"-ья":[u"", "-ья"], # should be caught earlier
    u"ъ-ья":[u"ъ", "-ья"],
    u"а":[u"а"],
    u"я":[u"я"],
    u"о":[u"о"],
    u"о-ы":[u"о", "(1)"],
    u"о-и":[u"о", "(1)"],
    u"о-ья":[u"о", "-ья"],
    u"е":[u"е"],
    u"е́":[u"е́"],
    u"ё":[u"ё"],
    u"ья":[u"ья"],
    u"ье":[u"ье"],
    u"ьё":[u"ьё"],
}

def process_page(index, page):
  pagetitle = unicode(page.title())
  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, page, txt))

  if not page.exists():
    pagemsg("WARNING: Page doesn't exist")
    return

  text = unicode(page.text)
  newtext = text
  parsed = blib.parse(text)

  for t in parsed.filter_templates():
    if unicode(t.name) == "ru-noun-table":

      # Punt if multi-arg-set, can't handle yet
      should_continue = False
      for param in t.params:
        if unicode(param.value) == "or":
          pagemsg("WARNING: Can't handle multi-decl templates: %s" % unicode(t))
          should_continue = True
          break
      if should_continue:
        continue

      # Retrieve params; if first arg isn't stress, then it's lemma
      # and shift everything down
      stress = getparam(t, "1")
      lemma = getparam(t, "2")
      decl = getparam(t, "3")
      bare = getparam(t, "4")
      plstem = getparam(t, "5")
      if not arg1_is_stress(stress):
        assert not plstem
        plstem = bare
        bare = decl
        decl = lemma
        lemma = stress
        stress = ""
      if not lemma:
        lemma = pagetitle

      # Extract specials from decl, put into newdecl
      newdecl = ""
      decl, nsubs = re.subn(ur"(\d|[^/\w])ё(\d|[^/\w]|$)", r"\1\2", decl)
      if nsubs:
        newdecl += u";ё"
      decl, nsubs = re.subn(r"\(1\)", "", decl)
      if nsubs:
        newdecl += "(1)"
      decl, nsubs = re.subn(r"\(2\)", "", decl)
      if nsubs:
        newdecl += "(2)"
      decl, nsubs = re.subn(r"\*", "", decl)
      if nsubs:
        newdecl += "*"
      decl = re.sub(";", "", decl)

      # If remaining decl is new format (opt. GENDER then opt. -ья), do nothing
      m = re.match(u"^([mfn]|3f|)(-ья)?", decl)
      if m:
        newdecl = decl + newdecl
      # If slash decl, also do nothing
      elif "/" in decl:
        pagemg("Found slash decl in template, not changing: %s" % unicode(t))
        newdecl = decl + newdecl
      # Else, single old-style decl; convert to new by transfering the ending
      # to the end of the lemma and incorporating and new special marker if
      # needed
      else:
        ending_stressed = ru.is_stressed(decl)
        if decl != u"е́":
          decl = ru.remove_accents(decl)
        if decl not in conv_decl:
          pagemsg("WARNING: Unrecognized declension %s in %s" %
              (decl, unicode(t)))
          continue
        declconv = conv_decl[decl]
        # FIXME: If declconv[0] is "-ин" we might not be able to transfer it
        lemma = lemma + declconv[0]
        if len(declconv) == 0:
          special = []
        elif type(declconv[1]) is list:
          special = declconv[1]
        else:
          special = [declconv[1]]
          for spec in ["m", "f", u"-ья", "(1)"]:
            if spec in special:
              if spec not in newdecl:
                newdecl += spec
              special = [x for x in special if x ~= spec]
          assert not special

        FIXME

      FIXME

  if verbose:
    pagemsg("Replacing [[%s]] with [[%s]]" % text, newtext)

  comment = "Add pronunciation %s" % pronun
  if save:
    pagemsg("Saving with comment = %s" % comment)
    page.text = newtext
    page.save(comment=comment)
  else:
    pagemsg("Would save with comment = %s" % comment)

pages = [x.strip() for x in codecs.open(sys.argv[1], "r", "utf-8")]
i = 0
for page in pages:
  i += 1
  msg("Page %s %s: Processing" % (i, page))
  process_page(i, pywikibot.Page(site, page))
