#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Convert plstem from 5th argument to 4th

import pywikibot, re, sys, codecs, argparse

import blib
from blib import getparam, rmparam

import rulib as ru

site = pywikibot.Site()

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

def process_page(templates, index, page, save=False, verbose=False):
  pagetitle = unicode(page.title())
  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  if not page.exists():
    pagemsg("WARNING: Page doesn't exist")
    return

  parsed = blib.parse(page)

  should_save = False

  for t in parsed.filter_templates():

    if unicode(t.name) in templates:
      origt = unicode(t)
      # Punt if multi-arg-set, can't handle yet
      should_continue = False
      for param in t.params:
        if not param.showkey:
          val = unicode(param.value)
          if val == "or":
            pagemsg("WARNING: Can't handle multi-decl templates: %s" % unicode(t))
            should_continue = True
            break
          if val == "-" or val == "_" or val.startswith("join:"):
            pagemsg("WARNING: Can't handle multi-word templates: %s" % unicode(t))
            should_continue = True
            break
      if should_continue:
        continue

      if arg1_is_stress(getparam(t, "1")):
        oldplarg = "5"
        newplarg = "4"
      else:
        oldplarg = "4"
        newplarg = "3"
      plstem = getparam(t, oldplarg)
      if plstem:
        if getparam(t, newplarg):
          pagemsg("WARNING: Something wrong, found args in both positions %s and %s: %s" %
              (newplarg, oldplarg, unicode(t)))
          continue
        rmparam(t, oldplarg)
        t.add(newplarg, plstem)
        should_save = True
        pagemsg("Replacing %s with %s" % (origt, unicode(t)))

  if should_save:
    comment = "Move plstem from 5th/4th argument to 4th/3rd"
    if save:
      pagemsg("Saving with comment = %s" % comment)
      page.text = unicode(parsed)
      page.save(comment=comment)
    else:
      pagemsg("Would save with comment = %s" % comment)

pa = argparse.ArgumentParser()
pa.add_argument("start", nargs="?", help="Start index", type=int)
pa.add_argument("end", nargs="?", help="Start index", type=int)
pa.add_argument("--save", help="Save pages", action="store_true")
pa.add_argument("--verbose", help="Output verbose messages", action="store_true")
pargs = pa.parse_args()

templates_to_do = ["ru-noun-table", "ru-noun-old", "ru-noun+", "ru-proper noun+"]
def yield_ref_pages():
  for template in templates_to_do:
    for i, page in blib.references("Template:" + template, pargs.start or None,
        pargs.end or None):
      yield i, page

do_pages = yield_ref_pages()
for i, page in do_pages:
  msg("Page %s %s: Processing" % (i, unicode(page.title())))
  process_page(templates_to_do, i, page, save=pargs.save, verbose=pargs.verbose)
