#!/usr/bin/env python
# -*- coding: utf-8 -*-

import pywikibot, re, sys, codecs, argparse

import blib
from blib import getparam, rmparam

site = pywikibot.Site()

def msg(text):
  print text.encode("utf-8")

def errmsg(text):
  print >>sys.stderr, text.encode("utf-8")

def process_page(index, page, save, verbose):
  pagetitle = unicode(page.title())
  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  text = unicode(page.text)
  parsed = blib.parse(page)

  for t in parsed.filter_templates():
    if unicode(t.name) == "ru-IPA":
      origt = unicode(t)
      if getparam(t, "phon"):
        pagemsg("phon= already present: %s" % unicode(t))
      else:
        phon = getparam(t, "1")
        pagemsg("Adding phon=: %s" % unicode(t))
        rmparam(t, "1")
        t.add("phon", phon)
        pagemsg("Replaced %s with %s" % (origt, unicode(t)))

  newtext = unicode(parsed)

  if newtext != text:
    if verbose:
      pagemsg("Replacing <<%s>> with <<%s>>" % (text, newtext))
    comment = "Add phon= to ru-IPA templates"
    if save:
      pagemsg("Saving with comment = %s" % comment)
      page.text = newtext
      page.save(comment=comment)
    else:
      pagemsg("Would save with comment = %s" % comment)
  else:
    pagemsg("Skipping")

parser = argparse.ArgumentParser(description="Add phon= to ru-IPA uses")
parser.add_argument('start', help="Starting page index", nargs="?")
parser.add_argument('end', help="Ending page index", nargs="?")
parser.add_argument('--save', action="store_true", help="Save results")
parser.add_argument('--verbose', action="store_true", help="More verbose output")
parser.add_argument('--pagefile', help="File containing pages to process, one per line")
args = parser.parse_args()
start, end = blib.parse_start_end(args.start, args.end)

pages = [x.strip() for x in codecs.open(args.pagefile, "r", "utf-8")]
for i, page in blib.iter_items(pages, start, end):
  msg("Page %s %s: Processing" % (i, page))
  process_page(i, pywikibot.Page(site, page), args.save, args.verbose)
