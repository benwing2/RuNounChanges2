#!/usr/bin/env python
#coding: utf-8

import pywikibot, mwparserfromhell, re, string, sys, codecs, urllib2, datetime, json

import blib

site = pywikibot.Site()

def msg(text):
  print text.encode("utf-8")

def errmsg(text):
  print >>sys.stderr, text.encode("utf-8")

def process_file(index, page):
  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, page, txt))

  pass

pages = [x.strip() for x in codecs.open(sys.argv[1], "utf-8")]
i = 0
for page in pages:
  i += 1
  msg("Page %s %s: Processing" % (i, page))
  process_file(i, page)
