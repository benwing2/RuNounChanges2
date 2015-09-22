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

numbered_to_lettered_stress = {
    "1":"a",
    "2":"b",
    "3":"c",
    "4":"d",
    "5":"e",
    "6":"f",
    "4*":"d'",
    "6*":"f'",
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

        # Transfer all the special markers, make sure they're not duplicated
        # and go in the right order
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

      if ending_stressed and not stress:
        stress = "b"

      # Canonicalize and split stress
      newstress = []
      is_3f = lemma.endswith(u"ь") and "f" in newdecl
      if stress:
        for s in re.split(",", stress):
          if s == "2" and is_3f:
            newstress.append("b'")
          elif s == "6" and is_3f:
            newstress.append("f''")
          else:
            newstress.append(numbered_to_lettered_stress.get(s, s))
      # Now, attempt to eliminate explicit stress b by stressing the ending,
      # and eliminate explicit stress a by not stressing the ending:
      #
      # 1. If lemma doesn't end with a vowel, there's no ending to stress.
      #    Accent b must remain, but accent a can be removed as long as
      #    there isn't a missing accent. In the process, remove monosyllabic
      #    accents (unnecessary) and remove the lemma if same as pagename.
      #
      # 2. If lemma ends in a vowel: First, if multi-stressed, warn and do
      #    nothing. If stress is empty or multiple, do nothing. Then,
      #    (1) if last syllable is stressed: if stress is a, c, or e
      #    (stem-stressed), warn that there may be an error somewhere;
      #    else if stress is b, remove it, else do nothing. (2) if last
      #    syllable isn't stressed: extract off stem and ending. If stress
      #    is a, c or e, auto-stress a monosyllabic stem; then if the stem
      #    isn't stressed, issue a warning and do nothing, else remove
      #    stress a. If stress is b, d or f (ending-stressed): If no stress
      #    on stem, no problem -- stress the ending, remove stress b. If
      #    stem has ё, and stress is b or d, check that ё is the last vowel
      #    in the stem; if not, issue a warning and do nothing. If so, remove
      #    ё and add ;ё to newdecl, with a message to this effect. If stem
      #    has ё, and stress is f, check that ё is the first vowel in the stem
      #    *and* that there's no following plain е; if both conditions check
      #    out, remove ё and add ;ё to newdecl, with a message to this effect;
      #    else, issue a warning and do nothing. Else, if no ё, if b or d and
      #    ending stress, or f and beginning stress, we can safely remove the
      #    stress as it will be put back in the same spot by ru-noun.lua;
      #    do so, and issue a message if stem is multi-syllabic, and stress
      #    the ending, remove stress b. Otherwise, can't safely remove stress;
      #    issue a warning and take no further action.

      if not ru.ends_with_vowel(lemma):
        if newstress == ["a"]:
          if ru.needs_accents(lemma):
            pagemsg("WARNING: Missing stress in lemma %s: %s" %
                (lemma, unicode(t)))
          else:
            lemma = remove_monosyllabic_accents(lemma)
            if lemma == pagetitle:
              lemma = ""
            newstress = []
      # else, ends with vowel
      elif ru.is_multi_stressed(lemma):
        pagemsg("WARNING: Multiple stresses in lemma %s: %s" %
            (lemma, unicode(t)))
      elif len(newstress) != 1:
        pass
      elif ru.is_ending_stressed(lemma):
        if newstress[0] in ["a", "c", "e"]:
          pagemsg("WARNING: Ending-stressed lemma %s with stem stress pattern %s, may be error: %s" %
              (lemma, newstress[0], unicode(t)))
        elif newstress == ["b"]:
          newstress = []
      else: # last syllable isn't stressed
        need_ending_stress = False
        m = re.match("^(.*)([" + ru.vowel + "][" + ru.AC + ru.GR +ru.DI + "]?)$", lemma)
        assert m
        stem, ending = m.groups()
        if newstress[0] in ["a", "c", "e"]:
          if ru.is_monosyllabic(stem):
            stem = ru.make_ending_stressed(stem)
            lemma = stem + ending
          if ru.needs_accents(lemma):
            pagemsg("WARNING: Missing stress in lemma %s: %s" %
                (lemma, unicode(t)))
          elif newstress == ["a"]:
            newstress = []
        # else stress is b, d, f or variants
        elif ru.is_unstressed(stem):
          need_ending_stress = True
        elif u"ё" in stem:
          if re.match("^[bd]", newstress[0]):
            if re.search(u"ё.*[" + ru.vowel + "]", stem):
              pagemsg(u"WARNING: ё in stem with later vowel in lemma %s and ending stress %s wanted, can't use ;ё special: %s" %
                  (lemma, newstress[0], unicode(t)))
            else:
              pagemsg(u"Removing ё from stem in lemma %s with ending stress %s wanted and using ;ё special: %s" %
                  (lemma, newstress[0], unicode(t)))
              need_ending_stress = True
          else:
            assert re.match("^f", newstress[0])
            if (re.search("[" + ru.vowel + u"].*ё", stem) or
              re.search(u"ё.*е", stem)):
              pagemsg(u"WARNING: ё in stem with earlier vowel or later е in lemma %s and ending stress %s wanted, can't use ;ё special: %s" %
                  (lemma, newstress[0], unicode(t)))
            else:
              pagemsg(u"Removing ё from stem in lemma %s with ending stress %s wanted and using ;ё special: %s" %
                  (lemma, newstress[0], unicode(t)))
              need_ending_stress = True
        else:
          if (re.match("^[bd]", newstress[0]) and ru.is_ending_stressed(stem) or
              re.match("^f", newstress[0]) and ru.is_beginning_stressed(stem)):
            if not ru.is_monosyllabic(stem):
              pagemsg("Stem-stressed lemma %s with stress in default position for ending-stressed accent %s, removing: %s" % (
                lemma, newstress[0], unicode(t)))
            need_ending_stress = True
          else:
            pagemsg("WARNING: Stem-stressed lemma %s has stress not in default position for ending-stressed accent %s, can't remove: %s" % (
              lemma, newstress[0], unicode(t)))
        if need_ending_stress:
          lemma = ru.make_ending_stressed(lemma)
          if newstress == ["b"]:
            newstress = []

      # FIXME: Check for switching BARE to * reducible (or not)

      # Here we need to compare the old declension with the new one and
      # make sure the output is the same.
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
