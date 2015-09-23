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

keep_decl = [u"#", u"#-а", u"#-ья"]

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
    u"ин":[u"ин"],
    u"ёнок":[u"ёнок"],
    u"онок":[u"онок"],
    u"енок":[u"ёнок"],
    u"ёночек":[u"ёночек"],
    u"оночек":[u"оночек"],
    u"еночек":[u"ёночек"],
    u"инъ":[u"инъ"],
    u"ёнокъ":[u"ёнокъ"],
    u"онокъ":[u"онокъ"],
    u"енокъ":[u"ёнокъ"],
    u"ёночекъ":[u"ёночекъ"],
    u"оночекъ":[u"оночекъ"],
    u"еночекъ":[u"ёночекъ"],
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

# Split off lemma and decl. Doesn't handle plural lemmas currently
# (returns None for lemma and decl in that case).
def lemma_to_stem_decl(lemma, decl):
  m = re.search(u"^(.*?)([ая]́?)", lemma)
  if m:
    if "n" in decl:
      return None, None
  m = re.search(u"^(.*?)([ыи]́?)", lemma)
  if m:
    return None, None
  m = re.search(u"^(.*?)(ь[яеё]́?)$", lemma)
  if m:
    return m.groups()
  m = re.search(u"^(.*?)ь$", lemma)
  if m:
    if "f" in decl:
      return lemma, u"ь-f"
    elif "m" in decl:
      return lemma, u"ь-m"
    else:
      return None, None
  m = re.search(u"^(.*?)([йаяеёо]́?)$", lemma)
  if m:
    return m.groups()
  return lemma, ""

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
    if unicode(t.name) in ["ru-noun-table", "ru-noun-old"]:
      old = unicode(t.name) == "ru-noun-old"

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

      explicit_decl = False
      # If remaining decl is new format (opt. GENDER then opt. -ья), do nothing
      m = re.search(u"^([mfn]|3f|)(-ья)?", decl)
      if m:
        newdecl = decl + newdecl
      # If slash decl, also do nothing
      elif "/" in decl:
        pagemg("Found slash decl in template, not changing: %s" % unicode(t))
        newdecl = decl + newdecl
      elif "+" in decl:
        pagemg("Found adjectival decl in template, not changing: %s" % unicode(t))
        newdecl = decl + newdecl
      # Else, single old-style decl; convert to new by transfering the ending
      # to the end of the lemma and incorporating and new special marker if
      # needed
      else:
        ending_stressed = ru.is_stressed(decl)
        if decl != u"е́":
          decl = ru.remove_accents(decl)
        if decl in keep_decl:
          pagemsg("Keeping explicit decl %s: %s" % (decl, unicode(t)))
          explicit_decl = True
          newdecl = decl + newdecl
        else:
          if decl not in conv_decl:
            pagemsg("WARNING: Unrecognized declension %s in %s" %
                (decl, unicode(t)))
            continue
          declconv = conv_decl[decl]
          # If declconv[0] is "-ин" we might not be able to transfer it;
          # -ин is only recognized as special when animate and in combination
          # with -анин or -янин.
          if declconv[0] in [u"ин", u"инъ"] and (not re.search("^a", getparam(t, "a"))
              or not re.search(u"[яа]н", lemma.lower())):
            pagemsg("Keeping explicit decl %s: %s" % (declconv[0], unicode(t)))
            explicit_decl = True
            newdecl = declconv[0] + newdecl
          else:
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
                  special = [x for x in special if x != spec]
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

      if bare:
        if "*" in newdecl:
          pagemsg("WARNING: Bare %s found with reducible mark *, not changing: %s" %
              (bare, unicode(t)))
        elif len(newstress) > 1:
          pagemsg("WARNING: Bare %s found with multiple stresses %s, not changing: %s" %
              (bare, "/".join(newstress), unicode(t)))
        elif explicit_decl:
          pagemsg("WARNING: Bare %s found with kept explicit decl, not changing: %s" %
              (bare, unicode(t)))
        else:
          stem, decl = lemma_to_stem_decl(lemma, newdecl)
          if not stem:
            pagemsg("WARNING: Plural lemma %s found with bare %s, not changing: %s" %
                (lemma, bare, unicode(t)))
          else:
            if not newstress:
              if ru.is_stressed(decl):
                the_stress = "b"
              else:
                the_stress = "a"
            else:
              the_stress = newstress[0]
            if decl != u"е́":
              decl = ru.remove_accents(decl)
            bare_result = site.expand_text("{{#invoke:ru-noun|bare_tracking|%s|%s|%s|%s%s}}" % (
              stem, bare, decl, the_stress, old and "|yes" or ""))
            if not bare_result:
              bare = ""
            elif bare_result == "*":
              bare = ""
              newdecl += "*"
            elif bare_result == "**":
              lemma = bare
              bare = ""
              newdecl += "*"
            else:
              pagemsg("WARNING: Unable to remove bare %s: %s: %s" %
                  (bare, bare_result, unicode(t)))

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
        m = re.search("^(.*)([" + ru.vowel + "][" + ru.AC + ru.GR +ru.DI + "]?)$", lemma)
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
          if re.search("^[bd]", newstress[0]):
            if re.search(u"ё.*[" + ru.vowel + "]", stem):
              pagemsg(u"WARNING: ё in stem with later vowel in lemma %s and ending stress %s wanted, can't use ;ё special: %s" %
                  (lemma, newstress[0], unicode(t)))
            else:
              pagemsg(u"Removing ё from stem in lemma %s with ending stress %s wanted and using ;ё special: %s" %
                  (lemma, newstress[0], unicode(t)))
              need_ending_stress = True
          else:
            assert re.search("^f", newstress[0])
            if (re.search("[" + ru.vowel + u"].*ё", stem) or
              re.search(u"ё.*е", stem)):
              pagemsg(u"WARNING: ё in stem with earlier vowel or later е in lemma %s and ending stress %s wanted, can't use ;ё special: %s" %
                  (lemma, newstress[0], unicode(t)))
            else:
              pagemsg(u"Removing ё from stem in lemma %s with ending stress %s wanted and using ;ё special: %s" %
                  (lemma, newstress[0], unicode(t)))
              need_ending_stress = True
        else:
          if (re.search("^[bd]", newstress[0]) and ru.is_ending_stressed(stem) or
              re.search("^f", newstress[0]) and ru.is_beginning_stressed(stem)):
            if not ru.is_monosyllabic(stem):
              pagemsg("Stem-stressed lemma %s with stress in default position for ending-stressed accent %s, removing: %s" % (
                lemma, newstress[0], unicode(t)))
            need_ending_stress = True
          else:
            pagemsg("WARNING: Stem-stressed lemma %s has stress not in default position for ending-stressed accent %s, can't remove: %s" % (
              lemma, newstress[0], unicode(t)))
        if need_ending_stress:
          if re.search(u"е$", lemma):
            lemma = re.sub(u"е$", u"ё", lemma)
          else:
            lemma = ru.make_ending_stressed(lemma)
          if newstress == ["b"]:
            newstress = []

      # Handle spelling rules for ё/е/о after sibilants and ц
      lemma, nsubs = re.subn("([" + ru.sib_c + u"])ё(нок|ночек|)$", ur"\1о́\2$", lemma)
      if nsubs:
        pagemsg(u"Converted -ё after sibilant/ц to -о́ in %s: %s" % (
          lemma, unicode(t)))
      lemma, nsubs = re.subn("([" + ru.sib_c + u"])о$", ur"\1е$", lemma)
      if nsubs:
        pagemsg(u"Converted -о after sibilant/ц to -е in %s: %s" % (
          lemma, unicode(t)))

      # Compare the old declension with the new one and make sure the
      # output is the same.
      old_template = unicode(t)
      if newstress:
        new_values = [",".join(newstress), lemma, newdecl, bare, plstem]
      else:
        new_values = [lemma, newdecl, bare, plstem]
      while new_values and not new_values[-1]:
        del new_values[-1]
      new_values = "|".join(new_values)
      if new_values:
        new_values = "|" + new_values
      new_named_params = "|".join(unicode(x) for x in t.params
          if unicode(x.name) not in ["1", "2", "3", "4", "5"])
      if new_named_params:
        new_named_params = "|" + new_named_params
      new_template = "{{%s%s%s}}" % (unicode(t.name), new_values, new_named_params)
      if old_template != new_template:
        if verbose:
          pagemsg("Comparing output of %s and %s" % (old_template, new_template))
        raw_result = site.expand_text("{{#invoke:ru-noun|generate_multi_forms|%s|%s}}" % (
          old_template.replace("=", "<->").replace("|", "<!>"),
          new_template.replace("=", "<->").replace("|", "<!>")))
        if verbose:
          pagemsg("Raw result is %s" % raw_result)
        old_result, new_result = re.split("<!>", raw_result)
        old_vals = dict(re.split("=", x) for x in re.split(r"\|", old_result))
        new_vals = dict(re.split("=", x) for x in re.split(r"\|", new_result))
        keys = set(old_vals.keys())|set(new_vals.keys())
        vals_differ = False
        for key in keys:
          oval = old_vals.get(key, "")
          nval = new_vals.get(key, "")
          if oval != nval:
            pagemsg("WARNING: For key %s, old value %s different from new %s" % (
              key, oval, nval))
            vals_differ = True
        if vals_differ:
          pagemsg("WARNING: Template %s and replacement %s don't give same declension" % (old_template, new_template))
        else:
          rmparam(t, "5")
          rmparam(t, "4")
          rmparam(t, "3")
          rmparam(t, "2")
          rmparam(t, "1")
          i = 0
          for param in new_values:
            i += 1
            t.add(str(i), param)

  newtext = unicode(parsed)
  if newtext != oldtext:
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
