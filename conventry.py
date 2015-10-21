#!/usr/bin/env python
# -*- coding: utf-8 -*-

# TODO:
# 1. Canonicalize a=a etc. to a=an; remove a=i etc.; similarly for n=s, n=p.
#    (DONE)
# 2. Consider handling е́. One way is to destress it with a special variable
#    indicating that it needs restressing as е́ not ё. Another way is just
#    not to handle these cases and do them manually since there are only
#    maybe 3 or 4. (WON'T DO)
# 3. d|армянин generates армя́нин, should be d|армяни́н (DONE)
# 3a. b with ёнок and variants should be removed (DONE)
# 3b. Finish testing masculine additions (DONE)
# 4. Add reducible test cases. Implement some mockup of bare handling
#    for offline testing. (DONE)
# 5. Add test cases with -а, -й, ь-я, -ья, ъ-ья, etc. (DONE)
# 6. Add some adjectival test cases; at least make sure nothing changes. (DONE)
# 7. Make sure invariable cases are correctly handled. (DONE)
# 8. Test for real (online). (DONE)
# 9. Do a big run. (DONE)

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
    # u"-ья":[u"", u"-ья"], # should be caught earlier
    u"ъ-ья":[u"ъ", u"-ья"],
    u"а":[u"а"],
    u"я":[u"я"],
    u"о":[u"о"],
    u"о-ы":[u"о", "(1)"],
    u"о-и":[u"о", "(1)"],
    u"е":[u"е"],
    u"е́":[u"е́"],
    u"ё":[u"ё"],
    u"ья":[u"ья"],
    u"ье":[u"ье"],
    u"ьё":[u"ьё"],
    u"ин":[u"ин"],
    u"ёнок":[u"ёнок"],
    u"онок":[u"о́нок"],
    u"енок":[u"ёнок"],
    u"ёночек":[u"ёночек"],
    u"оночек":[u"о́ночек"],
    u"еночек":[u"ёночек"],
    u"инъ":[u"инъ"],
    u"ёнокъ":[u"ёнокъ"],
    u"онокъ":[u"о́нокъ"],
    u"енокъ":[u"ёнокъ"],
    u"ёночекъ":[u"ёночекъ"],
    u"оночекъ":[u"о́ночекъ"],
    u"еночекъ":[u"ёночекъ"],
    u"мя":[u"мя"],
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
# (returns None for lemma and decl in that case). Doesn't currently correctly
# handle suffixed -ин or -ёнок, etc. (FIXME; caller should check).
def lemma_to_stem_decl(lemma, decl):
  m = re.search(u"^(.*?)([ая]́?)$", lemma)
  if m:
    if "n" in decl:
      return None, "pl"
  m = re.search(u"^(.*?)([ыи]́?)$", lemma)
  if m:
    return None, "pl"
  m = re.search(u"^(.*?)(ь[яеё]́?)$", lemma)
  if m:
    return m.groups()
  m = re.search(u"^(.*?)(мя́?)$", lemma)
  if m:
    return m.groups()
  m = re.search(u"^(.*?)ь$", lemma)
  if m:
    if "f" in decl:
      return m.group(1), u"ь-f"
    elif "m" in decl:
      return m.group(1), u"ь-m"
    else:
      return None, "bad"
  m = re.search(u"^(.*?)([ъйаяеёо]́?)$", lemma)
  if m:
    stem, decl = m.groups()
    # Convert е to о after sibilant/ц or we will have problems in bare tracking
    if decl == u"е" and re.search("[" + ru.sib_c + "]$", stem):
      decl = u"о"
    return stem, decl
  return lemma, ""

def is_suffixed(lemma):
  return re.search(u"([яа]нин|[ёо]нок|[ёо]ночек|мя)ъ?$", ru.remove_accents(lemma))

def process_page(index, page, save=False, verbose=False):
  pagetitle = unicode(page.title())
  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  if not page.exists():
    pagemsg("WARNING: Page doesn't exist")
    return

  pagetext = unicode(page.text)
  newtext, comment = process_page_data(index, pagetitle, pagetext, save, verbose)
  if newtext != pagetext:
    if save:
      pagemsg("Saving with comment = %s" % comment)
      page.text = newtext
      page.save(comment=comment)
    else:
      pagemsg("Would save with comment = %s" % comment)


def process_page_data(index, pagetitle, pagetext, save=False, verbose=False, offline=False):
  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  def expand_text(txt):
    if verbose:
      pagemsg("Expanding text: %s" % txt)
    result = site.expand_text(txt, title=pagetitle)
    if verbose:
      pagemsg("Raw result is %s" % result)
    if result.startswith('<strong class="error">'):
      result = re.sub("<.*?>", "", result)
      pagemsg("WARNING: Got error: %s" % result)
      return False
    return result

  newtext = pagetext
  parsed = blib.parse_text(pagetext)

  lemmas_changed = []

  for t in parsed.filter_templates():
    if unicode(t.name) in ["ru-noun-table", "ru-noun-old"]:
      old = unicode(t.name) == "ru-noun-old"

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

      # Retrieve params; if first arg isn't stress, then it's lemma
      # and shift everything down
      stress = getparam(t, "1")
      lemma = getparam(t, "2")
      decl = getparam(t, "3")
      bare = getparam(t, "4")
      plstem = getparam(t, "5")
      if not arg1_is_stress(stress):
        if plstem:
          pagemsg("WARNING: Extraneous 5th argument: %s" % unicode(t))
          continue
        plstem = bare
        bare = decl
        decl = lemma
        lemma = stress
        stress = ""
      if not lemma:
        lemma = pagetitle
      if "$" in decl:
        pagemsg("WARNING: Skipping invariable decl: %s" % unicode(t))
        continue
      if "+" in decl:
        pagemsg("WARNING: Skipping adjectival decl: %s" % unicode(t))
        continue
      if "//" in lemma:
        pagemsg("WARNING: Skipping lemma with manual translit: %s" % unicode(t))
        continue
      if lemma.startswith("*"):
        pagemsg("WARNING: Skipping lemma marked as no-accent: %s" % unicode(t))
        continue

      # Extract specials from decl, put into newdecl
      newdecl = ""
      decl, nsubs = re.subn(ur"(\d|[^/\w])ё(\d|[^/\w]|$)", r"\1\2", decl, 0, re.U)
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
      ending_stressed = False
      # If remaining decl is new format (opt. GENDER then opt. decl variant,
      # do nothing
      m = re.search(u"^([mfn]|3f|)(-ья|-ин|-ишко|-ище)?$", decl)
      if m:
        newdecl = decl + newdecl
      # If slash decl, also do nothing
      elif "/" in decl:
        pagemsg("Found slash decl in template, not changing: %s" % unicode(t))
        newdecl = decl + newdecl
      elif "+" in decl:
        pagemsg("Found adjectival decl in template, not changing: %s" % unicode(t))
        newdecl = decl + newdecl
      # Else, single old-style decl; convert to new by transfering the ending
      # to the end of the lemma and incorporating any new special marker if
      # needed
      else:
        ending_stressed = ru.is_stressed(decl)
        if decl != u"е́":
          decl = ru.make_unstressed(decl)
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
              or not re.search(u"[яа]́?н$", lemma.lower())):
            pagemsg("Keeping explicit decl %s: %s" % (declconv[0], unicode(t)))
            explicit_decl = True
            newdecl = declconv[0] + newdecl
          else:
            lemma = lemma + declconv[0]

            # Transfer all the special markers, make sure they're not duplicated
            # and go in the right order
            if len(declconv) == 1:
              special = []
            elif type(declconv[1]) is list:
              special = declconv[1]
            else:
              special = [declconv[1]]
            prepend = ""
            for spec in ["m", "f", u"-ья", "(1)"]:
              if spec in special:
                if spec not in newdecl:
                  prepend += spec
                special = [x for x in special if x != spec]
            newdecl = prepend + newdecl
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
          pagemsg("WARNING: Bare %s found with multiple stress patterns %s, not changing: %s" %
              (bare, "/".join(newstress), unicode(t)))
        elif explicit_decl:
          pagemsg("WARNING: Bare %s found with kept explicit decl, not changing: %s" %
              (bare, unicode(t)))
        elif is_suffixed(lemma):
          pagemsg("WARNING: Bare %s found with suffixed lemma %s, not changing: %s" % (
            bare, lemma, unicode(t)))
        elif offline:
          pagemsg("WARNING: Bare %s found when testing, not changing: %s" % (
            bare, unicode(t)))
        else:
          stem, decl = lemma_to_stem_decl(lemma, newdecl)
          if decl == "bad":
            pagemsg("WARNING: Unable to extract stem and decl from lemma %s found with bare %s, not changing: %s" %
                (lemma, bare, unicode(t)))
          elif decl == "pl":
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
              decl = ru.make_unstressed(decl)
            restressed_stem = stem
            if ru.is_monosyllabic(restressed_stem):
              restressed_stem = ru.make_ending_stressed(restressed_stem)
            elif ru.is_unstressed(restressed_stem):
              if re.search("^[bd]", the_stress):
                restressed_stem = ru.make_ending_stressed(restressed_stem)
              elif re.search("^f", the_stress):
                restressed_stem = ru.make_beginning_stressed(restressed_stem)
            bare_result = expand_text("{{#invoke:ru-noun|bare_tracking|%s|%s|%s|%s%s}}" % (
              restressed_stem, bare, decl, the_stress, old and "|yes" or ""))
            if bare_result == "remove":
              bare = ""
            elif bare_result == "remove-star":
              bare = ""
              newdecl += "*"
            elif bare_result == "sub-star":
              lemma = bare
              if re.search(u"^ь", decl) and not re.search(u"ь$", lemma):
                lemma += u"ь"
              elif re.search(u"^ъ", decl) and not re.search(u"ъ$", lemma):
                lemma += u"ъ"
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
            newstress = []
      # else, ends with vowel
      elif ru.is_multi_stressed(lemma):
        pass # We will warn about this later
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
        elif u"ё" in stem or u"Ё" in stem:
          if re.search("^[bd]", newstress[0]):
            if re.search(u"[ёЁ].*[" + ru.vowel + "]", stem):
              pagemsg(u"WARNING: ё in stem with later vowel in lemma %s and ending stress %s wanted, can't use ;ё special: %s" %
                  (lemma, newstress[0], unicode(t)))
            else:
              pagemsg(u"Removing ё from stem in lemma %s with ending stress %s wanted and using ;ё special: %s" %
                  (lemma, newstress[0], unicode(t)))
              newdecl += u";ё"
              need_ending_stress = True
          else:
            assert re.search("^f", newstress[0])
            if (re.search("[" + ru.vowel + u"].*ё", stem) or
              re.search(u"[ёЁ].*[еЕ]", stem)):
              pagemsg(u"WARNING: ё in stem with earlier vowel or later е in lemma %s and ending stress %s wanted, can't use ;ё special: %s" %
                  (lemma, newstress[0], unicode(t)))
            else:
              pagemsg(u"Removing ё from stem in lemma %s with ending stress %s wanted and using ;ё special: %s" %
                  (lemma, newstress[0], unicode(t)))
              newdecl += u";ё"
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
            lemma = re.sub(u"е$", u"ё", ru.make_unstressed(lemma))
          else:
            lemma = ru.make_ending_stressed(lemma)
          if newstress == ["b"]:
            newstress = []

      # Remove stress b from lemmas in -ёнок and variants
      if newstress == ["b"] and re.search(u"ёно(че)?къ?$|[" + ru.sib_c + u"]о́но(че)?къ?$", lemma.lower()):
        newstress = []

      # Convert d|армя́нин and d|гра́жданин to be ending-stressed
      if (newstress == ["d"] and re.search(u"[яа]́?ни́?нъ?$", lemma.lower()) and
          re.search("^a", getparam(t, "a"))):
        lemma = ru.make_ending_stressed(lemma)

      # Handle spelling rules for ё/е/о after sibilants and ц
      lemma, nsubs = re.subn("([" + ru.sib_c + u"])ё(нокъ?|ночекъ?|)$", ur"\1о́\2", lemma)
      if nsubs:
        pagemsg(u"Converted -ё after sibilant/ц to -о́ in %s: %s" % (
          lemma, unicode(t)))
      lemma, nsubs = re.subn("([" + ru.sib_c + u"])о$", ur"\1е", lemma)
      if nsubs:
        pagemsg(u"Converted -о after sibilant/ц to -е in %s: %s" % (
          lemma, unicode(t)))

      # If n=p given, convert lemma to plural and remove n=p
      remove_n = False
      if re.search("^p", getparam(t, "n")):
        if "+" in newdecl:
          pagemsg("WARNING: FIXME: Not converting adjectival lemma %s to plural: %s" % (
            lemma, unicode(t)))
        elif "/" in newdecl or explicit_decl:
          pagemsg("WARNING: Explicit decl %s, not converting lemma %s to plural: %s" % (
          newdecl, lemma, unicode(t)))
        elif is_suffixed(lemma):
          pagemsg("WARNING: Not converting suffixed lemma %s to plural: %s" % (
            lemma, unicode(t)))
        elif "(1)" in newdecl:
          pagemsg("WARNING: Not converting lemma %s with special (1) to plural: %s" % (
            lemma, unicode(t)))
        elif u"-ья" in newdecl:
          pagemsg(u"WARNING: Not converting lemma %s with special -ья to plural: %s" % (
            lemma, unicode(t)))
        else:
          stem, decl = lemma_to_stem_decl(lemma, newdecl)
          if decl == "bad":
            pagemsg("WARNING: Unable to extract stem and decl from lemma %s, not changing: %s" %
                (lemma, unicode(t)))
          elif decl == "pl":
            pagemsg("WARNING: Plural lemma %s found with n=p, removing n=p: %s" %
                (lemma, unicode(t)))
            remove_n = True
          elif decl == u"е́":
            pagemsg(u"WARNING: Not converting lemma %s with -е́ to plural: %s" % (
              lemma, unicode(t)))
          else:
            was_stressed = ru.is_stressed(decl)
            decl = ru.make_unstressed(decl)
            newlemma = lemma
            if decl in ["", u"ъ", u"й", u"ь-m", u"ь-f"]:
              if bare:
                pagemsg("WARNING: Not converting lemma %s with explicit bare %s to plural: %s" % (
                  lemma, bare, unicode(t)))
              elif "*" in newdecl:
                pagemsg("WARNING: Not converting lemma %s with reducible to plural: %s" % (
                  lemma, unicode(t)))
              else:
                if re.search("[" + ru.velar + ru.sib + "]$", stem) or decl in [u"й", u"ь-m", u"ь-f"]:
                  newlemma = stem + u"и"
                else:
                  newlemma = stem + u"ы"
                if newstress == ["b"]:
                  if u"ё" in stem or u"Ё" in stem:
                    pagemsg(u"ё in stem %s and stress b, not accenting pl ending: %s" % (stem, unicode(t)))
                  else:
                    newlemma = ru.make_ending_stressed(newlemma)
                    newstress = []
                if decl == u"ь-f":
                  newgender = "3f"
                else:
                  newgender = "m"
            else:
              if decl in [u"а", u"я"]:
                if re.search("[" + ru.velar + ru.sib + "]$", stem) or decl == u"я":
                  newlemma = stem + u"и"
                else:
                  newlemma = stem + u"ы"
                newgender = "f"
              elif decl in [u"е", u"ё", u"о"]:
                if re.search("[" + ru.velar + ru.sib_c + "]$", stem) or decl == u"о":
                  newlemma = stem + u"а"
                else:
                  newlemma = stem + u"я"
                newgender = "n"
              elif decl in [u"ье", u"ьё"]:
                newlemma = stem + u"ья"
                newgender = "n"
              else:
                pagemsg("WARNING: Strange decl %s for lemma %s: %s" % (
                  decl, lemma, unicode(t)))
                newlemma = lemma
              if was_stressed:
                newlemma = ru.make_ending_stressed(newlemma)
            if newlemma != lemma:
              m = re.search("(3f|[mnf])", newdecl)
              if m and m.group(1) == "f" and newgender == "3f" and decl == u"ь-f":
                pagemsg(u"Converting f gender in ь-f declension to 3f: %s" %
                    unicode(t))
                newdecl = newdecl.replace("f", "3f")
                lemma = newlemma
                remove_n = True
              elif m and m.group(1) != newgender:
                pagemsg("WARNING: Existing gender %s present and not same as new plural gender %s: %s" % (
                  m.group(1), newgender, unicode(t)))
              else:
                if not m:
                  newdecl = newgender + newdecl
                pagemsg("Changing lemma from %s to plural %s with gender %s and removing n=p: %s" % (
                  lemma, newlemma, newgender, unicode(t)))
                lemma = newlemma
                remove_n = True

      lemma = ru.remove_monosyllabic_accents(lemma)
      if ru.is_multi_stressed(lemma):
        pagemsg("WARNING: Multiple stresses in lemma %s: %s" %
            (lemma, unicode(t)))
      full_lemma = lemma
      if lemma == pagetitle:
        lemma = ""

      def get_template_args(ttext):
        if ttext.startswith("{{ru-noun-old"):
          ttext = re.sub("\}\}$", "|old=y}}", ttext)
        t = list(blib.parse_text(ttext).filter_templates())[0]
        return "<!>".join(("%s<->%s" % (unicode(x.name), unicode(x.value)) if x.showkey else unicode(x.value)) for x in t.params)

      # Compare the old declension with the new one and make sure the
      # output is the same.
      old_template = unicode(t)

      # Canonicalize animacy and number while we're at it.
      anim = getparam(t, "a")
      if not anim:
        pass
      elif re.search("^a", anim):
        t.add("a", "an")
      elif re.search("^i", anim):
        rmparam(t, "a")
      elif re.search("^b", anim):
        t.add("a", "bi")
      else:
        pagemsg("WARNING: Strange animacy value %s: %s" % (anim, unicode(t)))
      num = getparam(t, "n")
      if not num:
        pass
      elif re.search("^s", num):
        t.add("n", "sg")
      elif re.search("^p", num):
        t.add("n", "pl")
      elif re.search("^b", num):
        rmparam(t, "n")
      else:
        pagemsg("WARNING: Strange number value %s: %s" % (num, unicode(t)))

      if newstress:
        new_values = [",".join(newstress), lemma, newdecl, bare, plstem]
      else:
        new_values = [lemma, newdecl, bare, plstem]
      while new_values and not new_values[-1]:
        del new_values[-1]
      new_value_str = "|".join(new_values)
      if new_value_str:
        new_value_str = "|" + new_value_str
      new_named_params = [x for x in t.params
          if unicode(x.name) not in ["1", "2", "3", "4", "5"] and
          (not remove_n or unicode(x.name) != "n")]
      new_named_param_str = "|".join(unicode(x) for x in new_named_params)
      if new_named_param_str:
        new_named_param_str = "|" + new_named_param_str
      new_template = "{{%s%s%s}}" % (unicode(t.name), new_value_str, new_named_param_str)
      if old_template == new_template:
        pagemsg("Not changing template %s" % old_template)
      else:
        vals_differ = False
        if verbose:
          pagemsg("Comparing output of %s and %s" % (old_template, new_template))
        if not offline:
          raw_result = expand_text("{{#invoke:ru-noun|generate_multi_forms|%s|%s}}" % (
            get_template_args(old_template),
            get_template_args(new_template)))
          if not raw_result:
            continue
          old_result, new_result = re.split("<!>", raw_result)
          vals_differ = old_result != new_result
          #old_vals = dict(re.split("=", x) for x in re.split(r"\|", old_result))
          #new_vals = dict(re.split("=", x) for x in re.split(r"\|", new_result))
          #keys = set(old_vals.keys())|set(new_vals.keys())
          #for key in keys:
          #  oval = old_vals.get(key, "")
          #  nval = new_vals.get(key, "")
          #  if oval != nval:
          #    pagemsg("WARNING: For key %s, old value %s different from new %s" % (
          #      key, oval, nval))
          #    vals_differ = True
        if vals_differ:
          pagemsg("WARNING: Template %s and replacement %s don't give same declension" % (old_template, new_template))
        else:
          del t.params[:]
          #if remove_n:
          #  rmparam(t, "n")
          #rmparam(t, "5")
          #rmparam(t, "4")
          #rmparam(t, "3")
          #rmparam(t, "2")
          #rmparam(t, "1")
          i = 0
          for param in new_values:
            i += 1
            t.add(str(i), param)
          t.params.extend(new_named_params)
          if new_template != unicode(t):
            pagemsg("WARNING: Something wrong: New template text %s not equal to new template %s" % (
              new_template, unicode(t)))
            assert False
          pagemsg("Converting %s to %s" % (old_template, new_template))
          if full_lemma not in lemmas_changed:
            lemmas_changed.append(full_lemma)

  comment = "Rewrite decl templates to new style for %s" % ", ".join(lemmas_changed)
  return unicode(parsed), comment

pa = argparse.ArgumentParser()
pa.add_argument("--start", help="Start index", type=int)
pa.add_argument("--end", help="Start index", type=int)
pa.add_argument("--file", help="File containing pages to do")
pa.add_argument("--save", help="Save pages", action="store_true")
pa.add_argument("--verbose", help="Output verbose messages", action="store_true")
pa.add_argument("--test", help="Test", action="store_true")
pa.add_argument("--offline", help="Do testing offline", action="store_true")
pargs = pa.parse_args()

def yield_pages(fn):
  pages = [x.strip() for x in codecs.open(fn, "r", "utf-8")]
  i = 0
  for page in pages:
    i += 1
    if pargs.start and i < pargs.start:
      continue
    if pargs.end and i > pargs.end:
      break
    yield i, pywikibot.Page(site, page)

def yield_ref_pages():
  for i, page in blib.references("Template:ru-noun-table", pargs.start or None,
      pargs.end or None):
    yield i, page
  for i, page in blib.references("Template:ru-noun-old", pargs.start or None,
      pargs.end or None):
    yield i, page

testdata = [(u"яблоко", u"{{ru-noun-table|1|я́блок|о-и}}"),
            (u"имя", u"{{ru-noun-table|3|и́|мя}}"),
            (u"имя", u"{{ru-noun-old|3|и́|мя}}"),
            (u"ребёнок", u"""{{ru-noun-table|6|реб|ёнок/ь-m||де́т|or|2|ребёнок|dat_pl=де́тям,ребя́там*|ins_pl=детьми́,ребя́тами*|pre_pl=де́тях,ребя́тах*|a=an|pltail=*|notes=* Use the second plural with the meaning "boys!", "fellows!", "guys!", "comrades!".}}"""),
            (u"море", u"{{ru-noun-table|3|мо́р|е}}"),
            (u"другъ", u"{{ru-noun-old|c|дру́гъ|-ья(2)||друз|voc=дру́же|a=an}}"),
            (u"сажень", u"{{ru-noun-table|1|са́жен|ь-f|gen_pl=са́женей, са́жен}}"),
            # masculines
            (u"завод", u"{{ru-noun-table|1|заво́д}}"),
            (u"житель", u"{{ru-noun-table|1|жи́тел|ь-m|a=an}}"),
            (u"товарищ", u"{{ru-noun-table|1|това́рищ|a=an}}"),
            (u"случай", u"{{ru-noun-table|1|слу́ча|й}}"),
            (u"случай", u"{{ru-noun-old|1|слу́ча|й}}"),
            (u"герой", u"{{ru-noun-table|1|геро́|й|a=an}}"),
            (u"сценарий", u"{{ru-noun-table|1|сцена́ри|й}}"),
            (u"топор", u"{{ru-noun-table|2|топо́р|}}"),
            (u"словарь", u"{{ru-noun-table|2|слова́р|ь-m}}"),
            (u"рыбак", u"{{ru-noun-table|2|рыба́к|a=an}}"),
            (u"нож", u"{{ru-noun-table|2|но́ж}}"),
            (u"холуй", u"{{ru-noun-table|2|холу́|й}}"),
            (u"холуй", u"{{ru-noun-table|2|холу́й}}"),
            (u"дар", u"{{ru-noun-table|3|да́р}}"),
            (u"плуг", u"{{ru-noun-table|3|плу́г}}"),
            (u"буй", u"{{ru-noun-table|3|бу́|й}}"),
            (u"кол", u"{{ru-noun-table|4|ко́л|-ья|loc=на +}}"),
            (u"зуб", u"{{ru-noun-old|5|зу́б|ъ-ья|pltailall=*|notes=* Technical.}}"),
            (u"голубь", u"{{ru-noun-table|5|го́луб|ь-m|a=an}}"),
            (u"волк", u"{{ru-noun-table|5|во́лк|a=an}}"),
            (u"конь", u"{{ru-noun-table|6|кон|ь-m}}"),
            (u"конь", u"{{ru-noun-table|6||m}}"),
            (u"римлянин", u"{{ru-noun-table|1|ри́млян|ин|a=an}}"),
            (u"южанин", u"{{ru-noun-table|1|южа́н|ин|a=an}}"),
            (u"армянин", u"{{ru-noun-table|4|армя́н|ин|a=an}}"),
            (u"боярин", u"{{ru-noun-table|1|боя́р|ин|a=an}}"),
            (u"господин", u"{{ru-noun-table|2|госпо́д|ин|a=an|nom_pl=господа́}}"),
            (u"господь", u"{{ru-noun-table|го́спод|nom_sg=госпо́дь|voc=~и|n=s|a=a}}"),
            (u"господь", u"{{ru-noun-table|1|го́спод||госпо́дь|voc=~и|n=s|a=a}}"),
            (u"цыплёнок", u"{{ru-noun-table|2|цыпл|ёнок|a=an}}"),
            (u"мышонок", u"{{ru-noun-table|2|мыш|онок|a=an}}"),
            (u"цыплёночек", u"{{ru-noun-table|2|цыпл|ёночек|a=an}}"),
            (u"цыплёночек", u"{{ru-noun-table|2|цыплёночек|a=an}}"),
            (u"цыплёночек", u"{{ru-noun-table|2|цыплёночек|a=an}}"),
            (u"мышоночек", u"{{ru-noun-table|2|мыш|оночек|a=an}}"),
            (u"мышоночек", u"{{ru-noun-table|2|мышо́ночек|a=an}}"),
            (u"обшлаг", u"{{ru-noun-table|2|обшла́г|(1)}}"),
            (u"мастер", u"{{ru-noun-table|3|ма́стер|-а|a=an}}"),
            (u"округ", u"{{ru-noun-table|3|о́круг|-а}}"),
            (u"якорь", u"{{ru-noun-table|3|я́кор|ь-я}}"),
            (u"край", u"{{ru-noun-table|3|кра́|й|nom_pl=края́|par=кра́ю|loc=краю́}}"),
            (u"край", u"{{ru-noun-table|3|кра́|й-я|par=кра́ю|loc=краю́}}"),
            (u"раз", u"{{ru-noun-table|c||(2)|par=+}}"),
            (u"раз", u"{{ru-noun-table|3|ра́з|(2)|par=+}}"),
            (u"волос", u"{{ru-noun-table|e|во́лос|(2)}}"),
            (u"осётр", u"{{ru-noun-table|2|осётр|a=an}}"),
            (u"мёд", u"{{ru-noun-table|3|мёд|par=+|loc=+}}"),
            (u"чёрт", u"{{ru-noun-table|5|чёрт|/ь-m|nom_pl=че́рти|a=an|acc_sg=чёрта|gen_sg=чёрта,черта́*|notes=<sup>*</sup> Only in the expression ''ни черта́''.}}"),
            (u"жёлудь", u"{{ru-noun-table|5|жёлуд|ь-m}}"),
            (u"жёрнов", u"{{ru-noun-table|3|жёрнов|-а}}"),
            (u"шёлк", u"{{ru-noun-table|3|шёлк|-а}}"),
            (u"анналы", u"{{ru-noun-table|1|анна́л|n=pl}}"),
            (u"весы", u"{{ru-noun-table|2|вес|n=pl}}"),
            (u"кочкари", u"{{ru-noun-table|2|кочкар|ь-m|n=p}}"),
            (u"сани", u"{{ru-noun-table|5|са́нь|m|n=pl}}"),
            (u"трусики", u"{{ru-noun-table|тру́сик|n=pl}}"),
            (u"духи", u"{{ru-noun-table|2|дух|n=pl}}"),
            (u"пассатижи", u"{{ru-noun-table|1|пассати́ж|n=pl}}"),
            (u"щи", u"{{ru-noun-table|2|щ|ь-f|n=p}}"),
            (u"щи", u"{{ru-noun-old|2|щ|ь-f|n=p}}"),
            (u"дрожжи", u"{{ru-noun-table|6|дро́жж|а|n=pl}}"),
            (u"помои", u"{{ru-noun-table|1|помо́|й|n=plural}}"),
            (u"леса", u"{{ru-noun-table|2|лес|-а|n=pl}}"),
            (u"зеленя", u"{{ru-noun-table|2|зелен|ь-я|n=p}}"),
            (u"торока", u"{{ru-noun-table|2|торок|n=plu}}"),
            (u"стул", u"{{ru-noun-table|1|сту́л|-ья}}"),
            (u"брат", u"{{ru-noun-table|бра́т|-ья|a=an|pltailall=*|notes=* The soft ending of the plural was originally used for feminine [[collective noun]]s, and the meaning of '''братья''' then was [[brotherhood]].}}"),
            (u"клок", u"{{ru-noun-table|4|кло́к|-ья||кло́ч}}"),
            (u"крюк", u"{{ru-noun-table|2|крю́к|or|4||-ья||крю́ч}}"),
            (u"крюк", u"{{ru-noun-table|4||-ья||крю́ч}}"),
            (u"колос", u"{{ru-noun-table|1|ко́лос|-ья||коло́с}}"),
            (u"сынъ", u"{{ru-noun-old|c|сын|ъ-ья(2)||сынов|voc=сы́не|a=an}}"),
            (u"деверь", u"{{ru-noun-table|c|де́верь|m-ья(2)|a=an}}"),
            (u"кум", u"{{ru-noun-table|3|ку́м|-ья||кумов|a=an}}"),
            # feminines
            (u"похвала", u"{{ru-noun-table|2|похва́л|а}}"),
            (u"тля", u"{{ru-noun-table|2|тл|я|a=an}}"),
            (u"сирота", u"{{ru-noun-table|4|сиро́т|а|a=an}}"),
            (u"заря", u"{{ru-noun-table|d|заря́|||зор|dat_pl=зо́рям,заря́м*|ins_pl=зо́рями,заря́ми*|pre_pl=зо́рях,заря́х*|notes=* Archaic.}}"),
            (u"межа", u"{{ru-noun-table|4|ме́ж|а}}"),
            (u"змея", u"{{ru-noun-table|4|зме́|я|a=a}}"),
            (u"доля", u"{{ru-noun-table|5|до́л|я}}"),
            (u"слобода", u"{{ru-noun-table|f|слобода́}}"),
            (u"рука", u"{{ru-noun-table|6*|ру́к|а}}"),
            (u"пустошь", u"{{ru-noun-table|1|пу́стош|ь-f}}"),
            (u"площадь", u"{{ru-noun-table|5|пло́щад|ь-f}}"),
            (u"статья", u"{{ru-noun-table|2|стат|ья}}"),
            (u"гостья", u"{{ru-noun-table|1|го́ст|ья|a=an}}"),
            (u"пелена", u"{{ru-noun-table|2|пелён|а}}"),
            (u"слеза", u"{{ru-noun-table|6|слёз|а}}"),
            (u"железа", u"{{ru-noun-table|f|железа́|;ё}}"),
            (u"щёлочь", u"{{ru-noun-table|5|щёлоч|ь-f}}"),
            (u"брюки", u"{{ru-noun-table|1|брю́к|а|n=pl}}"),
            (u"солнце", u"{{ru-noun-table|1|со́лнц|о}}"),
            (u"жилище", u"{{ru-noun-table|1|жили́щ|о}}"),
            (u"лицо", u"{{ru-noun-table|4|ли́ц|о}}"),
            (u"лицо", u"{{ru-noun-old|4|ли́ц|о}}"),
            (u"питьё", u"{{ru-noun-table|2|пит|ьё}}"),
            (u"копьё", u"{{ru-noun-table|4|ко́п|ьё}}"),
            (u"житие", u"{{ru-noun-table|2|жити́|е́}}"),
            (u"ворота", u"{{ru-noun-table|1|воро́т|о|n=p}}"),

	# misc to check
	(u"ветла", u"{{ru-noun-table|4|вётл|а|вётел}}"),
        (u"кошма", u"{{ru-noun-table|4|ко́шм|а|ко́шем|or|6||а}}"),
        (u"кошма", u"{{ru-noun-table|4|ко́шм|а|ко́шем}}"),
        # masculine reducibles
        (u"свёкор", u"{{ru-noun-table|1|свёкр||свёкор|a=an}}"),
        (u"бубен", u"{{ru-noun-table|1|бу́бн||бу́бен}}"),
        # following is irreg in accent on nom sg
        (u"заём", u"{{ru-noun-table|1|за́йм||заём}}"),
        (u"бугор", u"{{ru-noun-table|2|бугр||буго́р}}"),
        (u"угол", u"{{ru-noun-table|2|угл||у́гол|loc=углу́}}"),
        (u"котёл", u"{{ru-noun-table|2|котл||котёл}}"),
        (u"хребет", u"{{ru-noun-table|2|хребт||хребе́т}}"),
        # following is irreg in accent on nom sg
        (u"узел", u"{{ru-noun-table|2|узл||у́зел}}"),
        # following is irreg in fill vowel, can't reduce: 1b or 1*b
        (u"кочан", u"{{ru-noun-table|2|кочн||коча́н}}"),
        (u"лёд", u"{{ru-noun-table|2|льд||лёд|loc=льду́}}"),
        (u"лев", u"{{ru-noun-table|2|льв||лев|a=an}}"),
        # following is 1*e or 1*a or poetical 1*c(1) [occurs down below]
        (u"ветер", u"{{ru-noun-table|5,1|ве́тр||ве́тер|par=ве́тру|loc=ветру́}}"),
        (u"дёготь", u"{{ru-noun-table|дёгт|ь-m|дёготь}}"),
        (u"дёготь", u"{{ru-noun-table||m*}}"),
        (u"ливень", u"{{ru-noun-table|1|ли́вн|ь-m|ли́вень}}"),
        (u"увалень", u"{{ru-noun-table|у́вальнь|m|у́валень}}"),
        (u"огонь", u"{{ru-noun-table|2|огн|ь-m|ого́нь}}"),
        # following is ломо́ть 2*b or ло́моть 2*e
        (u"ломоть", u"{{ru-noun-table|2|ломот|ь-m|ломо́ть}}"),
        # following is irreg in accent on nom sg
        (u"угорь", u"{{ru-noun-table|2|угр|ь-m|у́горь|a=an}}"),
        (u"ремень", u"{{ru-noun-table|2|ремн|ь-m|реме́нь}}"),
        (u"ноготь", u"{{ru-noun-table|5|но́гт|ь-m|но́готь}}"),
        (u"корень", u"{{ru-noun-table|5|ко́рн|ь-m|ко́рень}}"),
        (u"корень", u"{{ru-noun-old|5|ко́рн|ь-m|ко́рень}}"),
        (u"кубок", u"{{ru-noun-table|1|ку́бк||ку́бок}}"),
        (u"кусочек", u"{{ru-noun-table|1|кусо́чк||кусо́чек}}"),
        (u"перешеек", u"{{ru-noun-table|1|переше́йк||переше́ек}}"),
        (u"кусок", u"{{ru-noun-table|2|куск||кусо́к}}"),
        (u"хорёк", u"{{ru-noun-table|2|хорьк||хорёк|a=an}}"),
        (u"паёк", u"{{ru-noun-table|2|пайк||паёк}}"),
        (u"немец", u"{{ru-noun-table|1|не́мц||не́мец|a=an}}"),
        (u"палец", u"{{ru-noun-table|1|па́льц||па́лец}}"),
        (u"китаец", u"{{ru-noun-table|1|кита́йц||кита́ец|a=an}}"),
        # following is irreg in fill vowel, can't reduce: 5*a
        (u"заяц", u"{{ru-noun-table|за́йц|nom_sg=за́яц|a=an}}"),
        (u"заяц", u"{{ru-noun-table|a|за́йц||за́яц|a=an}}"),
        (u"конец", u"{{ru-noun-table|2|конц||коне́ц}}"),
        (u"жилец", u"{{ru-noun-table|2|жильц||жиле́ц}}"),
        (u"боец", u"{{ru-noun-table|2|бойц||бое́ц|a=an}}"),
        (u"улей", u"{{ru-noun-table|1|у́ль|й|у́лей}}"),
        (u"ручей", u"{{ru-noun-table|2|ручь|й|руче́й}}"),
        (u"ручей", u"{{ru-noun-old|2|ручь|й|руче́й}}"),
        (u"муравей", u"{{ru-noun-table|2|муравь|й|мураве́й|a=an}}"),
        (u"щенок", u"{{ru-noun-table|b|щено́к|*|or|d||*(1)(2)||щеня́т|a=an}}"),
        # special (1)
        (u"промысел", u"{{ru-noun-table|1|про́мысл||про́мысел}}"),
        (u"промысел", u"{{ru-noun-table|3|про́мысл|-а|про́мысел}}"),
        (u"ветер", u"{{ru-noun-table|3|ве́тр|-а|ве́тер|par=ве́тру|loc=ветру́}}"),
        # special (2)
        (u"ботинок", u"{{ru-noun-table|боти́нок|*(2)}}"),
        (u"ботинок", u"{{ru-noun-table|1|боти́нк|(2)|боти́нок}}"),
        (u"ботиночек", u"{{ru-noun-table|1|боти́ночк|(2)|боти́ночек}}"),
        (u"чулок", u"{{ru-noun-table|b|чуло́к|*(2)}}"),
        (u"чулок", u"{{ru-noun-table|b|чулк|(2)|чуло́к}}"),
        (u"глазок", u"{{ru-noun-table|d|глазо́к|*(2)}}"),
        (u"глазок", u"{{ru-noun-table|4|глазк|(2)|глазо́к}}"),
        (u"глазок", u"{{ru-noun-table|b|глазо́к|*}}"),
        (u"глазок", u"{{ru-noun-table|2|глазк||глазо́к}}"),
        (u"рожок", u"{{ru-noun-table|d|рожо́к|*(2)}}"),
        (u"рожок", u"{{ru-noun-table|d|рожк|(2)|рожо́к}}"),
        # special optional (2)
        (u"черевичек", u"{{ru-noun-table|череви́чк|(2)|череви́чек}}"),
        # reducible pl-only
        (u"плавни", u"{{ru-noun-table|пла́вни|*m}}"),
        # following is 2*a // 2*e
        (u"дровни", u"{{ru-noun-table|a,e|дро́вни|*m}}"),
        (u"останки", u"{{ru-noun-table|оста́нки|*m}}"),
        (u"останки", u"{{ru-noun-table|оста́нк||оста́нок|n=pl}}"),
        (u"очки", u"{{ru-noun-table|2|очк|n=pl}}"),
        (u"очки", u"{{ru-noun-table|2|очк||оче́к|n=pl}}"),
        (u"очки", u"{{ru-noun-table|2|оче́к|*|n=pl}}"),
        (u"очки", u"{{ru-noun-table|2|очки|*m}}"),
        (u"очки", u"{{ru-noun-table|очки́|m}}"),
        (u"плоскозыбцы", u"{{ru-noun-table|плоскозу́бцы|*m}}"),
        (u"щипцы", u"{{ru-noun-table|2|щипц|n=pl}}"),
        (u"щипцы", u"{{ru-noun-table|2|щипц||щипе́ц|n=pl}}"),
        (u"щипцы", u"{{ru-noun-table|шипцы́|m}}"),

        # feminine reducibles
        (u"царевна", u"{{ru-noun-table|1|царе́вн|а|царе́вен|a=an}}"),
        # the following is bianimate
        (u"кукла", u"{{ru-noun-table|1|ку́кл|а|ку́кол|a=bo}}"),
        (u"свадьба", u"{{ru-noun-table|1|сва́дьб|а|сва́деб}}"),
        # the following has irreg dereduced form тем
        (u"княжна", u"{{ru-noun-table|2|княжн|а|княжо́н|a=an}}"),
        (u"тьма", u"{{ru-noun-table|2|тьм|а|тем|n=s}}"),
        (u"кайма", u"{{ru-noun-table|2|кайм|а|каём}}"),
        (u"сосна", u"{{ru-noun-table|4|со́сн|а|со́сен}}"),
        # the following is type 1*d // 1f
        (u"кошма", u"{{ru-noun-table|4|ко́шм|а|ко́шем}}"),
        # the following is masc anim or fem anim
        (u"чухна", u"{{ru-noun-table|4|чу́хн|а|чу́хон|a=an}}"),
        (u"чухна", u"{{ru-noun-table|4|чу́хна|m|чу́хон|a=an}}"),
        (u"тюрьма", u"{{ru-noun-table|4|тю́рьм|а|тю́рем}}"),
        # the following is type 1*a // 1*e
        (u"бубна", u"{{ru-noun-table|бу́бна||бу́бен}}"),
        (u"бубна", u"{{ru-noun-table|e|бу́бна||бубён}}"),
        (u"бубна", u"{{ru-noun-table|1,5|бу́бна|*}}"),
        # the following is type 1*f // 1*d
        (u"копна", u"{{ru-noun-table|6|копн|а|копён}}"),
        (u"копна", u"{{ru-noun-table|4|копна||ко́пен}}"),
        (u"копна", u"{{ru-noun-table|6,4|копн|а*}}"),
        (u"песня", u"{{ru-noun-table|1|пе́сн|я|пе́сен}}"),
        (u"капля", u"{{ru-noun-table|1|ка́пл|я|ка́пель}}"),
        (u"башня", u"{{ru-noun-table|1|ба́шн|я|ба́шен}}"),
        # the following has irreg dereduced form ку́хонь
        (u"кухня", u"{{ru-noun-table|ку́хня|gen_pl=ку́хонь}}"),
        (u"кухня", u"{{ru-noun-table|1|ку́хн|я|ку́хонь}}"),
        (u"спальня", u"{{ru-noun-table|1|спа́льн|я|спа́лен}}"),
        (u"бойня", u"{{ru-noun-table|1|бо́йн|я|бо́ен}}"),
        # the following has irreg dereduced form шестерён
        (u"шестерня", u"{{ru-noun-table|2|шестерн|я|gen_pl=шестерён}}"),
        (u"шестерня", u"{{ru-noun-table|2|шестерн|я|шестерён}}"),
        # the following is type пе́тля 2*a [// петля́ 2*d]
        (u"петля", u"{{ru-noun-table|1,4|пе́тл|я|пе́тель}}"),
        (u"земля", u"{{ru-noun-table|d'|земля́|gen_pl=земе́ль}}"),
        (u"земля", u"{{ru-noun-old|4*|земл|я́|gen_pl=земе́ль}}"),
        (u"деревня", u"{{ru-noun-table|5|дере́вн|я|gen_pl=дереве́нь}}"),
        (u"деревня", u"{{ru-noun-table|5|дере́вн|я|дереве́нь}}"),
        (u"сказка", u"{{ru-noun-table|1|ска́зк|а|ска́зок}}"),
        (u"точка", u"{{ru-noun-table|1|то́чк|а|то́чек}}"),
        (u"нивхка", u"{{ru-noun-table|1|нивхк|а|ни́вхок}}"),
        (u"шпилька", u"{{ru-noun-table|1|шпи́льк|а|шпи́лек}}"),
        (u"чайка", u"{{ru-noun-table|1|ча́йк|а|ча́ек|a=a}}"),
        (u"кабарга", u"{{ru-noun-table|2|кабарг|а|кабаро́г|a=an}}"),
        (u"кишка", u"{{ru-noun-table|2|кишк|а|кишо́к}}"),
        # the following is sg-only; theoretical gen pl *таёг
        (u"тайга", u"{{ru-noun-table|2|тайг|а|n=sg}}"),
        (u"тайга", u"{{ru-noun-table|2|тайг|а|таёг}}"),
        # the following is type 3*d // 3*b with irreg dereduced form ки́рок (b only)
        (u"кирка", u"{{ru-noun-table|4,2|кирк|а́|*|gen_pl=ки́рок}}"),
        # the following is type 3*f // 3*d with irreg dereduced form серёг (f only)
        (u"серьга", u"{{ru-noun-table|f,d|серьга́|gen_pl=серёг}}"),
        (u"серьга", u"{{ru-noun-table|6,4|серьг|а́|серёг}}"),
        # the following is type 3*f' // 3*d
        (u"доска", u"{{ru-noun-table|4|до́ск|а|до́сок}}"),
        (u"дверца", u"{{ru-noun-table|1|две́рц|а|две́рец}}"),
        (u"крепостца", u"{{ru-noun-table|крепостц|а́|крепосте́ц"),
        # the following has irreg dereduced form ове́ц
        (u"овца", u"{{ru-noun-table|d|овца́|gen_pl=ове́ц|a=an}}"),
        (u"овца", u"{{ru-noun-table|4|овц|а́|ове́ц|a=an}}"),
        (u"гостья", u"{{ru-noun-table|1|го́ст|ья|a=an}}"),
        (u"гостья", u"{{ru-noun-table|1|го́ст|ья|го́стий|a=an}}"),
        (u"вайя", u"{{ru-noun-table|1|вай|я|ва́ий}}"),
        (u"статья", u"{{ru-noun-table|2|стат|ья}}"),
        # the following has irreg dereduced form свине́й [but apparently
        # this is normal in decl ья in d and d', and we handle it as normal]
        (u"свинья", u"{{ru-noun-table|4|сви́н|ья|a=an}}"),
        (u"любовь", u"{{ru-noun-table|b'|любо́вь|f*}}"),
        (u"любовь", u"{{ru-noun-table|2|любв|ь-f|любо́вь}}"),
        (u"вошь", u"{{ru-noun-table|b'||f*|a=an}}"),
        (u"вошь", u"{{ru-noun-table|2|вш|ь-f|вошь|a=an}}"),
        (u"церковь", u"{{ru-noun-table|5|це́ркв|ь-f|це́рковь|ins_sg=це́рковью|dat_pl=церквя́м,церква́м|ins_pl=церквя́ми,церква́ми|pre_pl=церквя́х,церква́х}}"),
        # special (2)
        (u"сходня", u"{{ru-noun-table|2|сходн|я(2)}}"),
        # optional special (2)
        (u"путля", u"{{ru-noun-table|пу́тля|*|or||*(2)}}"),
        (u"путля", u"{{ru-noun-table|1|пу́тл|я|пу́тель}}"),
        (u"лютня", u"{{ru-noun-table|лю́тня|*|or||*(2)}}"),
        # special ё
        (u"весна", u"{{ru-noun-table|4|вёсн|а|вёсен}}"),
        (u"весна", u"{{ru-noun-old|4|вёсн|а|вёсенъ}}"),
        # pl-only
        (u"козлы", u"{{ru-noun-table|1|козл|а|ко́зел|n=p}}"),
        (u"мохны", u"{{ru-noun-table|мохн|а́|мохо́н|n=plr}}"),
        # the following is но́жны type 1*a with archaic ножны́ type 1*b
        (u"ножны", u"{{ru-noun-table|1|но́жны|f*|n=p}}"),
        (u"ножны", u"{{ru-noun-table|b|ножна||ножо́н|n=p}}"),
        # the following is type 1*a // 1*e
        (u"бубны", u"{{ru-noun-table|бу́бн|а|бу́бен}}"),
        (u"бубны", u"{{ru-noun-table|5|бу́бн|а|бубён}}"),
        (u"капли", u"{{ru-noun-table|1|ка́пл|я|ка́пель|n=ppp}}"),
        # the following has irreg gen pl form полдён or полдне́й
        (u"полдни", u"{{ru-noun-table|e|полдня||полдён|n=pl}}"),
        (u"санки", u"{{ru-noun-table|a|санк|а|са́нок|n=p}}"),
        (u"штанишки", u"{{ru-noun-table|штани́шки|f*}}"),
        (u"штанишки", u"{{ru-noun-table|штани́шки|f*|n=pl}}"),
        (u"штанишки", u"{{ru-noun-table|1|штани́шк|а|штани́шек|n=pl}}"),
        (u"грабельки", u"{{ru-noun-table|гра́бельки|f*|n=ppp}}"),
        (u"грабельки", u"{{ru-noun-table|гра́бельк|а|гра́белек|n=ppp}}"),
        (u"обойки", u"{{ru-noun-table|a|обо́йк|а|обо́ек|n-p}}"),
        # the following is masc or fem, type 5*a
        (u"портки", u"{{ru-noun-table|портки́|m*|n=pl}}"),
        (u"портки", u"{{ru-noun-table|b||f*}}"),
        (u"портки", u"{{ru-noun-table|b|портк|а|порто́к|n=p}}"),
        # the following is type 3*e // archaic 3*a with irreg gen pl form де́нег (a only)
        (u"деньги", u"{{ru-noun-table|5,1|деньг|а|де́нег|n=pl}}"),
        (u"деньги", u"{{ru-noun-table|5|деньг|а|де́нег|n=pl}}"),
        (u"деньги", u"{{ru-noun-table|1|деньг|а|де́нег|n=pl}}"),
        # the following is fem or masc, type 5*a
        (u"пяльцы", u"{{ru-noun-table|пя́льц|а|пя́лец|n=plur}}"),
        (u"пяльцы", u"{{ru-noun-table|пя́лец||пя́льц|n=plur}}"),
        # we purposely put the stress oddly in the following to see what happens
        (u"пяльцы", u"{{ru-noun-table|1|пяльцы́|f*}}"),

        # neuter reducibles
        (u"кресло", u"{{ru-noun-table|1|кре́сл|о|кре́сел}}"),
        (u"брашно", u"{{ru-noun-table|1|брашн|о|бра́шен}}"),
        # the following is 1*a // 1a
        (u"тягло", u"{{ru-noun-table|тя́гл|о|тя́гол}}"),
        # the following has irreg dereduced form зол
        (u"зло", u"{{ru-noun-table|зло́|gen_pl=зо́л|pltailall=*|notes=* The plural forms are not used, except for the genitive plural [[зол]].}}"),
        (u"зло", u"{{ru-noun-table|||зол}}"),
        # the following has irreg dereduced form ма́сел
        (u"масло", u"{{ru-noun-table|c|ма́сло|*|gen_pl=ма́сел}}"),
        (u"масло", u"{{ru-noun-table|c|ма́сл|о|ма́сел}}"),
        (u"полотно", u"{{ru-noun-table|4|поло́тн|о|поло́тен}}"),
        # accent in stem omitted; should be put back correctly
        (u"полотно", u"{{ru-noun-table|4|полотн|о|поло́тен}}"),
        (u"окно", u"{{ru-noun-table|4|о́кн|о|о́кон}}"),
        (u"окно", u"{{ru-noun-old|4|о́кн|о|о́конъ}}"),
        (u"письмо", u"{{ru-noun-table|4|пи́сьм|о|пи́сем}}"),
        (u"блюдце", u"{{ru-noun-table|1|блю́дц|о|блю́дец}}"),
        (u"зеркальце", u"{{ru-noun-table|1|зе́ркальце||зе́ркалец}}"),
        (u"словцо", u"{{ru-noun-table|словц|о́|слове́ц}}"),
        (u"сельцо", u"{{ru-noun-table|2|се́льц|о|селе́ц}}"),
        (u"сердце", u"{{ru-noun-table|3|се́рдц|о|серде́ц}}"),
        (u"сердце", u"{{ru-noun-old|3|се́рдц|о|серде́цъ}}"),
        (u"долотцо", u"{{ru-noun-table|4|долотцо||доло́тец}}"),
        (u"долотцо", u"{{ru-noun-table|4|долотц|о́|доло́тец}}"),
        (u"долотцо", u"{{ru-noun-table|4|доло́тц|о́|доло́тец}}"),
        (u"долотцо", u"{{ru-noun-table|4|доло́тц|о|доло́тец}}"),
        # the following has irreg dereduced form коле́ц
        (u"кольцо", u"{{ru-noun-table|4|кольц|о|коле́ц}}"),
        # the following has irreg dereduced form яи́ц
        (u"яйцо", u"{{ru-noun-table|d|яйцо́|gen_pl=яи́ц}}"),
        (u"яйцо", u"{{ru-noun-table|4|яйц|о́|яи́ц}}"),
        (u"крыльцо", u"{{ru-noun-table|6|крыльц|о|крыле́ц}}"),
        (u"ущелье", u"{{ru-noun-table|1|уще́л|ье}}"),
        (u"питьё", u"{{ru-noun-table|2|пит|ьё}}"),
        (u"питьё", u"{{ru-noun-old|2|пит|ьё}}"),
        (u"копьё", u"{{ru-noun-table|4|ко́п|ьё}}"),
        # special case (1)
        (u"ведёрко", u"{{ru-noun-table|1|ведёрк|о-и|ведёрок}}"),
        (u"окошко", u"{{ru-noun-table|1|око́шк|о-и|око́шек}}"),
        # the following is у́шко 3*a(1) [// ушко́ 3*d(1)]
        (u"ушко", u"{{ru-decl-noun|a,d|у́шко|*(1)}}"),
        (u"ушко", u"{{ru-decl-noun|1,4|у́шко|*(1)}}"),
        (u"ушко", u"{{ru-decl-noun|4|у́шк|о-и|у́шек}}"),
        # special case (2)
        (u"облачко", u"{{ru-decl-noun|3|о́блачко|(2)}}"),
        (u"болотце", u"{{ru-decl-noun|боло́тц|о(2)}}"),
        (u"платье", u"{{ru-noun-table|пла́тье|(2)}}"),
        (u"платье", u"{{ru-noun-table|1|плат|ье(2)}}"),
        (u"жнивьё", u"{{ru-noun-table|d}}"),
        (u"жнивьё", u"{{ru-noun-table|4|жнив|ьё}}"),
        # optional special case (2)
        (u"волоконце", u"{{ru-noun-table|волоко́нц|е(2)"),
        (u"волоконце", u"{{ru-noun-table|волоко́нц|е|волоко́нец"),
        (u"деревце", u"{{ru-noun-table|1|де́ревц|о|де́ревец}}"),
        (u"деревцо", u"{{ru-noun-table|2|деревцо́||дереве́ц}}"),
        (u"верховье", u"{{ru-noun-table|1|верхо́в|ье|верхо́вий}}"),
        # special case (1)(2)
        # the following is дре́вко 3*a(1)(2) [// древко́ 3*b(1)(2)]
        (u"древко", u"{{ru-noun-table|1,2|дре́вк|о-и}}"),
        (u"древко", u"{{ru-noun-table|1,2|дре́вк|*о-и}}"),
        (u"древко", u"{{ru-noun-table|2|дре́вк|о-и}}"),
        (u"очко", u"{{ru-noun-table|очк|о́-и*(2)}}"),
        # special case ё
        (u"весло", u"{{ru-noun-table|4|вёсл|о|вёсел}}"),
        (u"весло", u"{{ru-noun-old|4|вёсл|о|вёселъ}}"),
        (u"стекло", u"{{ru-noun-table|4|стёкл|о|стёкол}}"),
        (u"озерцо", u"{{ru-noun-table|4|озёрц|е|озёрец}}"),
        # masculine neuter-form
        # special case (1)
        (u"сиверко", u"{{ru-noun-table|си́верк|о-и|си́верок}}"),
        (u"народишко", u"{{ru-noun-table|наро́дишк|о-и|наро́дишек}}"),
        (u"домишко", u"{{ru-noun-table|доми́шко|(1)|доми́шек}}"),
        (u"соловейко", u"{{ru-noun-table|1|солове́йко|(1)|солове́ек|a=an}}"),
        # special case (1)(2)
        (u"воронко", u"{{ru-noun-table|воронкó|(1)(2)|a=an}}"),
        (u"воронко", u"{{ru-noun-table|воронк|ó-и(2)|a=an"),
        # special case (2)
        (u"подмастерье", u"{{ru-noun-table|1|подмасте́р|ье(2)|a=an}}"),
        # pl-only
        (u"кросна", u"{{ru-noun-table|1|кросн|о|кро́сен|n=pl}}"),
        (u"воротца", u"{{ru-noun-table|воро́тц|о|воро́тец|n=pl}}"),
        (u"воротца", u"{{ru-noun-table|воро́тца|n|воро́тец}}"),
        (u"перильца", u"{{ru-noun-table|пери́льц|о|пери́лец|n=pl}}"),
        (u"перильца", u"{{ru-noun-table|пери́льца|n|пери́лец}}"),
        # special case (2)
        (u"хлопья", u"{{ru-noun-table|1|хло́п|ье(2)|n=pl}}"),
        (u"хлопья", u"{{ru-noun-table|хло́пья|n(2)|n=pl}}"),
        (u"хлопья", u"{{ru-noun-table|хло́пья|*n(2)}}"),

        # masculine non-reducibles
        # accent a
        (u"завод", u"{{ru-noun-table|1|заво́д}}"),
        (u"артист", u"{{ru-noun-table|1|арти́ст|a=an}}"),
        (u"портфель", u"{{ru-noun-table|1|портфе́л|ь-m}}"),
        (u"житель", u"{{ru-noun-table|1|жи́тел|ь-m|a=an}}"),
        (u"чайник", u"{{ru-noun-table|ча́йник|a=both}}"),
        (u"чайник", u"{{ru-noun-table|1|ча́йник|a=both}}"),
        (u"бульдог", u"{{ru-noun-table|1|бульдо́г|a=an}}"),
        (u"марш", u"{{ru-noun-table|1|ма́рш}}"),
        (u"товарищ", u"{{ru-noun-table|1|това́рищ|a=an}}"),
        (u"месяц", u"{{ru-noun-table|1|ме́сяц}}"),
        (u"принц", u"{{ru-noun-table|1|при́нц|a=an}}"),
        (u"случай", u"{{ru-noun-table|1|слу́ча|й}}"),
        (u"случай", u"{{ru-noun-old|1|слу́ча|й}}"),
        (u"герой", u"{{ru-noun-table|1|геро́|й|a=an}}"),
        (u"сценарий", u"{{ru-noun-table|1|сцена́ри|й}}"),
        (u"викарий", u"{{ru-noun-table|1|вика́ри|й|a=an}}"),
        # accent b
        (u"топор", u"{{ru-noun-table|b|топо́р}}"),
        (u"бегун", u"{{ru-noun-table|2|бегу́н|a=an}}"),
        (u"словарь", u"{{ru-noun-table|2|слова́р|ь-m}}"),
        (u"карась", u"{{ru-noun-table|2|карас|ь-m|кара́сь|a=an}}"),
        (u"пирог", u"{{ru-noun-table|2|пиро́г}}"),
        (u"рыбак", u"{{ru-noun-table|2|рыба́к|a=an}}"),
        (u"нож", u"{{ru-noun-table|2|но́ж}}"),
        (u"богач", u"{{ru-noun-table|2|бога́ч|a=an}}"),
        (u"кострец", u"{{ru-noun-table|b|костре́ц}}"),
        (u"кузнец", u"{{ru-noun-table|2|кузне́ц||a=a}}"),
        (u"лишай", u"{{ru-noun-table|b|лиша́й}}"),
        (u"холуй", u"{{ru-noun-table|b|холу́й|a=an}}"),
        (u"кий", u"{{ru-noun-table|3,2|ки́|й|pre_sg=ки́и,ки́е,кие́}}"),
        # accent c
        (u"дар", u"{{ru-noun-table|3|да́р}}"),
        (u"плуг", u"{{ru-noun-table|3|плу́г}}"),
        (u"харч", u"{{ru-noun-table|c||||a=in}}"),
        (u"буй", u"{{ru-noun-table|3|бу́|й}}"),
        # accent d
        (u"кол", u"{{ru-noun-table|4|ко́л|-ья|or|2|loc=на +}}"),
        (u"казак", u"{{ru-noun-table|2,4|казак|||a=an}}"),
        # accent e
        (u"зуб", u"{{ru-noun-table|5|зу́б|or||-ья|pltail=*|notes=* Technical.}}"),
        (u"голубь", u"{{ru-noun-table|5|го́луб|ь-m|a=an}}"),
        (u"волк", u"{{ru-noun-table|5|во́лк|a=an}}"),
        (u"обруч", u"{{ru-noun-table|5|о́бруч}}"),
        # accent f
        (u"конь", u"{{ru-noun-table|6|кон|ь-f|a=aaa}}"),
        (u"путь", u"{{ru-noun-table|2|пу́т|ь-f|ins_sg=путём}}"),
        # suffixal (1°, 3°)
        (u"римлянин", u"{{ru-noun-table|1|ри́млян|ин|a=an}}"),
        (u"южанин", u"{{ru-noun-table|1|южа́н|ин|a=an}}"),
        (u"армянин", u"{{ru-noun-table|4|армя́н|ин|a=an}}"),
        (u"боярин", u"{{ru-noun-table|1|боя́р|ин|a=an}}"),
        # the following is also special case (1)
        (u"господин", u"{{ru-noun-table|2|госпо́д|ин|a=an|nom_pl=господа́}}"),
        # the following is 1°c (c ∆) // 1a [first plural in -ья]
        (u"шурин", u"{{ru-noun-table|3|шу́рин|-ья||шур|a=an}}"),
        (u"маслёнок", u"{{ru-noun-table|2|масл|ёнок}}"),
        (u"опёнок", u"{{ru-noun-table|опёнок}}"),
        (u"цыплёнок", u"{{ru-noun-table|2|цыпл|ёнок|a=an}}"),
        (u"мышонок", u"{{ru-noun-table|2|мыш|онок|a=an}}"),
        (u"цыплёночек", u"{{ru-noun-table|b|цыплёночек|a=a}}"),
        (u"мышоночек", u"{{ru-noun-table|2|мыш|о́ночек|a=an}}"),
        # the following is 3*b//3°d (c ∆)
        (u"щенок", u"{{ru-noun-table|b|щено́к|*|or|d||*(1)(2)||щеня́т|a=an}}"),
        # special case (1)
        (u"рукав", u"{{ru-noun-table|2|рука́в|-а}}"),
        (u"мастер", u"{{ru-noun-table|3|ма́стер|-а|a=an}}"),
        (u"якорь", u"{{ru-noun-table|3|я́кор|ь-я}}"),
        (u"обшлаг", u"{{ru-noun-table|2|обшла́г|(1)}}"),
        (u"округ", u"{{ru-noun-table|3|о́круг|-а}}"),
        (u"сторож", u"{{ru-noun-table|3|сто́рож|-а|a=an}}"),
        (u"край", u"{{ru-noun-table|3|кра́|й-я|par=кра́ю|loc=краю́}}"),
        # special case (2)
        (u"грузин", u"{{ru-noun-table|1|грузи́н|(2)|a=an}}"),
        (u"раз", u"{{ru-noun-table|c|раз|(2)|par=+}}"),
        (u"волос", u"{{ru-noun-table|5|во́лос|(2)}}"),
        (u"сапог", u"{{ru-noun-table|b|сапо́г|(2)}}"),
        (u"герц", u"{{ru-noun-table|1|ге́рц|(2)}}"),
        # optional special case (2)
        (u"мадьяр", u"{{ru-noun-table|1|мадья́р|or||(2)"),
        (u"чувяк", u"{{ru-noun-table|3|чувя́к|or||(2)"),
        # special cases (1)(2)
        (u"глаз", u"{{ru-noun-table|3|гла́з|(1)(2)|loc=+}}"),
        # ё special
        (u"осётр", u"{{ru-noun-table|2|осётр|a=an}}"),
        (u"мёд", u"{{ru-noun-table|3|мёд|par=+|loc=+}}"),
        (u"чёрт", u"{{ru-noun-table|5|чёрт|/ь-m|nom_pl=че́рти|a=an|acc_sg=чёрта|gen_sg=чёрта,черта́*|notes=&lt;sup>*&lt;/sup> Only in the expression ''ни черта́''.}}"),
        (u"жёлудь", u"{{ru-noun-table|5|жёлуд|ь-m}}"),
        (u"ёж", u"{{ru-noun-table|2|ёж}}"),
        (u"жёрнов", u"{{ru-noun-table|3|жёрнов|-а}}"),
        (u"шёлк", u"{{ru-noun-table|3|шёлк|-а}}"),
        # plural only
        (u"анналы", u"{{ru-noun-table|1|анна́л|n=pl}}"),
        (u"весы", u"{{ru-noun-table|2|вес|n=pl}}"),
        (u"дебри", u"{{ru-noun-table|1|де́брь|m|n=pl}}"),
        (u"кочкари", u"{{ru-noun-table|2|кочкари|m|n=plural}}"),
        (u"сани", u"{{ru-noun-table|5|са́нь|m|n=pl}}"),
        (u"трусики", u"{{ru-noun-table|3|тру́сик|n=pl}}"),
        (u"духи", u"{{ru-noun-table|2|дух|n=pl}}"),
        (u"пассатижи", u"{{ru-noun-table|1|пассати́ж|n=pl}}"),
        (u"щи", u"{{ru-noun-table|2|щ|ь-f|n=p}}"),
        (u"щи", u"{{ru-noun-old|2|щ|ь-f|n=p}}"),
        (u"дрожжи", u"{{ru-noun-table|6|дро́жж|а|n=pl}}"),
        (u"помои", u"{{ru-noun-table|помо́|й|n=pl}}"),
        (u"прелиминарии", u"{{ru-noun-table|1|прелимина́рий|n=p}}"),
        # plural only, special case (1)
        (u"леса", u"{{ru-noun-table|2|лес|-а|n=pl}}"),
        # singular feminine that we should also handle correctly
        (u"леса", u"{{ru-noun-table|4|лёс|а}}"),
        (u"зеленя", u"{{ru-noun-table|2|зелен|ь-я|n=pl}}"),
        (u"торока", u"{{ru-noun-table|торок|-а́|n=pl}}"),

        # feminine non-reducibles
        # accent a
        (u"карта", u"{{ru-noun-table|1|ка́рт|а}}"),
        (u"корова", u"{{ru-noun-table|a|коро́ва|a=an}}"),
        (u"неделя", u"{{ru-noun-table|1|неде́л|я}}"),
        (u"богиня", u"{{ru-noun-table|1|боги́н|я|a=an}}"),
        (u"книга", u"{{ru-noun-table|1|кни́г|а}}"),
        (u"собака", u"{{ru-noun-table|1|соба́к|а|a=an}}"),
        (u"туча", u"{{ru-noun-table|1|ту́ч|а}}"),
        (u"кассирша", u"{{ru-noun-table|1|касси́рш|а|a=an}}"),
        (u"улица", u"{{ru-noun-table|1|у́лиц|а}}"),
        (u"улица", u"{{ru-noun-old|1|у́лиц|а}}"),
        (u"волчица", u"{{ru-noun-table|1|волчи́ц|а|a=an}}"),
        (u"статуя", u"{{ru-noun-table|1|ста́ту|я}}"),
        (u"фея", u"{{ru-noun-table|фе́я|a=an}}"),
        (u"линия", u"{{ru-noun-table|1|ли́ни|я}}"),
        (u"фурия", u"{{ru-noun-table|1|фу́ри|я́}}"),
        (u"секвойя", u"{{ru-noun-table|1|секво́й|я}"),
        # accent b
        (u"похвала", u"{{ru-noun-table|2|похва́л|а}}"),
        (u"гюрза", u"{{ru-noun-table|2|гю́рз|а|a=an}}"),
        (u"стезя", u"{{ru-noun-table|2|стез|я}}"),
        (u"тля", u"{{ru-noun-table|2|тл|я|a=an}}"),
        (u"тля", u"{{ru-noun-table|a=an}}"),
        (u"острога", u"{{ru-noun-table|2|остро́г|а}}"),
        (u"карга", u"{{ru-noun-table|2|ка́рг|а|a=an}}"),
        (u"каланча", u"{{ru-noun-table|2|каланч|а}}"),
        (u"ханжа", u"{{ru-noun-table|2|ханж|а|a=an}}"),
        (u"маца", u"{{ru-noun-table|2|мац|а}}"),
        (u"колея", u"{{ru-noun-table|колея́}}"),
        (u"швея", u"{{ru-noun-table|швея́|a=an}}"),
        (u"лития", u"{{ru-noun-table|лити|я́}}"),
        # accent d
        (u"сирота", u"{{ru-noun-table|4|сиро́т|а|a=an}}"),
        (u"заря", u"{{ru-noun-table|d|заря́|||зор|dat_pl=зо́рям,заря́м*|ins_pl=зо́рями,заря́ми*|pre_pl=зо́рях,заря́х*|notes=* Archaic.}}"),
        (u"дуга", u"{{ru-noun-table|4|ду́г|а}}"),
        (u"межа", u"{{ru-noun-table|4|ме́ж|а}}"),
        (u"змея", u"{{ru-noun-table|4|зме́|я|a=a}}"),
        # accent d'
        (u"спина", u"{{ru-noun-table|4|спи́н|а|acc_sg=спи́ну}}"),
        (u"дрога", u"{{ru-noun-table|4*|дрог|а}}"),
        (u"душа", u"{{ru-noun-table|4*|душ|а́}}"),
        # accent e
        (u"доля", u"{{ru-noun-table|5|до́л|я}}"),
        # accent f
        (u"губа", u"{{ru-noun-table|6|гу́б|а}}"),
        (u"слобода", u"{{ru-noun-table|f|слобода́}}"),
        (u"ноздря", u"{{ru-noun-table|6|но́здр|я́}}"),
        (u"простыня", u"{{ru-noun-table|6|про́стын|я|gen_pl=просты́нь}}"),
        (u"слега", u"{{ru-noun-table|6,4|слег|а́}}"),
        (u"вожжа", u"{{ru-noun-table|6|во́жж|а}}"),
        # accent f'
        (u"гора", u"{{ru-noun-table|6*|го́р|а}}"),
        (u"борода", u"{{ru-noun-table|f'|борода́}}"),
        (u"рука", u"{{ru-noun-table|6*|ру́к|а}}"),
        (u"рука", u"{{ru-noun-old|6*|ру́к|а}}"),
        # accent a, 3rd decl
        (u"тетрадь", u"{{ru-noun-table|1|тетра́д|ь-f}}"),
        (u"лань", u"{{ru-noun-table|1|ла́н|ь-f|a=an}}"),
        (u"пустошь", u"{{ru-noun-table|1|пу́стош|ь-f}}"),
        # accent e, 3rd decl
        (u"площадь", u"{{ru-noun-table|5|пло́щад|ь-f}}"),
        (u"стерлядь", u"{{ru-noun-table|5|сте́рляд|ь-f|a=an}}"),
        (u"ночь", u"{{ru-noun-table|5|но́ч|ь-f|loc=ночи́}}"),
        (u"ночь", u"{{ru-noun-old|5|но́ч|ь-f|loc=ночи́}}"),
        (u"мышь", u"{{ru-noun-table|5|мы́ш|ь-f|a=an}}"),
        # accent f'', 3rd decl
        (u"грудь", u"{{ru-noun-table|6|гру́д|ь-f}}"),
        (u"грудь", u"{{ru-noun-table|5|гру́д|ь-f|title=Dated declension of {{lang|ru|гру́дь}}}}"),
        (u"грудь", u"{{ru-noun-old|6,5|гру́д|ь-f}}"),
        (u"глушь", u"{{ru-noun-table|6|глушь|f|n=sing}}"),
        # special case (2)
        # the following is given as masc anim or fem anim
        (u"рохля", u"{{ru-noun-table|ро́хля|m(2)|a=an}}"),
        (u"корча", u"{{ru-noun-table|ко́рча|(2)}}"),
        # optional special case (2)
        # the following is given as masc anim or fem anim
        (u"растеря", u"{{ru-noun-table|расте́ря|or||(2)|a=an}}"),
        (u"верша", u"{{ru-noun-table|ве́рша|or||(2)}}"),
        # ё special
        (u"пелена", u"{{ru-noun-table|2|пелён|а}}"),
        (u"звезда", u"{{ru-noun-table|4|звезд|а́;ё}}"),
        (u"слеза", u"{{ru-noun-table|6|слёз|а}}"),
        (u"железа", u"{{ru-noun-table|6|железа́|;ё}}"),
        (u"середа", u"{{ru-noun-table|6*|серед|а;ё}}"),
        (u"стега", u"{{ru-noun-table|стега́|;ё}}"),
        # The following is 3f' // 3f
        (u"щека", u"""{{ru-noun-table|6*,4|щёк|а|acc_sg=щёку,щеку́1|dat_pl=щека́м,щёкам1|acc_pl=щеки́,щёки|ins_pl=щека́ми,щёками2|pre_pl=щека́х,щёках1|notes=
            # Obsolete
            # Colloquial
            }}"""),
        # The following is 4b // 4f
        (u"дежа", u"{{ru-noun-table|2,6|дежа́|;ё}}"),
        (u"щёлочь", u"{{ru-noun-table|5|щёлоч|ь-f}}"),
        # masculine fem-type words, all animate
        (u"мужчина", u"{{ru-noun-table|1|мужчи́н|а|a=a}}"),
        (u"мужчина", u"{{ru-noun-old|1|мужчи́н|а|a=a}}"),
        (u"дядя", u"{{ru-noun-table|дя́дя|(2)|a=an}}"),
        (u"дядя", u"{{ru-noun-table|c|дя́дя|-ья|a=an}}"),
        (u"судия", u"{{ru-noun-table|судия́|a=an|dat_sg=судии́|pre_sg=судии́|ins_sg=судие́й|gen_pl=су́дий}}"),
        # pl-only words
        (u"вилы", u"{{ru-noun-table|1|ви́л|а|n=pl}}"),
        (u"бразды", u"{{ru-noun-table|2|бразд|а́|n=pl}}"),
        (u"похороны", u"{{ru-noun-table|e|по́хороны|f|n=plur}}"),
        (u"брюки", u"{{ru-noun-table|1|брю́к|а|n=pl}}"),
        (u"ладоши", u"{{ru-noun-table|ладо́ши|f}}"),
        (u"ножницы", u"{{ru-noun-table|1|но́жница|n=pl}}"),
        (u"сатурналии", u"{{ru-noun-table|сатурна́ли|а|n=p}}"),

        # neuter non-reducible
        # accent a
        (u"болото", u"{{ru-noun-table|1|боло́т|о}}"),
        (u"чадо", u"{{ru-noun-table|1|ча́д|о|a=a}}"),
        (u"чадо", u"{{ru-noun-old|1|ча́д|о|a=a}}"),
        (u"горе", u"{{ru-noun-table|1|го́р|е}}"),
        (u"благо", u"{{ru-noun-table|1|бла́г|о}}"),
        (u"жилище", u"{{ru-noun-table|1|жили́щ|о}}"),
        (u"чудовище", u"{{ru-noun-table|1|чудо́вищ|о|a=a}}"),
        (u"чудовище", u"{{ru-noun-old|1|чудо́вищ|о|a=a}}"),
        (u"солнце", u"{{ru-noun-table|1|со́лнц|о}}"),
        (u"здание", u"{{ru-noun-table|1|зда́ни|е}}"),
        # accent b
        (u"вещество", u"{{ru-noun-table|2|веще́ств|о}}"),
        (u"ружьецо", u"{{ru-noun-table|2|ружье́ц|о}}"),
        (u"житие", u"{{ru-noun-table|2|жити́|е́}}"),
        (u"бытие", u"{{ru-noun-table|2|быти́|е́}}"),
        # accent c
        (u"место", u"{{ru-noun-table|3|ме́ст|о}}"),
        (u"поле", u"{{ru-noun-table|3|по́л|е}}"),
        (u"войско", u"{{ru-noun-table|3|во́йск|о}}"),
        # accent d
        (u"вино", u"{{ru-noun-table|4|ви́н|о}}"),
        (u"лицо", u"{{ru-noun-table|4|ли́ц|о}}"),
        (u"лицо", u"{{ru-noun-old|4|ли́ц|о}}"),
        # accent e
        (u"ухо", u"{{ru-noun-table|e||(1)||уш|notes=''Note on irregular forms — '''''{{lang|ru|у́ши}}''''' is actually an old nominative [[dual]] form for neuter nouns.}}"),
        # accent f
        (u"тавро", u"{{ru-noun-table|4,6|та́вр|о́}}"),
        # 8°a, ё (c ∆) [the whole plural]
        (u"знамя", u"{{ru-noun-table|зна́|мя/о||знамён}}"),
        (u"знамя", u"{{ru-noun-old|зна́|мя/о||знамён}}"),
        # 8°c, ё
        (u"имя", u"{{ru-noun-table|3|и́|мя}}"),
        (u"имя", u"{{ru-noun-old|3|и́|мя}}"),
        # 8°c, ё (c ∆) [gen pl]
        (u"семя", u"{{ru-noun-table|3|се́|мя|gen_pl=семя́н}}"),
        # special case (1)
        (u"яблоко", u"{{ru-noun-table|1|я́блок|о-и}}"),
        (u"молоко", u"{{ru-noun-table|2|моло́к|о|n=s}}"),
        # the following has irregular gen pl
        (u"плечо", u"{{ru-noun-table|6|пле́ч|о-и|gen_pl=пле́ч}}"),
        (u"плечо", u"{{ru-noun-old|6|пле́ч|о-и|gen_pl=пле́чъ}}"),
        # special case (2)
        (u"облако", u"{{ru-noun-table|c|о́блако|(2)}}"),
        (u"остриё", u"{{ru-noun-table|остриё|(2)}}"),
        # special case (1)(2), animate, neuter or masculine
        (u"мазло", u"{{ru-noun-table|мазл|о́-и(2)}}"),
        (u"личико", u"{{ru-noun-table|ли́чик|о(1)(2)}}"),
        # ё special
        (u"веретено", u"{{ru-noun-table|4|веретён|о}}"),
        # masc neut-type words
        # the following is optionally special case (1), and animate
        (u"сверлило", u"{{ru-noun-table|1|сверли́ло|m}}"),
        # the following is special case (1)
        (u"когтище", u"{{ru-noun-table|a|когти́щ|о-и}}"),
        # the following is optionally special case (1)
        (u"домище", u"{{ru-noun-table|1|доми́щ|о-и}}"),
        (u"домище", u"{{ru-noun-table|1|доми́що}}"),
        (u"домище", u"{{ru-noun-table|a|доми́щ|о}}"),
        (u"домище", u"{{ru-noun-table|1|доми́щ|о́}}"),
        (u"домище", u"{{ru-noun-table|1|доми́щ|е}}"),
        # the following is animate and special case (1); this one, and likely
        # the two before it, have additional colloquial feminine endings in
        # the singular; see section 4, page 74
        (u"волчище", u"{{ru-noun-table|1|волчи́щ|о-и|a=an}}"),
        # pl only
        (u"ворота", u"{{ru-noun-table|1|воро́т|о|n=p}}"),
        (u"ворота", u"{{ru-noun-old|1|воро́т|о|n=p}}"),
        (u"уста", u"{{ru-noun-table|2|у́ст|о|n=pl}}"),
        (u"войска", u"{{ru-noun-table|2|войска́|n|n=p}}"),
        (u"прения", u"{{ru-noun-table|1|пре́ни|е|n=plu}}"),
        # the following is also ё special
        (u"письмена", u"{{ru-noun-table|2|письмёна|n}}"),
        (u"письмена", u"{{ru-noun-table|2|письмёно|n=ppp}}"),
        (u"письмена", u"{{ru-noun-table|письмён|о́|n=ppp}}"),
        # adjectival nouns
        (u"целко́вый", u"{{ru-noun-table|целко́вый|+}}"),
    ]

def ignore_page(page):
  if not isinstance(page, basestring):
    page = unicode(page.title())
  if re.search(r"^(Template|Template talk|Appendix|Appendix talk|User|User talk|Talk):", page):
    return True
  return False

if pargs.test:
  i = 0
  for title, text in testdata:
    i += 1
    if pargs.start and i < pargs.start:
      continue
    if pargs.end and i > pargs.end:
      break
    process_page_data(i, title, text, save=pargs.save, verbose=pargs.verbose, offline=pargs.offline)
else:
  if pargs.file:
    do_pages = yield_pages(pargs.file)
  else:
    do_pages = yield_ref_pages()
  for i, page in do_pages:
    if ignore_page(page):
      msg("Page %s %s: Skipping due to namespace" % (i, unicode(page.title())))
    else:
      msg("Page %s %s: Processing" % (i, unicode(page.title())))
      process_page(i, page, save=pargs.save, verbose=pargs.verbose)
