#!/usr/bin/env python
# -*- coding: utf-8 -*-

# FIXME (many older FIXME's missing):

# 1. (DONE) Check that headword matches page name
# 2. (DONE) Auto-accent single-syllable words that would be accented by ru-pron,
#    when comparing multiple etymologies.
# 3. (DONE) When adding a pronunciation, check if it's the same as the page name
#    (presence of ё), or is monosyllabic, in which case use an empty
#    pronunciation.
# 4. (DONE) When deleting pronunciations because monosyllabic, also delete
#    pronunciations that have ё in them and are the same as the page title.
# 5. (DONE) Preserve order of pronunciations derived from headwords (cf. бора,
#    where the headword order is бора́,бо́ра but we get the order backwards
#    because we conver to a set and then sort as a list).
# 6. (DONE) Add ann=y if more than one pronun line (NOTE: fixed below when
#    reverse-translit support added to include ann=фоо for actual headword, and
#    only add ann= if multiple headwords, not just multiple pronun variants
#    of same headword ({{ru-noun form|а́бвера|tr=ábvera, ábvɛra|m-in}})).
# 7. (DONE) Warn if there appear to be missing pronunciations when we're done
#    (fewer ru-IPA than headwords).
# 8. (DONE) Support --cats so we can do just non-lemma forms.
# 9. (DONE) Reverse-transliterate Latin to Cyrillic to generate phon= arguments
#    for noun forms etc. with transliterations. Make sure to check whether
#    the transliteration is redundant. Need to check the following to see how
#    translit with semireduction is handled: адекватностям, амнезиями,
#    амнезиях, педерастиями. The last one has annotations with translit
#    (in fact, two translits for each of two forms, four total pronuns).
#    Make sure the dot-under doesn't show in the annotation.
# 10. (DONE) Clean up 'Existing pronunciation ... different' messages to not
#    pay attention to pos=,adj=,etc. and to output different messages when
#    differences only due to gem= or dotbelow.
# 11. (DONE) Remove no-longer-used adj=.
# 12. (DONE) When splitting translit on a comma, don't do it if there's a comma
#     in the headword.
# 13. (DONE) If there are inflection-of templates on the page, look up the
#     lemma page and fetch the ru-IPA template(s) on the page and the value(s)
#     of gem= for each. (Cache these results.) (If there are multiple
#     lemmas mentioned in inflection-of templates, look up the value(s) of
#     gem= for each page and combine them all.) If multiple such values, issue
#     a warning and do do nothing. If one value representing no gem= param,
#     do nothing. Else, add the value to each of the headword-derived ru-IPA
#     templates. When comparing existing pronunciations to headword-derived
#     pronunciations, if they differ and gem= is present in headword-derived
#     pronun, and not in existing pronun, issue a warning.
# 14. Allow fixing of situation where gem= is in headword-derived
#     pronun but not existing pronun, either given a list of lemmas whose forms
#     we should fix, or perhaps a list of those forms themselves.
# 15. (DONE) If a word in base lemma ends in a double consonant cf. абсцесс,
#     аламанн/алеманн, англо-норманн: If no other double consonant, add gem=n.
#     If other double consonant or if gem= is already present, issue warning
#     and do nothing. To check for double consonant, expand the base
#     pronunciation and look for geminates other than those caused by щ or ӂӂ.
# 16. (DONE) If form ends in a double consonant and gem=y or gem=opt is
#     present on the base form (cf. абелевых групп of абелева группа,
#     абсцисс of абсцисса, etc.), we need to remove this. FIXME: Need to
#     check if removing is safe, and issue warning if not. FIXME: Handle
#     multiple words (e.g. групп войск of группа войск).
# 17. (DONE, WITHOUT WARNING) Issue warning if grave accent or circumflex or ӂӂ
#     found in lemma pronun, and maybe more generally if lemma pronun doesn't
#     match headword pronun. Consider trying to copy the lemma pronun by
#     extracting out the stem.
# 18. (DONE) When creating the pronunciation of non-lemma forms, work out a
#     mapping from headword stems to pronunciation for the lemma and propagate
#     that to the non-lemma. Do this in a way that succeeds if there are
#     multiple headword stems and pronunciations, as long as we can match them
#     up. Matching up should allow for identity as well as extra grave accents,
#     circumflexes, э vs. е, зж/жж vs ӂӂ in the ru-IPA pronunciation. We
#     propagate by just trying to replace the corresponding headword stem with
#     the pronunciation (although this won't work for multiple words). This
#     will take care of many cases with translit, but we fall back on reverse
#     translit.
# 19. (DONE) Need to handle reduce/dereduce forms when mapping headword stems
#     to pronunciation.
# 20. (DONE) Need to handle adjectival nouns when mapping headword stems to
#     pronun, cf. несовершѐнноле́тний. Probably need to have some way of
#     indicating multiple possible stems in the stem->pronun mapping.
# 21. (DONE) Add dot-under to both Cyrillic and translit so it's present in
#     case we use the Cyrillic with propagated headword->pronun mapping rather
#     than reverse-transliterating, and remove the dot-under from the
#     annotation.
# 22. (DONE) Support manual_pronun_mapping for cases where stem->pronun mapping
#     fails.
# 23. (DONE) In reverse transliteration, check against original when -vo or -vó
#     is found, and convert to -го or -го́ if appropriate.
# 24. Consider fixing dereduce so it converts words in -ье to -ий
#     (e.g. варе́нье gen pl варе́ний).
# 25. (DONE) Support allow_unaccented to allow processing of 'unaccented'
#     forms, e.g. forms of бальза́м на́ душу.
# 26. (DONE) BUG: Need to add stress to monosyllabic pronunciations in lemmas
#     or we will end up with unstressed multisyllabic pronunciations in forms,
#     e.g. forms of бренд (phon=брэнд).
# 27. (DONE) BUG: In forms of педерастия, скучный, свитер etc. where we have
#     two stress variants each with two translits, we get 6 or 8 pronunciations
#     instead of 4.
# 28. (DONE) Canonicalize headword annotations by removing all the characters
#     that Module:ru-pron removes (various accents, ‿); similarly when
#     comparing to see whether |ann=y is possible or we need to explicitly
#     specify the annotation.
# 29. (DONE) Make skip_pages have regexes instead of just a list.
# 30. (DONE) Implement --override-pronun to remove existing pronunciation
#     before adding new one.
# 31. (DONE) Implement --non-lemma-file and --forms to specify a list of
#     lemmas whose non-lemma forms should be processed.
# 32. (DONE) When propagating from lemma to non-lemma, check for text before or
#     after the ru-IPA call and preserve that text. Example: бассе́йн
#     (with phon=бассэ́йн labeled {{a|St. Petersburg or dated}}), неонацист
#     (with phon=нэ̀онаци́ст and phon=нэ̀о̂наци́ст both labeled
#     {{i|less common or dated}}).
# 33. (DONE) Support verbs in match_headword_and_found_pronuns(), including
#     iotation.
# 34. (DONE) Display a special message when unable to match and would or will
#     save changes for a term.
# 35. (DONE) Remove accents in lemmas taken from --lemmas/--lemma-file.
# 36. (DONE) Generalize code that parses --forms.
# 37. (NOT A BUG BUT A MISTAKE IN THE PAGE ITSELF) BUG: Handles пузыриться
#     (with two possible stresses) wrong, e.g. пузырившийся ends up with
#     three pronunciations, one of which is wrong:
#     {{ru-IPA|пузы́ри́вшийся|ann=пузыри́вшийся}}. For some reason,
#     газировать (also with two possible stresses) is handled correctly.

# WORDS NEEDING SPECIAL HANDLING IN PRONUN:
#
# алла́х: Normally gem=n, but has gem=y as emphatic form.
# бессо́нница: First is geminated, second is not, needs respelling бессо́ница,
#    etc.
# колба Эрленмейера: should have two pronuns, first gem=n, second gem=y,
#    because (j) is optional when gem=n. Don't use gem=opt.
# Пикассо прямоугольчатый: two stress variants, gem=opt when Пика́ссо, but
#    not when Пикассо́.
# пресса/пресс: Forms of пре́сса have gem=y, forms of пресс have gem=n (?).
# расстаться, расставаться: Needs с(с) because first gemination is optional,
#    second isn't.

import pywikibot, re, sys, codecs, argparse
import difflib
import unicodedata
from collections import Counter

import blib
from blib import getparam, rmparam, msg, site

import rulib as ru
from rulib import AC, GR, DUBGR, CFLEX, DOTABOVE, DOTBELOW
import ru_reverse_translit

vowel_list = u"aeiouyɛəäëöü"
ipa_vowel_list = vowel_list + u"ɐɪʊɨæɵʉ"
ipa_vowels_re = "[" + ipa_vowel_list + "]"
ipa_vowels_c = "([" + ipa_vowel_list + "])"
non_ipa_vowels_re = "[^ " + ipa_vowel_list + "]"
non_ipa_vowels_non_accent_re = u"[^ ˈˌ" + ipa_vowel_list + "]"

# Other possibilities are special-cased, e.g. sn/zn/dn/tn, st/zd
cons_assim_palatal = {
  'compulsory':set([u'nt͡ɕ', u'nɕ', u'ntʲ', u'ndʲ', u'xkʲ',
    u't͡ssʲ', u'd͡zzʲ']),
  'optional':set([u'slʲ', u'zlʲ', u'nsʲ', u'nzʲ',
    u'mpʲ', u'mbʲ', u'mfʲ', u'fmʲ'])
}

# Original comment from Lua:
# [words which will be treated as accentless (i.e. their vowels will be
# reduced), and which will liaise with a preceding or following word;
# this will not happen if the words have an accent mark, cf.
# по́ небу vs. по не́бу, etc.]
# We use these lists to determine whether to auto-accent monosyllabic words.
accentless = {
  # class 'pre': particles that join with a following word
  'pre':set([u'без', u'близ', u'в', u'во', u'да', u'до',
    u'за', u'из', u'из-под', u'из-за', u'изо', u'к', u'ко', u'меж',
    u'на', u'над', u'надо', u'не', u'ни', u'об', u'обо', u'от', u'ото',
    u'перед', u'передо', u'по', u'под', u'подо', u'пред', u'предо', u'при', u'про',
    u'с', u'со', u'у', u'через']),
  # class 'prespace': particles that join with a following word, but only
  #   if a space (not a hyphen) separates them; hyphens are used here
  #   to spell out letters, e.g. а-эн-бэ́ for АНБ (NSA = National Security
  #   Agency) or о-а-э́ for ОАЭ (UAE = United Arab Emirates)
  'prespace':set([u'а', u'о']),
  # class 'post': particles that join with a preceding word
  'post':set([u'бы', u'б', u'ж', u'же', u'ли', u'либо', u'ль', u'ка',
    u'нибудь', u'тка']),
  # class 'posthyphen': particles that join with a preceding word, but only
  #   if a hyphen (not a space) separates them
  'posthyphen':set([u'то']),
}

fronting = {
  'a': u'æ',
  u'u': u'ʉ',
  u'ʊ': u'ʉ',
}

# Alt-ё templates for specifying terms spelled with е in place of ё.
alt_yo_templates = [u"ru-noun-alt-ё", u"ru-verb-alt-ё", u"ru-adj-alt-ё",
  u"ru-proper noun-alt-ё", u"ru-pos-alt-ё"]

skip_pages = [
    u"^б/у$",
    u"^в/о$",
    u"^г-ж",
    u"^г-н",
    u"^е$",
    u"^и$",
    # FIXME! Script wrongly removes the stressed form in the next three;
    # it should notice that the word is normally unstressed and not do this;
    # for the first one, should also notice that it's actually two words
    u"^м-да$",
    u"^на$",
    u"^не$",
    u"^промеж$", # has unstressed multisyllabic form
    u"^ы$",
    u"^я$"
]

# Pages where we allow unaccented multisyllabic words in the pronunciation.
# NOTE: This isn't actually necessary for бальза́м.* на́ душу because we
# rewrite it in manual_pronun_mapping to have ‿ in на́‿душу, which gets
# treated as a single word for unaccented-checking.
allow_unaccented = [
    u"^бальзам.* на душу"
]

applied_manual_pronun_mappings = set()

# Used when the automatic headword->pronun mapping fails for non-lemma forms
# (typically, where there's a secondary stress along with either multiword
# phrases or accent type c/d/e/f or accent type b with masculine nouns;
# basically, where there is a non-identity pronunciation mapping and the stem
# of the headword, including acute accents, isn't a possible beginning
# substring of the non-lemma form, and reverse-translit from the translit
# won't generate the right pronunciation [reverse-translit will correctly
# handle most cases where е is pronounced э]). Each tuple is of the form
# (HEADWORD, SUB) where HEADWORD is a regex and SUB is either a single string
# to substitute in the regex or a list of such strings. (In place of a single
# string can be a three-entry type of (SUB, TEXTBEFORE, TEXTAFTER) for
# cases where there is surrounding text such as {{i|romantic meeting}} or
# {{a|Moscow}}). The entries included are those that won't be handled right;
# for single words with secondary accents, these are typically for the
# non-lemma forms that don't have the same stress as the headword (e.g.
# (u"^авиакорпус", u"а̀виакорпус") handles plurals like а̀виакорпусы́,
# а̀виакорпусо́в vs. headword авиако́рпус.
#
# To find new cases to add, look for the message
# "WARNING: Would save and unable to match mapping" and check whether the
# pronunciation that is generated automatically is correct.
manual_pronun_mapping = [
    (u"^авиакорпус", u"а̀виакорпус"),
    (u"^авиаполк", [u"авиаполк", u"а̀виаполк"]),
    (u"^авиазве́н", u"а̀виазве́н"),
    (u"^авиаэскадри́лий", u"а̀виаэскадри́лий"),
    (u"^автозапра́вочн(.*?) ста́нц", ur"а̀втозапра́вочн\1 ста́нц"),
    (u"^автозапчаст", ur"а̀втозапчаст"),
    (u"^автоко́льц", [u"автоко́льц", u"а̀втоко́льц"]),
    (u"^автоколе́ц", [u"автоколе́ц", u"а̀втоколе́ц"]),
    (u"^аминокисло́т", u"амѝнокисло́т"),
    (u"^ампер-час", u"ампѐр-час"),
    (u"^антител", u"а̀нтител"),
    (u"^антивеще́ств", u"а̀нтивеще́ств"),
    (u"^анте́нн(.*?) радио(локаци)", [ur"phon=антэ́нн\1 ра̀дио\2", ur"phon=антэ́нн\1 ра̀дио̂\2"]),
    (u"^аэронавигацио́нн(.*?) ог", ur"а̀эронавигацио́нн\1 ог"),
    (u"^(бальза́м.*?) на́ душу", ur"\1 на́‿душу"),
    # For бензозаправочная колонка, бензозаправочная станция
    (u"^бензозапра́вочн", u"бѐнзозапра́вочн"),
    (u"^блокпост", u"бло̀кпост"),
    # override pronunciation бох/Бох
    (u"^([Бб]о́?г(а|у|ом|е|и|о́в|а́м|а́ми|а́х))$", ur"\1"),
    (u"^бронекатер", u"бро̀некатер"),
    (u"^бронестёк", u"бро̀нестёк"),
    # the following helps with бухгалтерская кинга, also forms like
    # бухгалтера́ of бухга́лтер, where the automatic headword->pronun mapping
    # fails and reverse-translit leaves out spelling pronun бухгалтера́
    (u"^бухг(а́?лтер)", [
      ur"phon=буг\1",
      ur"буɣ\1",
      ur"бухг\1"]),
    (u"^ветврач", u"вѐтврач"),
    (u"^видеои́гр", [u"вѝдеои́гр", u"вѝдео̂и́гр"]),
    (u"^вое́?нно-", u"воѐнно-"),
    (u"^вольноопределя́ющ", u"во̀льноопределя́ющ"),
    (u"^(вредоно́сн.*?) ПО$", ur"phon=\1 пэ-о́"),
    (u"^гендиректор", u"гѐндиректор"),
    (u"^гидромёт", [u"гидромёт", u"гѝдромёт"]),
    (u"город([ао])́(.*?)-геро́", ur"город\1̀\2-геро́"), # го́род-геро́й
    (u"^госвласт", u"го̀свласт"),
    (u"^госдепартамент", u"го̀сдепартамент"),
    (u"^го́спод", [u"го́спод", u"ɣо́спод"]),
    (u"^Го́спод", [u"Го́спод", u"ɣо́спод"]),
    (u"^госсекретар", u"го̀ссекретар"),
    (u"^(гражда́нск.*? войн.* в) США$", ur"\1 сэ-шэ-а́"),
    # Need to handle гэ́канье manually; has two pronunciations, one requiring
    # tr= and one not; FIXME, addpron.py doesn't create a pronunciation for
    # гэ́каний|tr=hɛ́kanij at all and lists both pronunciations for both cases
    # for the other non-lemma forms.
    (u"^дезоксирибонуклеи́нов(.*?) кисл", ur"phon=дэзоксирѝбонуклеи́нов\1 кисл"),
    (u"^детсад", [u"дѐтсад", u"детсад"]),
    # This adds доӂӂ- pronunciations to non-lemma forms (lemma has an
    # accent, до́ждь)
    (u"^дожд(я́|ю́|ём|е́|и́|е́й|я́м|я́ми|я́х)$", [ur"дожд\1", ur"доӂӂ\1"]),
    (u"^дождевик", [u"дождевик", u"доӂӂевик"]),
    (u"^дождев(.*? че́?рв)", [ur"дождев\1", ur"доӂӂев\1"]),
    (u"^дрожж", [u"дроӂӂ", u"дрожж"]),
    (u"^жа(ле́?)", ur"phon=же\1"),
    (u"^заво́д(.*?)-подря́дчик", ur"заво̀д\1-подря́дчик"),
    (u"^загранпаспорт", u"загра̀нпаспорт"),
    (u"^зернофураж", u"зѐрнофураж"),
    # reverse-translit would produce ёркширский тэрье́р etc.
    (u"^(йо́ркширск.*?) терье́р", ur"phon=\1 тэрье́р"),
    (u"^квартирохозя́", u"квартѝрохозя́"),
    (u"^киберво́йн", u"кѝберво́йн"),
    # This adds доӂӂ- pronunciations as for дождь above
    (u"^(кисло́тн.*? )дожд", [ur"\1дожд", ur"\1доӂӂ"]),
    (u"^кни́г(.*?) за семью́ печа́тям", ur"кни́г\1 за семью́ печа́тя̣м"),
    # NOTE: The following remains unapplied because the non-lemma forms
    # haven't been created yet
    (u"^коми-зыря́н", u"ко̀ми-зыря́н"),
    (u"^лесополо́с", u"лѐсополо́с"),
    (u"^лесостеп", u"лѐсостеп"),
    (u"^лимфодренаж", u"лѝмфодренаж"),
    (u"^льносем", u"льно̀сем"),
    (u"^лю́к(.*?) фотопулемёт", ur"лю́к\1 фо̀топулемёт"),
    (u"^мало(заме́т.*? бомбардиро́вщик)", [ur"ма̀ло\1"]),
    (u"^мало(россия́н)", [ur"ма̀ло\1", ur"мало\1"]),
    # genitive plural of межсезо́нье; FIXME, should be handled correctly
    (u"^межсезо́ний$", u"мѐжсезо́ний"),
    (u"^меж(ъягоди́чн.*? скла́д)", [ur"мѐж\1", ur"мѐш\1", ur"меж\1"]),
    (u"^микрово́лн", u"мѝкрово́лн"),
    (u"^микро(волно́в.*? пе́?ч)", ur"мѝкро\1"),
    (u"^мозжеч(ко́в.*? минда́лин)", [ur"мозжеч\1", ur"моӂӂеч\1"]),
    (u"^моче(испуска́тельн.*? кана́л)", ur"мо̀че\1"),
    (u"^мундштук", u"phon=мунштук"),
    (u"^(носов.*? )радио(прозра́чн.*? обтека́т.*? )анте́нны радиолокац",
      ur"phon=\1ра̀дио\2антэ́нны ра̀диолокац"),
    (u"^не́рвно-паралити́ческ(.*?) га́з", u"нѐрвно-паралити́ческ(.*?) га́з"),
    (u"^несча́стий$", u"неща́стий"),
    # нетопырь handled correctly without override
    (u"^ново(англича́н)", ur"но̀во\1"),
    (u"^обезьяно([лч])", ur"обезья̀но\1"),
    # паремия handled correctly without override
    (u"^(пере́дн.*?) бронеперегоро́д", ur"\1 бро̀неперегоро́д"),
    # forms of перейти́
    (u"^перейд", [u"перейд", u"phon=перед"]),
    (u"^пионерлагер", u"поинѐрлагер"),
    (u"^(пла́н.*?) Б$", ur"\1 бэ́"),
    #(u"^подна́йм", u"по̀дна́йм"),
    (u"^политкаторжа́н", u"полѝткаторжа́н"),
    (u"^(полубо́?г(а|у|ом|е|и|о́в|а́м|а́ми|а́х))$", ur"\1"),
    (u"^полу(в[её]д)", ur"по̀лу\1"),
    (u"^полу(ве́?к)", ur"по̀лу\1"),
    (u"^полу(го́?д)", ur"по̀лу\1"),
    (u"^полу(дю́жин)", ur"по̀лу\1"),
    (u"^полу(им)", ur"по̀лу\1"),
    (u"^полу(килом)", ur"по̀лу\1"),
    (u"^полу(ли́тр)", ur"по̀лу\1"),
    (u"^полу(лю́?д|челов)", ur"по̀лу\1"),
    (u"^полу(ме́сяц)", ur"по̀лу\1"),
    (u"^полу(ме́тр)", ur"по̀лу\1"),
    (u"^полу(миллио́н)", ur"по̀лу\1"),
    (u"^полу(ос)", ur"по̀лу\1"),
    (u"^полу(остров)", [ur"по̀лу\1", ur"полу\1"]),
    (u"^полу(очк)", ur"по̀лу\1"),
    (u"^полу(плоск)", [ur"по̀лу\1", ur"полу\1"]),
    (u"^полу(со́т)", ur"по̀лу\1"),
    (u"^полу(ты́сяч)", ur"по̀лу\1"),
    (u"^полу(ча́?с)", ur"по̀лу\1"),
    (u"^после(обе́денн.*? вре́?м)", ur"по̀сле\1"),
    (u"^пост(травмат.*?) стре́сс", ur"phon=по̀ст\1 стрэ́сс"),
    (u"^пра(воохрани́тельн.*? о́рган)", [ur"пра̀\1", ur"пра\1"]),
    (u"^пра(материк)", ur"пра̀\1"),
    (u"^пресс(-секретар)", ur"прѐсс\1"),
    (u"^(прибо́р.*? управле́ния) фото", ur"\1 фо̀то"),
    (u"^про́волок(.)", ur"про́вол(о)к\1"),
    (u"^прое́зж(.*? ча́?ст)", [ur"прое́зж\1", ur"прое́ӂӂ\1"]),
    (u"^противополо́ж", [u"противополо́ж", u"про̀тивополо́ж"]),
    # пуп земли handled correctly without override
    (u"^радиово́лн", [u"ра̀диово́лн", u"ра̀дио̂во́лн"]),
    (u"^радио(локацио́нн.*? дальноме́р)", [ur"ра̀дио\1", ur"ра̀дио̂\1"]),
    # forms of разъебать handled correctly without override
    (u"^раке́т(.*?)-носи́тел", ur"ракѐт\1-носи́тел"),
    # not correctly handled as form of расстава́ться
    (u"^расста([юё])", ur"^рас(с)та\1"),
    # forms of расчеса́ть
    (u"^расче́ш", [u"расче́ш", u"раще́ш"]),
    (u"^расчёс", [u"расчёс", u"ращёс"]),
    # раджа handled correctly without override
    (u"^рибонуклеи́нов(.*?) кисл", ur"рѝбонуклеи́нов\1 кисл"),
    (u"^романтик", (u"романтик", "{{i|romantic meeting}} ", "")),
    (u"^санузл", [u"санузл", u"са̀нузл", u"са̀нъузл"]),
    (u"^саундтрек", [u"phon=са̀ундтрэк", u"phon=са̂ундтрэк", u"phon=саундтрэк"]),
    (u"^сверх(лю́?д)", ur"свѐрх\1"), # from сверхчелове́к
    (u"^сверх(но́в.*? зв)", ur"свѐрх\1"),
    (u"^скучн([аы]́)$", [(ur"phon=скушн\1", "{{a|Moscow}} ", ""), (ur"скучн\1", "{{a|Saint Petersburg}} ", "")]),
    (u"^сёгу́н", [u"сё̂гу́н", (u"сегу́н", "", " {{i|uncommon or fast and casual speech}}")]),
    (u"^соцсет", u"со̀цсет"),
    # стольный град handled correctly without override
    (u"^суперзвёзд", u"су̀перзвёзд"), # суперзвезда́
    (u"^счётши", u"щчётши"), # form of счесть
    (u"^(су́?д.*? на подво́дных )кры́льях", ur"\1кры́лья̣х"),
    (u"^тео́ри(.*?) ха́оса", ur"тео́ри\1 ха́о̂са"),
    (u"^топмачтовик", u"то̀пмачтовик"),
    (u"^трёх(эта́жн.*? сло́?в)", ur"трё̀х\1"),
    (u"^турбо(реакти́вн.*? дви́гат)", ur"ту̀рбо\1"),
    (u"^(уто́пленн.*? )воз(духозабо́рник)", ur"\1во̀з\2"),
    # override pronunciation у́к(о)р, which should apply only to base form
    (u"^(у́кр(а|у|ом|е|ы|ов|ам|ами|ах))$", ur"\1"),
    (u"^ультракоротко(волно́в.*?) радио(ста́нц)",
        [ur"у̀льтракоротко\1 радио\2", ur"у̀льтракоро̀тко\1 радио\2"]),
    (u"^ум([ауоеы])́(.*?)-ра́зум", ur"ум\1̀\2-ра́зум"), # forms of у́м-ра́зум
    (u"^человеко(обра́зн.*? обезья́н)", ur"человѐко\1"),
    (u"^четырёх(та́кт.*? дви́гател)", ur"четырё̀х\1"),
    (u"^четверг", [u"четверг", u"phon=четверьг"]),
    # член-учредитель handled correctly without override
    (u"^шеф-повар", u"шѐф-повар"),
    (u"^(шпио́нск.*?) ПО$", ur"phon=\1 пэ-о́"),
    # щавель handled correctly without override
    (u"^электро(магни́тн.*?) взаимо(де́йств)", ur"элѐктро\1 взаѝмо\2"),
    # for various expressions like гравитационное взаимодействие; this
    # must follow the previous entry for электромагнитное взаимодействие,
    # so that entry takes precendence
    (u" взаимоде́йств", u" взаѝмоде́йств"),
    (u"^электро(поезд)", ur"элѐктро\1"),
    # numerals
    (u"^((?:.*? )?)пятьдеся́т((?: .*?)?)$", [ur"\1пятьдеся́т\2|gem=opt", (ur"phon=\1пееся́т\2", "{{i|colloquial or fast speech}} ", "")]),
    (u"^((?:.*? )?)шестьдеся́т", [ur"\1шестьдеся́т", (ur"phon=\1шееся́т", "{{i|colloquial or fast speech}} ", "")]),
    (u"^((?:.*? )?)се́мьдесят", [ur"\1се́мьдесят", ur"phon=\1се́мдесят"]),
    (u"^((?:.*? )?)во́семьдесят", [ur"\1во́семьдесят", ur"phon=\1во́семдесят"]),
]

semicolon_tags = [';', ';<!--\n-->']

# FIXME: Remove this and use infltags.py once we move to ../RuNounChanges.
# Split tags into tag sets.
def split_tags_into_tag_sets(tags):
  tag_set_group = []
  cur_tag_set = []
  for tag in tags:
    if tag in semicolon_tags:
      if cur_tag_set:
        tag_set_group.append(cur_tag_set)
      cur_tag_set = []
    else:
      cur_tag_set.append(tag)
  if cur_tag_set:
    tag_set_group.append(cur_tag_set)
  return tag_set_group


# Make sure there are two trailing newlines
def ensure_two_trailing_nl(text):
  return re.sub(r"\n*$", r"\n\n", text)

def contains_latin(text):
  return re.search(u"[0-9a-zščžáéíóúýàèìòùỳɛě]", text.lower())

def contains_non_cyrillic_non_latin(text):
  # 0300 = grave, 0301 = acute, 0302 = circumflex, 0308 = diaeresis,
  # 0307 = dot-above, 0323 = dot-below
  # We also include basic punctuation as well as IPA chars ɣ ɕ ʑ, which
  # we allow in Cyrillic pronunciation; we also allow ()_/‿, which have
  # significance as phonological characters. FIXME: We allow Latin h as a
  # substitute for ɣ, we should allow it here and not have it trigger
  # contains_latin() by itself.
  return re.sub(ur"[\u0300\u0301\u0302\u0308\u0307\u0323 \-,.?!()_/‿ɣɕʑЀ-ԧꚀ-ꚗa-zščžáéíóúýàèìòùỳɛě]", "", text.lower()) != ""

def ipa_matches(headword, manual, auto, ipa_templates_msg, pagemsg):
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
  # Get rid of raising/lowering diacritics
  manual = re.sub(u"[\u031d\u031e]", u"", manual)
  manual = re.sub(u"ʲɛ", u"ʲe", manual)
  manual = re.sub(u"[ɘɞ]", u"ə", manual)
  # expand long vowels; need to do this before moving stress to beginning
  # of consonant clusters, below, as ː is treated as a consonant
  manual = re.sub(u"[ɐə]ː", u"ɐɐ", manual)
  manual = re.sub(u"ɪː", u"ɪɪ", manual)
  manual = re.sub(u"ɑ", "a", manual)
  manual = re.sub(u"ɔ", "o", manual)
  manual = re.sub(u"ɾ", "r", manual)
  manual = re.sub(u"χ", "x", manual)
  manual = re.sub(ur"\(ʲ\)", u"⁽ʲ⁾", manual)
  manual = re.sub(u"ʌ", u"ɐ", manual)
  manual = re.sub(u"'", u"ˈ", manual)
  manual = re.sub(u"ˈˈ", u"ˈ", manual)
  manual = re.sub(u"ɫ", "l", manual)
  manual = re.sub(ur"ʈʂ", u"t͡ʂʂ", manual)
  manual = re.sub(u"t͡ʃ|tʃ", u"t͡ɕ", manual)
  manual = re.sub(u"ʃ", u"ʂ", manual)
  # Convert regular g to IPA ɡ (looks same but different char)
  manual = re.sub("g", u"ɡ", manual)
  # Both ɡ's below are IPA ɡ's
  manual = re.sub(u"ŋɡ", u"nɡ", manual)
  manual = re.sub(u"st([ln])", r"s\1", manual)
  # Canonicalize spaces and hyphens in manual
  manual = re.sub(r"^[\s\-]+", "", manual)
  manual = re.sub(r"[\s\-]+$", "", manual)
  manual = re.sub(r"[\s\-]+", " ", manual)

  autowords = re.split(" ", auto)
  manwords = re.split(" ", manual)
  hwords = re.split(r"[\s\-]", headword)
  if len(autowords) != len(manwords):
    return "WARNING: For headword %s, auto %s%s not same as manual %s%s: different number of words (auto %s vs manual %s): %s" % (
      headword, auto, orig_auto != auto and " (%s)" % orig_auto or "",
      manual, orig_manual != manual and " (%s)" % orig_manual or "",
      len(autowords), len(manwords), ipa_templates_msg)
  if len(hwords) != len(autowords):
    pagemsg("WARNING: Number of words in headword %s not same as in auto %s" % (
      headword, auto))
    hwords = [""]*len(autowords)
  for j in xrange(len(autowords)):
    autoword = autowords[j]
    manword = manwords[j]
    hword = hwords[j]
    auto_monosyllabic = len(re.sub(non_ipa_vowels_re, "", autoword)) == 1
    man_monosyllabic = len(re.sub(non_ipa_vowels_re, "", manword)) == 1

    # If both auto and manual are monosyllabic, canonicalize by
    # removing primary accent
    if auto_monosyllabic and man_monosyllabic:
      autoword = re.sub(u"ˈ", "", autoword)
      manword = re.sub(u"ˈ", "", manword)

    # Convert some instances of ˈ (earlier converted from to ') to ʲ --
    # after a palatalizable consonant, before a vowel or end of word;
    # ɡ is IPA ɡ; do this before moving stress to beginning of cons clusters
    manword = re.sub(ur"([dtbpkɡszfvxrlmn])ˈ(ː?)($|" + ipa_vowels_re + ")",
        ur"\1ʲ\2\3", manword)

    # Canonicalize by moving stress at the beginning of all consonant
    # clusters
    autoword = re.sub("(" + non_ipa_vowels_re + u"+)([ˈˌ])", r"\2\1", autoword)
    manword = re.sub("(" + non_ipa_vowels_re + u"+)([ˈˌ])", r"\2\1", manword)

    # т(ь)ся and related fixes
    manual = re.sub(u"nt͡sk", u"n(t)sk", manual)
    manual = re.sub(u"ntsk", u"n(t)sk", manual)
    manual = re.sub(u"tts", u"t͡sː", manual)
    manword = re.sub(u"tt͡s", u"t͡sː", manword)
    manword = re.sub(u"tːs", u"t͡sː", manword)
    manword = re.sub(u"tʲ?t͡ɕ", u"t͡ɕː", manword)
    manword = re.sub(u"tːɕ", u"t͡ɕː", manword)
    # with -т(ь)ся after stressed syllable, need "t͡sː"; after unstressed,
    # need just t͡s
    if re.search(u"\u0301ть?ся$", hword):
      manword = re.sub(u"(" + ipa_vowels_re + u")(ts|t͡s)ə$", ur"\1t͡sːə", manword)
    elif re.search(u"ть?ся$", hword):
      manword = re.sub(u"(" + ipa_vowels_re + u")(ts|t͡s)ːə$", ur"\1t͡sə", manword)

    # ɐ vs. ə fixes; ɐ at beginning of word or directly before the stress
    # or in ɐɐ sequences, else ə
    manword = re.sub(u"ɐ", u"ə", manword)
    manword = re.sub(u"^ə", u"ɐ", manword)
    manword = re.sub(u"[ɐə][ɐə]", u"ɐɐ", manword)
    # need to do this after moving stress to beginning of consonant clusters
    manword = re.sub(u"əˈ", u"ɐˈ", manword)

    # Fix bug in auto (FIXME, remove this)
    autoword = re.sub(u"ɕ(ː?)ʲə", ur"ɕ\1ə", autoword)

    # palatalization and gemination need to be in the right order
    # Fix bug in auto
    autoword = re.sub(u"ːʲ", u"ʲː", autoword)
    manword = re.sub(u"ːʲ", u"ʲː", manword)

    # front vowel variants; here we first convert existing front variants
    # to back vowels, then eventually we apply the exact same fronting
    # logic as in ru-pron.lua.
    manword = re.sub(u"æ", u"a", manword)
    # u will get converted below to ʊ in unstressed syllables
    manword = re.sub(u"ʉ", u"u", manword)
    # Much simpler for o
    manword = re.sub(u"([ʲjɕ]ː?)o", ur"\1ɵ", manword)

    # i vs. ɪ fixes: i when stressed, ɪ otherwise; same for u vs. ʊ
    if not man_monosyllabic and re.search(u"ˈ", manword):
      # Convert all i to ɪ, then back to i in stressed syllables
      manword = re.sub(u"i", u"ɪ", manword)
      manword = re.sub(u"([ˈˌ]" + non_ipa_vowels_re + u"*)ɪ", r"\1i", manword)
      # Convert all u to ʊ, then back to u in stressed syllables
      manword = re.sub(u"u", u"ʊ", manword)
      manword = re.sub(u"([ˈˌ]" + non_ipa_vowels_re + u"*)ʊ", r"\1u", manword)
    # If monosyllabic, i and u unless word is accentless
    if man_monosyllabic and headword not in (
        [u"без", u"близ", u"из", u"меж", u"пред", u"при", u"не", u"ли"]):
      manword = re.sub(u"ɪ", u"i", manword)
    if man_monosyllabic and headword != u"у":
      manword = re.sub(u"ʊ", u"u", manword)

    # ɕ that's not geminate and not in t͡ɕ (with or without tie bar)
    # needs to be geminated (0361 = tie bar)
    manword = re.sub(u"([^t\u0361])ɕ($|[^ː])", ur"\1ɕː\2", manword)

    # Convert nː in n(ː) in various endings in n(ː) in auto
    if re.search(u"n\(ː\)", autoword):
      manword = re.sub(u"nː(ɨj|əsʲtʲ|əjə|ə)$", ur"n(ː)\1", manword)

    # Convert sʲə to s⁽ʲ⁾ə at end of word if necessary
    if re.search(u"s⁽ʲ⁾ə$", autoword):
      manword = re.sub(u"sʲə$", u"s⁽ʲ⁾ə", manword)

    # if affricate assimilation in autoword, make it same in manword
    # do before adding tie bar in ts
    if re.search(u"d͡zz", autoword):
      manword = re.sub(u"dz", u"d͡zz", manword)
    if re.search(u"t͡ss", autoword):
      manword = re.sub(u"ts", u"t͡ss", manword)

    # add tie bar if needed
    if re.search(u"t͡s", autoword):
      manword = re.sub(u"ts", u"t͡s", manword)
    if re.search(u"t͡ɕ", autoword):
      manword = re.sub(u"tɕ", u"t͡ɕ", manword)

    # palatalization needed before front vowels; note, ɡ is IPA ɡ
    # we don't do this before e because the lack of palatalization might
    # be legitimate (although should be written with ɛ)
    manword = re.sub(ur"([dtbpkɡszfvxrlmn])(ː?[ɪiɵæʉ])", ur"\1ʲ\2", manword)

    # Apply optional and compulsory palatal assimilation to manword
    def apply_tn_dn_assim_palatal(m):
      a, b, c = m.groups()
      if a == '':
        return a + b + u'ʲ' + c
      else:
        return a + b + u'⁽ʲ⁾' + c

    # Optional (j) before ɪ
    manword = re.sub(u"(^| )jɪ", ur"\1(j)ɪ", manword)
    manword = re.sub(ipa_vowels_c + u"([‿-]?)jɪ", ur"\1\2(j)ɪ", manword)

    # consonant assimilative palatalisation of tn/dn/sn/zn, depending on
    # whether [rl] precedes
    manword = re.sub(u"([rl]?)([ˈˌ]?[dtsz])ʲ?([ˈˌ]?nʲ)",
        apply_tn_dn_assim_palatal, manword)

    # consonant assimilative palatalisation of st/zd, depending on
    # whether [rl] precedes
    manword = re.sub(u"([rl]?)([ˈˌ]?[sz])ʲ?([ˈˌ]?[td]ʲ)",
        apply_tn_dn_assim_palatal, manword)

    def apply_assim_palatal(m):
      a, b, c = m.groups()
      if a + c in cons_assim_palatal['compulsory']:
        return a + u'ʲ' + b + c
      elif a + c in cons_assim_palatal['optional']:
        return a + u'⁽ʲ⁾' + b + c
      else:
        return m.group(0)

    #apply general consonant assimilative palatalisation, repeatedly for
    #recursive assimilation
    while True:
      new_manword = re.sub(u'(t͡s|d͡z|[xszntdmbpf])ʲ?([ˈˌ]?)(t͡ɕ|[ktdǰɕlnszmpbf]ʲ)',
        apply_assim_palatal, manword)
      if new_manword == manword:
        break
      manword = new_manword

    # optional palatal assimilation of вп, вб only word-initially
    manword = re.sub(u'^([ˈˌ]?[fv])ʲ?([ˈˌ]?[pb]ʲ)', ur'\1⁽ʲ⁾\2', manword)

    # END OF PER-WORD PROCESSING
    autowords[j] = autoword
    manwords[j] = manword

  auto = " ".join(autowords)
  manual = " ".join(manwords)

  # Following code is copied from ru-pron.lua (and converted to Python).
  #
  # Front a and u between soft consonants. If between a soft and
  # optionally soft consonant (should only occur in that order, shouldn't
  # ever have a or u preceded by optionally soft consonant),
  # split the result into two. We only split into two even if there
  # happen to be multiple optionally fronted a's and u's to avoid
  # excessive numbers of possibilities (and it simplifies the code).
  # 1. First, temporarily add soft symbol to inherently soft consonants.
  manual = re.sub(u"([čǰɕӂj])", ur"\1ʲ", manual)
  # 2. Handle case of [au] between two soft consonants
  manual = re.sub(u"(ʲ[ː()]*)([auʊ])([ˈˌ]?.ʲ)",
      lambda m:m.group(1) + fronting[m.group(2)] + m.group(3),
      manual)
  # 3. Handle [au] between soft consonant and optional j, which is still fronted
  manual = re.sub(ur"(ʲ[ː()]*)([auʊ])([ˈˌ]?\(jʲ\))",
      lambda m:m.group(1) + fronting[m.group(2)] + m.group(3),
      manual)
  # 4. Handle case of [au] between soft and optionally soft consonant
  if re.search(u"ʲ[ː()]*[auʊ][ˈˌ]?.⁽ʲ⁾", manual) or re.search(ur"ʲ[ː()]*[auʊ][ˈˌ]?\(jʲ\)", manual):
    opt_hard = re.sub(u"(ʲ[ː()]*)([auʊ])([ˈˌ]?.)⁽ʲ⁾", r"\1\2\3", manual)
    opt_soft = re.sub(u"(ʲ[ː()]*)([auʊ])([ˈˌ]?.)⁽ʲ⁾",
      lambda m:m.group(1) + fronting[m.group(2)] + m.group(3) + u"ʲ",
      manual)
    manual = opt_hard + ", " + opt_soft
  # 5. Undo addition of soft symbol to inherently soft consonants.
  manual = re.sub(u"([čǰɕӂj])ʲ", r"\1", manual)

  if auto == manual:
    pagemsg("For headword %s, %s equal to %s" % (headword,
      "auto %s" % auto if auto == orig_auto else
        "canon auto %s (orig %s)" % (auto, orig_auto),
      "manual" if manual == orig_manual else
        "canon manual (orig %s)" % (orig_manual)))
    return True

  if u"ˈ" not in manual and re.sub(u"ˈ", "", auto) == manual:
    pagemsg("WARNING: For headword %s, missing stress mark in %s compared to %s, accepting" % (headword,
      "manual" if manual == orig_manual else
        "canon manual (orig %s)" % (orig_manual),
      "auto %s" % auto if auto == orig_auto else
        "canon auto %s (orig %s)" % (auto, orig_auto)))
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
  return "WARNING: For headword %s, auto %s%s not same as manual %s%s: %s: %s" % (
    headword, auto, orig_auto != auto and " (%s)" % orig_auto or "",
    manual, orig_manual != manual and " (%s)" % orig_manual or "",
    ", ".join(changes), ipa_templates_msg)

def canonicalize_monosyllabic_pronun(pronun, tr):
  # Do nothing if there are multiple words
  if pronun not in accentless['pre'] and not re.search(r"[\s\-]", pronun):
    return ru.try_to_stress(pronun), ru.try_to_stress(tr)
  else:
    return pronun, tr

def remove_list_duplicates(l):
  newl = []
  for x in l:
    if x not in newl:
      newl.append(x)
  return newl

def printable_ru_tr(cyr, tr):
  if tr:
    return "%s//%s" % (cyr, tr)
  else:
    return cyr

def printable_ru_tr_list(values):
  return ",".join(printable_ru_tr(cyr, tr) for cyr, tr in values)

def ru_tr_as_pronun(cyr, tr):
  if tr:
    return "phon=%s" % ru_reverse_translit.reverse_translit(tr, cyr)
  else:
    return cyr

def get_first_param(t):
  lang = getparam(t, "lang")
  if lang:
    if lang == "ru":
      return "1"
    else:
      return None
  else:
    if getparam(t, "1") == "ru":
      return "2"
    else:
      return None

# Get a list of headword pronuns, a list of (HEAD, TRANSLIT) tuples.
def get_headword_pronuns(parsed, pagetitle, pagemsg, expand_text):
  # Get the headword pronunciation(s)
  headword_pronuns = []

  # Append headword to headword_pronuns, possibly with translit.
  # If translit is present, split on commas to handle cases like
  # {{ru-noun form|а́бвера|tr=ábvera, ábvɛra|m-in}}. Don't split regular
  # headwords on commas because sometimes they legitimately have commas
  # in them (idioms, phrases, etc.). When we have translit, check each
  # value against the headword to see if the translit is redundant
  # (e.g. as in {{ru-noun form|а́бвера|tr=ábvera, ábvɛra|m-in}}, where the
  # first is redundant).
  def append_headword(head, tr, trparam):
    if not tr:
      headword_pronuns.append((head, ""))
    else:
      pagemsg("WARNING: Using Latin for pronunciation, based on %s%s" %
          (trparam, tr))
      tr = ru.decompose(tr)
      # Split on commas, as described above; but don't do it if there's
      # a comma in the headword, e.g. in 'из-за того, что' (translit no
      # longer needed but formerly present).
      if "," in head:
        translits = [tr]
      else:
        translits = re.split(r"\s*,\s*", tr)
      for trval in translits:
        autotranslit = expand_text("{{xlit|ru|%s}}" % head)
        # Just in case, normalize both when comparing; not generally
        # necessary because we called ru.decompose() above
        if unicodedata.normalize("NFC", autotranslit) == unicodedata.normalize("NFC", trval):
          pagemsg("Ignoring redundant translit %s%s for headword %s" %
              (trparam, trval, head))
          headword_pronuns.append((head, ""))
        else:
          headword_pronuns.append((head, trval))

  for t in parsed.filter_templates():
    check_extra_heads = False
    tname = unicode(t.name)
    if tname in alt_yo_templates or tname == u"ru-alt-ё":
      pagemsg("WARNING: Found %s template, skipping" % tname)
      return None
    elif tname in ["ru-noun", "ru-proper noun", "ru-adj", "ru-adv", "ru-verb"]:
      head = getparam(t, "1") or pagetitle
      tr = getparam(t, "tr")
      append_headword(head, tr, "tr=")
      check_extra_heads = True
    elif tname in ["ru-noun form", "ru-phrase"]:
      head = getparam(t, "head") or getparam(t, "1") or pagetitle
      tr = getparam(t, "tr")
      append_headword(head, tr, "tr=")
      check_extra_heads = True
    elif tname == "head" and getparam(t, "1") == "ru" and getparam(t, "2") == "letter":
      pagemsg("WARNING: Skipping page with letter headword")
      return None
    elif tname == "head" and getparam(t, "1") == "ru":
      head = getparam(t, "head") or pagetitle
      tr = getparam(t, "tr")
      append_headword(head, tr, "tr=")
      check_extra_heads = True
    elif tname in ["ru-noun+", "ru-proper noun+"]:
      if tname == "ru-noun+":
        generate_template = re.sub(r"^\{\{ru-noun\+",
            "{{ru-generate-noun-forms", unicode(t))
      else:
        generate_template = re.sub(r"^\{\{ru-proper noun\+",
            "{{ru-generate-noun-forms|ndef=sg", unicode(t))
      generate_result = expand_text(generate_template)
      if not generate_result:
        pagemsg("WARNING: Error generating noun forms")
        return None
      args = {}
      for arg in re.split(r"\|", generate_result):
        name, value = re.split("=", arg)
        args[name] = re.sub("<!>", "|", value)
      lemma = args["nom_sg"] if "nom_sg" in args else args["nom_pl"]
      for head in re.split(",", lemma):
        tr = None
        if "//" in head:
          head, tr = re.split("//", head)
        append_headword(head, tr, "translit ")

    if check_extra_heads:
      for i in xrange(2, 10):
        headn = getparam(t, "head" + str(i))
        trn = getparam(t, "tr" + str(i))
        if headn:
          append_headword(headn, trn, "tr%s=" % str(i))

  # Do the following two sections before adding semi-reduced inflection
  # since ru.* may not be aware of dot-under.

  # Canonicalize by removing links and final !, ?
  headword_pronuns = [(re.sub("[!?]$", "", blib.remove_links(x)), re.sub("[!?]$", "", blib.remove_links(tr))) for x, tr in headword_pronuns]
  for pronun, translit in headword_pronuns:
    if ru.remove_accents(pronun) != pagetitle:
      pagemsg("WARNING: Headword pronun %s doesn't match page title, skipping" % pronun)
      return None

  # Check for acronym/non-syllabic.
  for pronun, translit in headword_pronuns:
    if ru.is_nonsyllabic(pronun):
      pagemsg("WARNING: Pronunciation is non-syllabic, skipping: %s" % pronun)
      return None
    if re.search("[" + ru.uppercase + u"ЀЍ][" + ru.AC + ru.GR + "]?[" + ru.uppercase + u"ЀЍ]", pronun):
      pagemsg("WARNING: Pronunciation may be an acronym, please check: %s" % pronun)

  # Check for the need for semi-reduced inflection of я in 3rd plural -ят
  # or dat/ins/pre plural -ям, -ями, -ях.
  found_semireduced_inflection = False
  for t in parsed.filter_templates():
    tname = unicode(t.name)
    if tname in ["inflection of", "infl of"]:
      first_param = get_first_param(t)
      if first_param:
        tags = []
        for param in t.params:
          pname = unicode(param.name)
          if re.search(r"^[0-9]+$", pname):
            pnum = int(pname)
            if pnum >= int(first_param) + 2:
              tags.append(unicode(param.value))
        tag_sets = split_tags_into_tag_sets(tags)
        for tag_set in tag_sets:
          def has(infl):
            return any(not not re.search(r"\b%s\b" % infl, x) for x in tag_set)
          if (has("3") or has("13") or has("23") or has("123")) and (
              has("p") or has("pl") or has("plural")):
            found_semireduced_inflection = True
            break
          elif ((has("p") or has("pl") or has("plural")) and (
            has("dat") or has("dative") or
            has("ins") or has("instrumental") or
            has("pre") or has("prep") or has("prepositional"))):
            found_semireduced_inflection = True
            break
        if found_semireduced_inflection:
          break
  if found_semireduced_inflection:
    def update_semireduced(pron, tr):
      if tr:
        tr = re.sub("([" + ru.translit_vowel + "][^" + ru.translit_vowel + " -]" + u"*(?:ja|ča))(t|tsja|m|mi|x)( |$)", r"\1" + DOTBELOW + r"\2\3", tr)
      pron = re.sub("([" + ru.vowel + "][^" + ru.vowel + " -]" + u"*(?:я|[щч]а))(т|тся|м|ми|х)( |$)", r"\1" + DOTBELOW + r"\2\3", pron)
      return pron, tr
    new_headword_pronuns = [update_semireduced(pron, tr) for pron, tr in headword_pronuns]
    if new_headword_pronuns != headword_pronuns:
      pagemsg("Using semi-reduced pronunciation: %s" % printable_ru_tr_list(new_headword_pronuns))
      headword_pronuns = new_headword_pronuns

  # Canonicalize headword pronuns. If a single monosyllabic word, add accent
  # unless it's in the list of unaccented words.
  headword_pronuns = [canonicalize_monosyllabic_pronun(x, tr) for x, tr in headword_pronuns]

  # Also, if two pronuns differ only in that one has an additional accent on a
  # word, remove the one without the accent.

  def headwords_same_but_first_maybe_lacks_accents(h1, h2):
    if ru.remove_accents(h1) == ru.remove_accents(h2) and len(h1) < len(h2):
      h1words = re.split(r"([\s\-]+)", h1)
      h2words = re.split(r"([\s\-]+)", h2)
      if len(h1words) == len(h2words):
        for i in xrange(len(h1words)):
          if not (h1words[i] == h2words[i] or ru.is_unaccented(h1words[i]) and ru.remove_accents(h2words[i]) == h1words[i]):
            return False
      return True
    return False
  def headword_should_be_removed_due_to_unaccent(hword, hwords):
    hwordru, hwordtr = hword
    for h in hwords:
      if hword != h:
        hru, htr = h
        if (headwords_same_but_first_maybe_lacks_accents(hwordru, hru) and
            headwords_same_but_first_maybe_lacks_accents(hwordtr, htr)):
          pagemsg("Removing headword %s because same as headword %s but lacking an accent" % (
            printable_ru_tr(hwordru, hwordtr), printable_ru_tr(hru, htr)))
          return True
    return False
  headword_pronuns = remove_list_duplicates(headword_pronuns)
  new_headword_pronuns = [x for x in headword_pronuns if not
      headword_should_be_removed_due_to_unaccent(x, headword_pronuns)]
  if len(new_headword_pronuns) <= len(headword_pronuns) - 2:
    pagemsg("WARNING: Removed two or more headword pronuns, check that something didn't go wrong: old=%s, new=%s" % (
      printable_ru_tr_list(headword_pronuns), printable_ru_tr_list(new_headword_pronuns)))
  headword_pronuns = new_headword_pronuns

  if len(headword_pronuns) < 1:
    pagemsg("WARNING: Can't find headword template")
    return None
  headword_pronuns = remove_list_duplicates(headword_pronuns)
  return headword_pronuns

def pronun_matches(hpron, foundpron, pagemsg):
  orighpron = hpron
  origfoundpron = foundpron
  foundpron = re.sub("^phon=", "", foundpron)
  if hpron == foundpron or not foundpron:
    return True
  foundpron = ru.remove_grave_accents(foundpron.replace(CFLEX, ""))
  if hpron == foundpron:
    pagemsg("Matching headword pronun %s to found pronun %s after removing circumflex and grave accents from the latter" %
      (orighpron, origfoundpron))
    return True
  foundpron = foundpron.replace(u"э", u"е")
  hpron = hpron.replace(u"э", u"е")
  if hpron == foundpron:
    pagemsg(u"Matching headword pronun %s to found pronun %s after converting э to е (and removing circumflex and grave accents)" %
      (orighpron, origfoundpron))
    return True
  foundpron = foundpron.lower()
  hpron = hpron.lower()
  if hpron == foundpron:
    pagemsg(u"Matching headword pronun %s to found pronun %s after lowercasing (and converting э to е and removing circumflex and grave accents)" %
      (orighpron, origfoundpron))
    return True
  hpron = hpron.replace(u"зж", u"ӂӂ")
  hpron = hpron.replace(u"жж", u"ӂӂ")
  if hpron == foundpron:
    pagemsg(u"Matching headword pronun %s to found pronun %s after converting зж and жж to ӂӂ (and lowercasing, converting э to е and removing circumflex and grave accents)" %
      (orighpron, origfoundpron))
    return True

  return False

# Simple class to hold pronunciation found in ru-IPA, along with the text
# before and after. Lots of boilerplate to support equality and hashing.
# Based on http://stackoverflow.com/questions/390250/elegant-ways-to-support-equivalence-equality-in-python-classes
class FoundPronun(object):
  """Very basic"""
  def __init__(self, pron, pre, post):
    self.pron = pron
    self.pre = pre
    self.post = post

  def __eq__(self, other):
    """Override the default Equals behavior"""
    if isinstance(other, self.__class__):
      return self.pron == other.pron and self.pre == other.pre and self.post == other.post
    return NotImplemented

  def __ne__(self, other):
    """Define a non-equality test"""
    if isinstance(other, self.__class__):
      return not self.__eq__(other)
    return NotImplemented

  def __hash__(self):
    """Override the default hash behavior (that returns the id or the object)"""
    return hash(tuple(self.pron, self.pre, self.post))

  def __repr__(self):
    return "%s%s%s" % (self.pre and "[%s]" % self.pre or "", self.pron,
        self.post and "[%s]" % self.post or "")

# Match up the stems of headword pronunciations and found pronunciations.
# On entry, HEADWORD_PRONUNS is a list of (CYRILLIC, TR) values extracted
# from headwords; FOUND_PRONUNS is a list of FoundPronun objects, each one
# listing a pronunciation from {{ru-IPA}} along with the text before and after
# the pronunciation on the same line, minus any '* ' at the beginning. Note
# that the pronunciation here will be of the form 'phon=PRON' and taken from
# param phon= of {{ru-IPA}} if it exists, else of the bare form 'PRON' and
# taken from param 1= (which may be empty).
#
# If able to do so, return a dictionary of all non-identity matchings, else
# return None. For each headword in the dictionary, the entry is a list of
# tuples of (STEM, FOUNDPRONSTEMS) where STEM is a possible stem of that
# headword and FOUNDPRONSTEMS is a list of the corresponding
# found-pronunciation stems. (Each such stem is actually a FoundPronun object,
# with the pre-text and post-text coming from the corresponding text before
# and after the {{ru-IPA}} template where the pronunciation was found.)
# We return a list of stem tuples because there may be multiple stems to
# consider for each headword -- including reduced and dereduced variants
# (e.g. for автозапра́вка with corresponding pronunciation а̀втозапра́вка we
# need to consider both the regular stem автозапарвк- with stemmed
# pronunciation а̀втозапра́вк- and dereduced stem автозапра́вок- with stemmed
# pronunciation а̀втозапра́вок- in order to handle genitive plural автозаправок,
# or for амстерда́мец with pronun амстэрда́мец most forms require reduced
# амстерда́мц -> амстэрда́мц) and adjectival variants (e.g. for
# несовершенноле́тний with pronunciation несовершѐнноле́тний we need
# to consider the stem несовершенноле́тн-). FOUNDPRONSTEMS is a list because
# there may be multiple such pronunciations per headword stem, e.g. а́бвер
# has two corresponding pronunciations а́бвер and phon=а́бвэр.
def match_headword_and_found_pronuns(headword_pronuns, found_pronuns, pagemsg,
    expand_text):
  matches = {}
  if not headword_pronuns:
    pagemsg("WARNING: No headword pronuns, possible error")
    # Error finding headword pronunciations, or something
    return None
  if not found_pronuns:
    pagemsg("WARNING: No found pronuns")
    return None
  # How many headword pronuns? If only one, automatically assign all found
  # pronuns to it.
  distinct_hprons = set(hpron for hpron, tr in headword_pronuns)
  if len(distinct_hprons) == 1:
    hpron = list(distinct_hprons)[0]
    for foundpron in found_pronuns:
      valtoadd = FoundPronun(foundpron.pron or hpron, foundpron.pre, foundpron.post)
      if hpron in matches:
        if valtoadd not in matches[hpron]:
          matches[hpron].append(valtoadd)
      else:
        matches[hpron] = [valtoadd]

  else:
    # Multiple headwords, need to match "the hard way"
    all_match = True
    unmatched_hpron = set()
    hpron_seen = set()
    for hpron, tr in headword_pronuns:
      if hpron in hpron_seen:
        pagemsg("Skipping already-seen headword pronun %s%s" % (
          hpron, tr and "//" + tr))
        continue
      hpron_seen.add(hpron)
      new_found_pronuns = []
      matched = False
      for foundpron in found_pronuns:
        if pronun_matches(hpron, foundpron.pron, pagemsg):
          if tr and not "phon=" in foundpron.pron:
            pagemsg("WARNING: Found translit %s for headword %s, but matched against ru-IPA pronun %s lacking phon=" % (
              tr, hpron, foundpron.pron))
          valtoadd = FoundPronun(foundpron.pron or hpron, foundpron.pre, foundpron.post)
          if hpron in matches:
            if valtoadd not in matches[hpron]:
              matches[hpron].append(valtoadd)
          else:
            matches[hpron] = [valtoadd]
          matched = True
        else:
          new_found_pronuns.append(foundpron)
      found_prons = new_found_pronuns
      if not matched:
        all_match = False
        unmatched_hpron.add(hpron)
    if not all_match:
      pagemsg("WARNING: Unable to match headword pronuns %s against found pronuns %s" %
          (",".join(unmatched_hpron), ",".join(unicode(x) for x in found_pronuns)))
      return None

  def get_reduced_stem(nom):
    # The stem for reduce_stem() should preserve -й
    stem_for_reduce = re.sub(u"[аяеоьыи]́?$", "", nom)
    epenthetic_vowel = nom.endswith(AC)
    if re.search(u"[аяеоыи]́?$", nom):
      reduced_stem = expand_text("{{#invoke:ru-common|dereduce_stem|%s||%s}}" %
        (stem_for_reduce, "y" if epenthetic_vowel else ""))
    else:
      reduced_stem = expand_text("{{#invoke:ru-common|reduce_stem|%s}}" %
          stem_for_reduce)
    return reduced_stem

  def get_dereduced_adj_stem(stem, epvowel):
    return expand_text("{{#invoke:ru-common|dereduce_stem|%s||%s}}" %
        (stem, "y" if epvowel else ""))

  def get_iotated_stem(stem, shch):
    return expand_text("{{#invoke:ru-common|iotation|%s||%s}}" %
        (stem, u"щ" if shch else ""))

  # Apply a function to a list of found pronunciations. If a pronunciation
  # begins with phon=, strip it off before applying the function and then
  # add it back. Don't include results where the return value from the
  # function is logically false.
  def frob_foundprons(foundprons, fun):
    retval = []
    for foundpron in foundprons:
      newval = None
      if foundpron.pron.startswith("phon="):
        funval = fun(re.sub("^phon=", "", foundpron.pron))
        if funval:
          newval = "phon=" + funval
      else:
        funval = fun(foundpron.pron)
        if funval:
          newval = funval
      if newval:
        retval.append(FoundPronun(newval, foundpron.pre, foundpron.post))
    return retval

  # Remove the common case where there's only one found pronunciation and it's
  # the same as the head pronunciation (and there's no pre-text or post-text
  # that we need to propagate).
  matches = dict((hpron,foundprons) for hpron,foundprons in matches.iteritems()
      if not (len(foundprons) == 1 and hpron == foundprons[0].pron and
        not foundprons[0].pre and not foundprons[0].post))
  matches_stems = {}

  for hpron, foundprons in matches.iteritems():
    stems = []
    def append_stem_foundstems(stem, foundpronunstems):
      if stem and foundpronunstems:
        stems.append((stem, foundpronunstems))
    append_stem_foundstems(re.sub(u"[аеиояыьй]́?$", "", hpron),
      frob_foundprons(foundprons, lambda x:re.sub(u"[аеиояыьй]́?$", "", x)))
    # Also compute reduced/unreduced stem
    append_stem_foundstems(get_reduced_stem(hpron),
      frob_foundprons(foundprons, get_reduced_stem))
    # Also check for adjectival stem
    adjstem = re.sub(u"([иыо]́?й|[ая]́?я|[ое]́?е|[ыи]́?е)$", "", hpron)
    if adjstem != hpron:
      foundpronstems = frob_foundprons(foundprons,
          lambda x:re.sub(u"([иыо]́?й|[ая]́?я|[ое]́?е|[ыи]́?е)$", "", x))
      append_stem_foundstems(adjstem, foundpronstems)
      pagemsg("Adding adjectival stem mapping %s->%s" % (
        adjstem, ",".join(unicode(x) for x in foundpronstems)))
      # If adjectival, dereduce with both stressed and unstressed epenthetic
      # vowel
      for epvowel in [False, True]:
        deredstem = get_dereduced_adj_stem(adjstem, epvowel)
        deredfoundpronstems = frob_foundprons(foundpronstems,
            lambda x:get_dereduced_adj_stem(x, epvowel))
        append_stem_foundstems(deredstem, deredfoundpronstems)
        pagemsg("Adding adjectival dereduced stem mapping %s->%s" % (
          deredstem, ",".join(unicode(x) for x in deredfoundpronstems)))
    # Also check for verbal stem; peel off parts that don't occur in all
    # forms of the verb
    verbstem = re.sub(u"(ова́?|ну́?|[аеияо]́?)ть(ся)?$", "", hpron)
    if verbstem != hpron:
      foundpronstems = frob_foundprons(foundprons,
          lambda x:re.sub(u"(ова́?|ну́?|[аеияо]́?)ть(ся)?$", "", x))
      append_stem_foundstems(verbstem, foundpronstems)
      pagemsg("Adding verbal stem mapping %s->%s" % (
        verbstem, ",".join(unicode(x) for x in foundpronstems)))
      # Iotate both with and without shch transformation, if called for
      for shch in [False, True]:
        # shch only makes a difference if stem ends with т
        if shch and not verbstem.endswith(u"т"):
          continue
        iotstem = get_iotated_stem(verbstem, shch)
        iotfoundpronstems = frob_foundprons(foundpronstems,
            lambda x:get_iotated_stem(x, shch))
        append_stem_foundstems(iotstem, iotfoundpronstems)
        pagemsg("Adding verbal iotated stem mapping %s->%s" % (
          iotstem, ",".join(unicode(x) for x in iotfoundpronstems)))

    matches_stems[hpron] = stems

  # Check to see if the mappings have different numbers of acute accents and
  # warn if so, esp. if one mapping has acute accents and the other doesn't
  for hpron, stem_foundprons in matches_stems.iteritems():
    for stem, foundprons in stem_foundprons:
      stemaccents = stem.count(AC)
      for foundpron in foundprons:
        foundpronaccents = foundpron.pron.count(AC)
        if stemaccents != foundpronaccents:
          pagemsg("WARNING: Mapping %s->%s has different number of acute accents (%s->%s)" %
              (stem, foundpron, stemaccents, foundpronaccents))
          if not stemaccents or not foundpronaccents:
            pagemsg("WARNING: In mapping %s->%s, one has no acute accents and the other has acute accents (%s->%s)" %
                (stem, foundpron, stemaccents, foundpronaccents))

  return matches_stems

def get_lemmas_of_form_page(parsed):
  lemmas = set()
  for t in parsed.filter_templates():
    tname = unicode(t.name)
    first_param = None
    if (tname in ["inflection of", "infl of", "comparative of", "superlative of"]):
      first_param = get_first_param(t)
    elif tname == "ru-participle of":
      first_param = "1"
    if first_param:
      lemma = ru.remove_accents(blib.remove_links(getparam(t, first_param)))
      lemmas.add(lemma)
  return lemmas

# Cache mapping page titles to a set of the gem= values found on the page.
lemma_gem_cache = {}
# Cache mapping page titles to a map from headwords to pronunciations
# found on the page.
lemma_headword_to_pronun_mapping_cache = {}

# Look up the lemmas of all inflection-of templates in PARSED (the contents
# of an etym section), and for each such lemma, do two things: (1) fetch the
# gem= values from the ru-IPA templates, (2) fetch a mapping from
# headword-derived stems to pronunciations as found in the ru-IPA templates.
# Return a tuple (GEMVALS, PRONUNMAPPING), where GEMVALS is the set of all
# gem= values found and PRONUNMAPPING is a map as described above.
def lookup_gem_values_and_pronun_mapping(parsed, verbose, pagemsg):
  lemmas = get_lemmas_of_form_page(parsed)
  all_gemvals = set()
  all_pronunmappings = {}
  final_geminate_in_lemma = False
  lemma_has_geminate_other_than_final = False
  for lemma in lemmas:
    # Need to create our own expand_text() with the page title set to the
    # lemma
    def expand_text(t):
      return blib.expand_text(t, lemma, pagemsg, verbose)

    if lemma in lemma_gem_cache:
      cached = True
      assert lemma in lemma_headword_to_pronun_mapping_cache
      gemval = lemma_gem_cache[lemma]
      pronunmapping = lemma_headword_to_pronun_mapping_cache[lemma]
    else:
      cached = False
      newpage = pywikibot.Page(site, lemma)
      gemval = set()
      try:
        parsed = blib.parse(newpage)
      except pywikibot.exceptions.InvalidTitle as e:
        pagemsg("WARNING: Invalid title, skipping")
        traceback.print_exc(file=sys.stdout)
        continue

      # Compute headword->pronun mapping
      headwords = get_headword_pronuns(parsed, lemma, pagemsg, expand_text)
      foundpronuns = []

      # Find the pronunciations but also get pre-text and post-text
      for m in re.finditer(r"^(.*)(\{\{ru-IPA(?:\|[^}]*)?\}\})(.*)$",
          newpage.text, re.M):
        pretext = m.group(1)
        ruIPA = m.group(2)
        posttext = m.group(3)
        wholeline = m.group(0)
        if not pretext.startswith("* "):
          pagemsg("WARNING: ru-IPA doesn't start with '* ': %s" % wholeline)
        pretext = re.sub(r"^\*?\s*", "", pretext) # remove '* ' from beginning
        if pretext or posttext:
          pagemsg("WARNING: pre-text or post-text with ru-IPA: %s" % wholeline)
        ruIPA_t = blib.parse_text(ruIPA).filter_templates()[0]
        assert unicode(ruIPA_t.name) == "ru-IPA"
        phon = getparam(ruIPA_t, "phon")
        if phon:
          foundpronun = "phon=%s" % ru.try_to_stress(phon)
        else:
          foundpronun = ru.try_to_stress(getparam(ruIPA_t, "1"))
        foundpronuns.append(FoundPronun(foundpronun, pretext, posttext))
      pronunmapping = match_headword_and_found_pronuns(headwords, foundpronuns,
          pagemsg, expand_text)
      lemma_headword_to_pronun_mapping_cache[lemma] = pronunmapping

      # Compute gemval
      for t in parsed.filter_templates():
        if unicode(t.name) == "ru-IPA":
          gemval.add(getparam(t, "gem"))

      # Now, see if we need to modify the gem= value to take into account final
      # geminates in the lemma.

      if re.search("([" + ru.cons + r"])\1( |,|$)", lemma):
        pagemsg("Found lemma %s with final geminate" % lemma)
        has_gemval = not not [x for x in gemval if x]
        if has_gemval:
          pagemsg("WARNING: Found final geminate in lemma %s and gem= present, can't set gem=n" %
              lemma)
        else:
          other_geminate_in_lemma = False
          for t in parsed.filter_templates():
            if unicode(t.name) == "ru-IPA":
              pronunval = expand_text(re.sub(r"\}\}$", "|raw=y}}", unicode(t)))
              # The following regex will trigger on C(ː), which is correct
              if re.search(u"[^ɕʑ]ː", pronunval):
                other_geminate_in_lemma = True
          # Also try the plain headwords as pronun, in case of missing pronun
          # or some reason where the stem->pronun mapping fails
          for headword, tr in headwords:
            pronunval = expand_text("{{ru-IPA|%s|raw=y}}" % headword)
            if re.search(u"[^ɕʑ]ː", pronunval):
              other_geminate_in_lemma = True
          if other_geminate_in_lemma:
            pagemsg("WARNING: Other geminate in lemma %s with final geminate, can't set gem=n" %
                lemma)
          else:
            pagemsg("Found final geminate in lemma %s, setting gem=n" % lemma)
            gemval = set(["n"])

      lemma_gem_cache[lemma] = gemval

    pagemsg("For lemma %s, found gem=%s%s" % (lemma, ",".join(gemval),
      cached and " (cached)" or ""))
    all_gemvals |= gemval
    # The output is HEADWORD->(STEMS_AND_PRONUNS),HEADWORD->(STEMS_AND_PRONUNS)...
    # where STEMS_AND_PRONUNS is STEM:PRONUNS,STEM:PRONUNS,...,
    # where PRONUNS is PRONUN/PRONUN/...
    # where PRONUN may be PRON or [PRE]PRON or PRON[POST] or [PRE]PRON[POST]
    pagemsg("For lemma %s, found pronun mapping %s%s" % (lemma, "None" if
      pronunmapping is None else "(empty)" if not pronunmapping else ",".join(
        "%s->(%s)" % (hpron, ",".join("%s:%s" % (stem, "/".join(unicode(x) for x in foundprons))
          for stem, foundprons in stem_foundprons))
        for hpron, stem_foundprons in pronunmapping.iteritems()),
      cached and " (cached)" or ""))
    if pronunmapping:
      all_pronunmappings.update(pronunmapping)

  if len(all_gemvals) > 1:
    pagemsg("WARNING: Found multiple gem= values (gem=%s) corresponding to lemma %s, not using" %
        (",".join(all_gemvals), ",".join(lemmas)))
  return all_gemvals, all_pronunmappings

def process_section(section, indentlevel, headword_pronuns, program_args,
    pagetitle, verbose, pagemsg, expand_text):
  assert indentlevel in [3, 4]
  notes = []

  was_unable_to_match = False

  def compute_ipa():
    computed_ipa = {}
    for cyr, tr in headword_pronuns:
      pronun = tr or cyr
      result = expand_text("{{#invoke:ru-pron|ipa_string|%s}}" % pronun)
      if not result:
        return False
      computed_ipa[pronun] = result
    return computed_ipa

  parsed = blib.parse_text(section)

  # Find gem= param by looking up {{inflection of}} pages
  headword_gemparam = ""
  gemvals, pronunmapping = lookup_gem_values_and_pronun_mapping(parsed,
      verbose, pagemsg)
  if len(gemvals) == 1:
    gemval = list(gemvals)[0]
    if gemval:
      headword_gemparam = "|gem=%s" % gemval
      pagemsg("Adding %s to ru-IPA pronun(s)" % headword_gemparam)

  pronun_lines = []
  bad_char_msgs = []
  # Figure out how many headword variants there are, and if there is more
  # than one, add |ann=y to each one; but don't get confused by cases where
  # there are multiple translit variants of the same headword, as in
  # {{ru-noun form|а́бвера|tr=ábvera, ábvɛra|m-in}}.
  num_annotations = 0
  annotations_set = set()
  for cyr, tr in headword_pronuns:
    annotations_set.add(cyr)
  matched_hpron = set()
  manually_subbed_pronun = False
  # List of pronunciations to insert into comment message; approximately
  # the same as what goes inside {{ru-IPA}}, except we don't include the
  # ann= parameter and we do include the pronunciation even if we leave
  # it out in {{ru-IPA}} because it's the same as the page title.
  pronuns_for_comment = []
  for pronun, tr in headword_pronuns:
    # Signal from within append_pronun_line() that we encountered badness
    # in the pronunciation and need to skip setting it.
    # HACK! Use an array to avoid problems setting non-local variables.
    bad_pronun_need_to_return = [False]

    orig_pronun = pronun

    def canonicalize_annotation(ann):
       return ru.remove_grave_accents(re.sub(
         "[" + CFLEX + DUBGR + DOTABOVE + DOTBELOW + GR + u"‿]", "", ann))

    def append_pronun_line(pronun, pre="", post=""):
      if len(annotations_set) > 1:
        # Need an annotation. Check to see whether |ann=y is possible: The
        # original pronunciation is the same as the new one (but we allow
        # possible differences in DOTBELOW, grave accents, etc. because they
        # will be stripped with |ann=y).
        if (canonicalize_annotation(orig_pronun) !=
            canonicalize_annotation(pronun)):
          # Don't include DOTBELOW, grave accents, etc. in the annotation param
          # or they will be shown to the user.
          headword_annparam = "|ann=%s" % canonicalize_annotation(orig_pronun)
        else:
          headword_annparam = "|ann=y"
      else:
        headword_annparam = ""

      # Check for various badnesses in the pronunciation
      check_pronun = re.sub(r"\|gem=.*", "", re.sub("^phon=", "", pronun))
      if check_pronun.startswith("-") or check_pronun.endswith("-"):
        pagemsg("WARNING: Skipping prefix or suffix: %s" % pronun)
        bad_pronun_need_to_return[0] = True
      if "." in check_pronun:
        pagemsg("WARNING: Pronunciation has dot in it, skipping: %s" % pronun)
        bad_pronun_need_to_return[0] = True
      if ru.needs_accents(check_pronun, split_dash=True):
        for allow_regex in allow_unaccented:
          if re.search(allow_regex, pagetitle):
            pagemsg("Pronunciation lacks accents but pagetitle in allow_unaccented, allowing: %s"
                 % pronun)
            break
        else: # no break
          pagemsg("WARNING: Pronunciation lacks accents, skipping: %s" % pronun)
          bad_pronun_need_to_return[0] = True
      # Check for non-Cyrillic or Latin chars. We set bad_char_msgs, which we
      # pay attention to farther down, just before adding the actual pronun,
      # so we can get other warnings as well. FIXME: Maybe we should do the
      # same with the above badnesses?
      if contains_non_cyrillic_non_latin(check_pronun):
        bad_char_msgs.append(
            "WARNING: Pronunciation %s to be added contains non-Cyrillic non-Latin chars, skipping" %
              pronun)
      elif contains_latin(check_pronun):
        bad_char_msgs.append(
            "WARNING: Cyrillic pronunciation %s contains Latin characters, skipping" %
            pronun)

      # If pronunciation has final geminate and we found gem=y or gem=opt in
      # the lemma, remove it.
      our_headword_gemparam = headword_gemparam # avoid 'ref before assign' err
      if re.search("([" + ru.cons + r"])\1( |,|$)", pronun):
        pagemsg("Found pronun %s with final geminate" % pronun)
        gem_y_or_opt = False
        if not headword_gemparam or headword_gemparam == "|gem=n":
          pass
        elif headword_gemparam in ["|gem=y", "|gem=opt"]:
          gem_y_or_opt = True
        else:
          pagemsg("WARNING: Unrecognized gemination spec %s, skipping" %
              headword_gemparam)
        if gem_y_or_opt:
          other_geminate_in_pronun = False
          pronunval = expand_text("{{ru-IPA|%s%s|raw=y}}" %
              (pronun, headword_gemparam))
          # Eliminate final geminates from pronunciation
          pronunval = re.sub(ur"(ː|\(ː\))( |,|\]|$)", r"\2", pronunval)
          # The following regex will trigger on C(ː), which is correct
          if re.search(u"[^ɕʑ]ː", pronunval):
            other_geminate_in_pronun = True
          if other_geminate_in_pronun:
            pagemsg("WARNING: Other geminate in pronun %s with final geminate, can't remove gem=y or gem=opt" %
                pronun)
          else:
            pagemsg("Found final geminate in pronun %s, removing gem=y and gem=opt" % pronun)
            our_headword_gemparam = ""

      pronun_base_for_comment = "%s%s" % (pronun, our_headword_gemparam)
      pronun_for_comment = unicode(FoundPronun(pronun_base_for_comment, pre, post))
      if pronun_for_comment not in pronuns_for_comment:
        pronuns_for_comment.append(pronun_for_comment)

      if (not pronun.startswith("phon=") and (
         ru.is_monosyllabic(pronun) and re.sub(AC, "", pronun) == pagetitle or
         re.search(u"ё", pronun) and pronun == pagetitle)):
        pronun = "* %s{{ru-IPA%s%s}}%s\n" % (pre, headword_annparam,
            our_headword_gemparam, post)
      else:
        pronun = "* %s{{ru-IPA|%s%s%s}}%s\n" % (pre, pronun, headword_annparam,
            our_headword_gemparam, post)
      if pronun not in pronun_lines:
        pronun_lines.append(pronun)

    subbed_pronun = False

    # Check for manual pronunciation mapping
    for regex, subvals in manual_pronun_mapping:
      if re.search(regex, pronun):
        applied_manual_pronun_mappings.add(regex)
        if type(subvals) is not list:
          subvals = [subvals]
        for subval in subvals:
          if type(subval) is tuple:
            subval, pre, post = subval
          else:
            subval, pre, post = (subval, "", "")
          newpronun = re.sub(regex, subval, pronun)
          pagemsg("Replacing headword-based pronunciation %s with %s due to manual_pronun_mapping"
              % (pronun, newpronun))
          append_pronun_line(newpronun, pre, post)
        subbed_pronun = True
        manually_subbed_pronun = True
        break

    # If there is an automatically-derived headword->pronun mapping (e.g.
    # in case of secondary stress or phon=), try to apply it.
    if not subbed_pronun and pronunmapping:
      for hpron, stem_foundprons in pronunmapping.iteritems():
        outerbreak = False
        for stem, foundpronstems in stem_foundprons:
          assert stem
          assert foundpronstems
          if pronun.startswith(stem):
            for foundpronstem in foundpronstems:
              newpronun = re.sub("^" + re.escape(stem), foundpronstem.pron,
                  pronun)
              if newpronun != pronun:
                pagemsg("Replacing headword-based pronunciation %s with %s" %
                    (pronun, newpronun))
              append_pronun_line(newpronun, foundpronstem.pre, foundpronstem.post)
            subbed_pronun = True
            matched_hpron.add(hpron)
            outerbreak = True
            break
        if outerbreak:
          break

    # Otherwise, reverse-translit if transliteration, or use headword pronun
    # unchanged.
    if subbed_pronun:
      pass
    elif tr:
      reverse_translit = ru_reverse_translit.reverse_translit(tr, pronun)
      pagemsg("WARNING: Reverse-transliterating %s to phon=%s" %
          (tr, reverse_translit))
      append_pronun_line("phon=%s" % reverse_translit)
    else:
      append_pronun_line(pronun)

    # Skip if badness in pronunciation
    if bad_pronun_need_to_return[0]:
      return None

  if pronunmapping and not manually_subbed_pronun:
    for hpron, stem_foundprons in pronunmapping.iteritems():
      if hpron not in matched_hpron:
        pagemsg("WARNING: Unable to match mapping %s->(%s) in non-lemma form(s)"
          % (hpron, ",".join("%s:%s" % (stem, "/".join(unicode(x) for x in foundprons))
            for stem, foundprons in stem_foundprons)))
        was_unable_to_match = True

  # Check for indications of pre-reform spellings
  for cat in [u"Russian spellings with е instead of ё",
      u"Russian terms spelled with Ѣ",
      u"Russian terms spelled with Ѳ",
      u"Russian pre-1918 spellings"]:
    if re.search(ur"\[\[Category:%s]]" % cat, section):
      pagemsg(u"WARNING: Found [[Category:%s]], skipping" % cat)
      return None

  ipa_templates = []
  for t in parsed.filter_templates():
    tname = unicode(t.name)
    if tname in ["ru-pre-reform", u"ru-alt-ё", "ru-IPA-manual"] or tname in alt_yo_templates:
      pagemsg("WARNING: Found %s template, skipping" % tname)
      return None
    if tname in ["alternative form of", "alternative spelling of"]:
      first_param = get_first_param(t)
      if first_param:
        # Check if word spelled with е instead of ё, without using
        # [[Category:Russian spellings with е instead of ё]], which we
        # catch above.
        target = getparam(t, first_param)
        if u"ё" in target and re.sub(u"ё", u"е", target) == pagetitle:
          pagemsg(u"WARNING: Found apparent alternative form using е in place of ё without explicit category, skipping: %s" %
              unicode(t))
          return None
    if tname == "IPA" and get_first_param(t):
      ipa_templates.append(t)
  if (re.search(r"[Aa]bbreviation", section) and not
      re.search("==Abbreviations==", section) or
      re.search("ru-(etym )?abbrev of", section)):
    pagemsg("WARNING: Found the word 'abbreviation', please check")
  if (re.search(r"[Aa]cronym", section) and not
      re.search("==Acronyms==", section)):
    pagemsg("WARNING: Found the word 'acronym', please check")
  if (re.search(r"[Ii]nitialism", section) and not
      re.search("==Initialisms==", section)):
    pagemsg("WARNING: Found the word 'initialism', please check")
  if ipa_templates:
    ipa_templates_msg = (
      "Processing raw IPA %s for headword(s) %s" % (
      "++".join([unicode(x) for x in ipa_templates]),
      "++".join(printable_ru_tr(cyr, ru) for cyr, ru in headword_pronuns)))
    pagemsg(ipa_templates_msg)
    computed_ipa = compute_ipa()
    num_replaced = 0
    if not computed_ipa:
      # Error occurred computing IPA of headwords
      return None
    for ipa_template in ipa_templates:
      mismatch_msgs = []
      computed_ipa_items = computed_ipa.items()
      for headword, autoipa in computed_ipa_items:
        if contains_latin(headword):
          pagemsg("WARNING: Headword %s to be used to replace manual IPA contains Latin chars, skipping" %
                headword)
          return None
        elif contains_non_cyrillic_non_latin(headword):
          pagemsg("WARNING: Headword %s to be used to replace manual IPA contains non-Cyrillic non-Latin chars, skipping" %
                headword)
          return None
        retval = ipa_matches(headword, getparam(ipa_template, "1"), autoipa,
            ipa_templates_msg, pagemsg)
        if retval != True and program_args.override_IPA and (
            len(ipa_templates) > 1 or len(computed_ipa_items) > 1):
          pagemsg("WARNING: Can't override IPA because multiple IPA templates or headwords: %s template(s), %s headword(s)" % (
            len(ipa_templates), len(computed_ipa_items)))
        elif retval == True or program_args.override_IPA:
          orig_ipa_template = unicode(ipa_template)
          lang = getparam(ipa_template, "lang")
          if lang:
            assert lang == "ru"
            rmparam(ipa_template, "lang")
            rmparam(ipa_template, "1")
          else:
            rmparam(ipa_template, "1")
            rmparam(ipa_template, "2")
          if len(ipa_template.params) > 0:
            pagemsg("WARNING: IPA template has extra parameters, skipping: %s" %
                orig_ipa_template)
            return None
          ipa_template.name = "ru-IPA"
          ipa_template.add("1", headword)
          if retval != True and program_args.override_IPA:
            pagemsg("WARNING: Overriding IPA despite pronunciation mismatch")
            mismatch_msgs.append(retval)
            for m in mismatch_msgs:
              pagemsg(re.sub("^WARNING:", "WARNING (IGNORED):", m))
          pagemsg("Replaced %s with %s" % (
            orig_ipa_template, unicode(ipa_template)))
          num_replaced += 1
          mismatch_msgs = []
          notes.append("replace {{IPA|...}} with {{ru-IPA|...}} for %s%s" % (
            headword, " (IPA override)" if retval != True and program_args.override_IPA else ""))
          break
        mismatch_msgs.append(retval)
      if mismatch_msgs:
        for m in mismatch_msgs:
          pagemsg(m)
    if num_replaced > 0:
      section = unicode(parsed)
    if num_replaced < len(ipa_templates):
      pagemsg("Unable to replace %s of %s raw IPA template(s)" % (
        len(ipa_templates) - num_replaced, len(ipa_templates)))
    return section, notes, was_unable_to_match

  def canonicalize_pronun(pron, paramname):
    newpron = re.sub(u"ё́", u"ё", pron)
    newpron = re.sub(AC + "+", AC, newpron)
    if newpron != pron:
      notes.append("remove extra accents from %s= (ru-IPA)" % paramname)
      pron = newpron
    # We want to go word-by-word and check to see if the headword word is
    # the same as the ru-IPA word but has additional accents in it, and
    # if so copy the headword word to the ru-IPA word. One way to do that
    # is to check that the ru-IPA word has no accents and that the headword
    # word minus accents is the same as the ru-IPA word.
    if not bad_char_msgs and len(headword_pronuns) == 1 and not headword_pronuns[0][1]:
      # FIXME, handle translit here
      hwords = re.split(r"([\s\-]+)", headword_pronuns[0][0])
      pronwords = re.split(r"([\s\-]+)", pron)
      changed = False
      if len(hwords) == len(pronwords):
        for i in xrange(len(hwords)):
          hword = hwords[i]
          pronword = pronwords[i]
          if (len(hword) > len(pronword) and ru.is_unaccented(pronword) and
              ru.remove_accents(hword) == pronword):
            changed = True
            pronwords[i] = hword
      if changed:
        pron = "".join(pronwords)
        notes.append("copy accents from headword to %s= (ru-IPA)" % paramname)
    return pron

  parsed = blib.parse_text(section)
  for t in parsed.filter_templates():
    if unicode(t.name) == "ru-IPA":
      origt = unicode(t)
      phon = getparam(t, "phon")
      if phon:
        if t.has("1"):
          rmparam(t, "1")
          notes.append("remove 1= when phon= present (ru-IPA)")
        newphon = canonicalize_pronun(phon, "phon")
        if phon != newphon:
          t.add("phon", newphon)
      arg1 = getparam(t, "1")
      if arg1:
        newarg1 = canonicalize_pronun(arg1, "1")
        if arg1 != newarg1:
          t.add("1", newarg1)
          arg1 = newarg1
        if ru.is_monosyllabic(arg1) and re.sub(AC, "", arg1) == pagetitle:
          notes.append("remove 1= because monosyllabic and same as pagetitle modulo accents (ru-IPA)")
          rmparam(t, "1")
        elif re.search(u"ё", arg1) and arg1 == pagetitle:
          notes.append(u"remove 1= because same as pagetitle and has ё (ru-IPA)")
          rmparam(t, "1")
      if t.has("adj"):
        notes.append("remove unused adj= (ru-IPA)")
        rmparam(t, "adj")
      newt = unicode(t)
      if newt != origt:
        pagemsg("Replaced %s with %s" % (origt, newt))
  section = unicode(parsed)

  overrode_existing_pronun = False
  if program_args.override_pronun:
    pronun_line_re = r"^(\* .*\{\{ru-IPA(?:\|([^}]*))?\}\}.*)\n"
    for m in re.finditer(pronun_line_re, section, re.M):
      overrode_existing_pronun = True
      pagemsg("WARNING: Removing pronunciation due to --override-pronun: %s" %
          m.group(1))
    section = re.sub(pronun_line_re, "", section, 0, re.M)

  foundpronuns = []
  for m in re.finditer(r"(\{\{ru-IPA(?:\|([^}]*))?\}\})", section):
    template_text = m.group(1)
    pagemsg("Already found pronunciation template: %s" % template_text)
    template = blib.parse_text(template_text).filter_templates()[0]
    phonparam = getparam(template, "phon")
    foundpronun = phonparam or getparam(template, "1") or pagetitle
    foundpronun = canonicalize_monosyllabic_pronun(foundpronun, "")[0]
    if phonparam:
      foundpronun = "phon=" + foundpronun
    # FIXME, not clear if we want to do this
    gemparam = getparam(template, "gem")
    if gemparam:
      foundpronun += "|gem=" + gemparam
    foundpronuns.append(foundpronun)
  if foundpronuns:
    joined_foundpronuns = ",".join(foundpronuns)
    headword_pronuns_as_pronuns_no_gem = [ru_tr_as_pronun(cyr, tr) for cyr, tr in headword_pronuns]
    headword_pronuns_as_pronuns = [x + headword_gemparam for x in headword_pronuns_as_pronuns_no_gem]
    joined_headword_pronuns = ",".join(headword_pronuns_as_pronuns)
    if "phon=" not in joined_foundpronuns and "phon=" in joined_headword_pronuns:
      pagemsg("WARNING: Existing pronunciation template %s probably needs phon= because headword-derived pronunciation %s contains Latin" %
        (joined_foundpronuns, joined_headword_pronuns))
    if "phon=" in joined_foundpronuns and "phon=" not in joined_headword_pronuns:
      pagemsg("WARNING: Existing pronunciation template has pronunciation %s with phon=, headword-derived pronunciation %s isn't Latin, probably need manual translit in headword and decl" %
        (joined_foundpronuns, joined_headword_pronuns))
    if "gem=" not in joined_foundpronuns and "gem=" in joined_headword_pronuns:
      pagemsg("WARNING: Existing pronunciation template %s probably needs gem= because headword-derived pronunciation %s has it" %
        (joined_foundpronuns, joined_headword_pronuns))

    if len(foundpronuns) < len(headword_pronuns_as_pronuns):
      pagemsg("WARNING: Fewer existing pronunciations (%s) than headword-derived pronunciations (%s): existing %s, headword-derived %s" % (
        len(foundpronuns), len(headword_pronuns_as_pronuns),
        joined_foundpronuns, joined_headword_pronuns))
    headword_pronuns_as_pronuns_no_gem_or_grave = [ru.remove_grave_accents(x) for x in headword_pronuns_as_pronuns_no_gem]
    headword_pronuns_as_pronuns_no_gem_grave_or_dotbelow = [x.replace(DOTBELOW, "") for x in headword_pronuns_as_pronuns_no_gem_or_grave]
    foundpronuns_no_gem = [re.sub(r"\|gem=[^|]*", "", x) for x in foundpronuns]
    foundpronuns_no_gem_or_grave = [ru.remove_grave_accents(x) for x in foundpronuns_no_gem]
    foundpronuns_no_gem_grave_or_dotbelow = [x.replace(DOTBELOW, "") for x in foundpronuns_no_gem_or_grave]
    if set(foundpronuns_no_gem_grave_or_dotbelow) != set(headword_pronuns_as_pronuns_no_gem_grave_or_dotbelow):
      pagemsg("WARNING: Existing pronunciation template (w/o gem=, grave accent or dotbelow) has different pronunciation %s from headword-derived pronunciation %s" %
            (joined_foundpronuns, joined_headword_pronuns))
    elif set(foundpronuns_no_gem_or_grave) != set(headword_pronuns_as_pronuns_no_gem_or_grave):
      pagemsg("WARNING: Existing pronunciation template (w/o gem= or grave accent) has different pronunciation %s from headword-derived pronunciation %s, but only in dotbelow" %
            (joined_foundpronuns, joined_headword_pronuns))
    elif set(foundpronuns_no_gem) != set(headword_pronuns_as_pronuns_no_gem):
      pagemsg("WARNING: Existing pronunciation template (w/o gem=) has different pronunciation %s from headword-derived pronunciation %s, but only in grave accents" %
            (joined_foundpronuns, joined_headword_pronuns))
    elif set(foundpronuns) != set(headword_pronuns_as_pronuns):
      pagemsg("WARNING: Existing pronunciation template has different pronunciation %s from headword-derived pronunciation %s, but only in gem=" %
            (joined_foundpronuns, joined_headword_pronuns))

    return section, notes, was_unable_to_match

  pronunsection = "%sPronunciation%s\n%s\n" % ("="*indentlevel, "="*indentlevel,
      "".join(pronun_lines))

  if bad_char_msgs:
    for badmsg in bad_char_msgs:
      pagemsg(badmsg)
      return None

  origsection = section
  # If pronunciation section already present, insert pronun into it; this
  # could happen when audio but not IPA is present, or when we deleted the
  # pronunciation because of --override-pronun
  if re.search(r"^===+Pronunciation===+$", section, re.M):
    pagemsg("Found pronunciation section without ru-IPA or IPA")
    section = re.sub(r"^(===+Pronunciation===+)\n", r"\1\n%s" %
        "".join(pronun_lines), section, 1, re.M)
  else:
    # Otherwise, skip past any ===Etymology=== or ===Alternative forms===
    # sections at the beginning. This requires us to split up the subsections,
    # find the right subsection to insert before, and then rejoin.
    subsections = re.split("(^===.*?===\n)", section, 0, re.M)

    insert_before = 1
    while True:
      if insert_before >= len(subsections):
        pagemsg("WARNING: Malformatted headers, no level-3/4 POS header")
        return None
      if ("===Alternative forms===" not in subsections[insert_before] and
          "===Etymology===" not in subsections[insert_before]):
        break
      insert_before += 2
    subsections[insert_before] = re.sub(r"(^===)", r"%s\1" % pronunsection, subsections[insert_before], 1, re.M)
    section = "".join(subsections)

  # Make sure there's a blank line before an initial header (even if there
  # wasn't one before).
  section = re.sub("^===", "\n===", section, 1)

  if section == origsection:
    pagemsg("WARNING: Something wrong, couldn't sub in pronunciation section")
    return None

  if overrode_existing_pronun:
    notes.append("override pronunciation with %s" % ",".join(pronuns_for_comment))
  else:
    notes.append("add pronunciation %s" % ",".join(pronuns_for_comment))

  return section, notes, was_unable_to_match

def process_page_text(index, text, pagetitle, verbose, program_args):
  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  def expand_text(tempcall):
    return blib.expand_text(tempcall, pagetitle, pagemsg, verbose)

  notes = []

  foundrussian = False
  was_unable_to_match = False
  sections = re.split("(^==[^=]*==\n)", text, 0, re.M)
  orig_text = text
  for j in xrange(2, len(sections), 2):
    if sections[j-1] == "==Russian==\n":
      if foundrussian:
        pagemsg("WARNING: Found multiple Russian sections")
        return None
      foundrussian = True

      need_l3_pronun = False
      if "===Pronunciation 1===" in sections[j]:
        pagemsg("WARNING: Found ===Pronunciation 1===, should convert page to multiple etymologies")
        return None
      if "===Etymology 1===" in sections[j]:

        # If multiple etymologies, things are more complicated. We may have to
        # process each section individually. We fetch the headwords from each
        # section to see whether the etymologies should be in split or
        # combined form. If they should be in split form, we remove any
        # combined pronunciation and add pronunciations to each section if
        # not already present. If they should be in combined form, we
        # remove pronunciations from individual sections (PARTLY IMPLEMENTED)
        # and add a combined pronunciation at the top.

        etymsections = re.split("(^ *=== *Etymology +[0-9]+ *=== *\n)", sections[j], 0, re.M)
        # Make sure there are multiple etymologies, otherwise page is malformed
        pagemsg("Found multiple etymologies (%s)" % (len(etymsections)//2))
        if len(etymsections) < 5:
          pagemsg("WARNING: Misformatted page with multiple etymologies (too few etymologies, skipping)")
          return None

        # Check for misnumbered etymology sections
        # FIXME, this should be a separate script
        expected_etym_num = 0
        l3split = re.split(r"^(===[^=\n].*===\n)", sections[j], 0, re.M)
        seen_etym_1 = False
        for k in xrange(1, len(l3split), 2):
          if not seen_etym_1 and l3split[k] != "===Etymology 1===\n":
            continue
          seen_etym_1 = True
          expected_etym_num += 1
          if l3split[k] != "===Etymology %s===\n" % expected_etym_num:
            pagemsg("WARNING: Misformatted page with multiple etymologies, expected ===Etymology %s=== but found %s" % (
              expected_etym_num, l3split[k].replace("\n", "")))
            break

        # Check if all per-etym-section headwords are the same
        etymparsed2 = blib.parse_text(etymsections[2])
        etym_headword_pronuns = {}
        # Fetch the headword pronuns of the ===Etymology 1=== section.
        # We don't check for None here so that an error in an individual
        # section doesn't cause us to bow out entirely; instead, we treat
        # any comparison with None as False so we will always end up with
        # per-section pronunciations.
        etym_headword_pronuns[2] = get_headword_pronuns(etymparsed2, pagetitle, pagemsg, expand_text)
        need_per_section_pronuns = False
        for k in xrange(4, len(etymsections), 2):
          etymparsed = blib.parse_text(etymsections[k])
          # Fetch the headword pronuns of the ===Etymology N=== section.
          # We don't check for None here; see above.
          etym_headword_pronuns[k] = get_headword_pronuns(etymparsed, pagetitle, pagemsg, expand_text)
          # Treat any comparison with None as False.
          if not etym_headword_pronuns[2] or not etym_headword_pronuns[k] or set(etym_headword_pronuns[k]) != set(etym_headword_pronuns[2]):
            pagemsg("WARNING: Etym section %s pronuns %s different from etym section 1 pronuns %s" % (
              k//2, printable_ru_tr_list(etym_headword_pronuns[k] or [("none", "")]), printable_ru_tr_list(etym_headword_pronuns[2] or [("none", "")])))
            need_per_section_pronuns = True
        numpronunsecs = len(re.findall("^===Pronunciation===$", etymsections[0], re.M))
        if numpronunsecs > 1:
          pagemsg("WARNING: Multiple ===Pronunciation=== sections in preamble to multiple etymologies, needs to be fixed")
          return None

        if need_per_section_pronuns:
          pagemsg("Multiple etymologies, split pronunciations needed")
        else:
          pagemsg("Multiple etymologies, combined pronunciation possible")

        # If need split pronunciations and there's a combined pronunciation,
        # delete it if possible.
        if need_per_section_pronuns and numpronunsecs == 1:
          pagemsg("Multiple etymologies, converting combined pronunciation to split pronunciation (deleting combined pronun)")
          # Remove existing pronunciation section; but make sure it's safe
          # to do so (must have nothing but ru-IPA templates in it, and the
          # pronunciations in them must match what's expected)
          m = re.search(r"(^===Pronunciation===\n)(.*?)(^==|\Z)", etymsections[0], re.M | re.S)
          if not m:
            pagemsg("WARNING: Can't find ===Pronunciation=== section when it should be there, logic error?")
            return None
          if not re.search(r"^(\* \{\{ru-IPA(?:\|([^}]*))?\}\}\n)*$", m.group(2)):
            pagemsg("WARNING: Pronunciation section to be removed contains extra stuff (e.g. manual IPA or audio), can't remove: <%s>\n" % (
              m.group(1) + m.group(2)))
            return None
          foundpronuns = []
          for m in re.finditer(r"(\{\{ru-IPA(?:\|([^}]*))?\}\})", m.group(2)):
            # FIXME, not right, should do what we do above with foundpronuns
            # where we work with the actual parsed template
            foundpronuns.append(m.group(2) or pagetitle)
          # FIXME, this may be wrong with translit
          foundpronuns = remove_list_duplicates([canonicalize_monosyllabic_pronun(x, "")[0] for x in foundpronuns])
          if foundpronuns:
            joined_foundpronuns = ",".join(foundpronuns)
            # Combine headword pronuns while preserving order. To do this,
            # we sort by numbered etymology sections and then flatten.
            combined_headword_pronuns = remove_list_duplicates([y for k,v in sorted(etym_headword_pronuns.iteritems(), key=lambda x:x[0]) for y in (v or [])])
            joined_headword_pronuns = ",".join(ru_tr_as_pronun(cyr, tr) for cyr, tr in combined_headword_pronuns)
            if not (set(foundpronuns) <= set(combined_headword_pronuns)):
              pagemsg("WARNING: When trying to delete pronunciation section, existing pronunciation %s not subset of headword-derived pronunciation %s, unable to delete" %
                    (joined_foundpronuns, joined_headword_pronuns))
              return None
          etymsections[0] = re.sub(r"(^===Pronunciation===\n)(.*?)(\Z|^==|^\[\[|^--)", r"\3", etymsections[0], 1, re.M | re.S)
          sections[j] = "".join(etymsections)
          text = "".join(sections)
          notes.append("remove combined pronun section")
          pagemsg("Removed pronunciation section because combined pronunciation with multiple etymologies needs to be split")

        # If need combined pronunciations, check for split pronunciations and
        # remove them. As a special case, if there's only one split
        # pronunciation, just move the whole section to the top. We do this
        # so we move audio, homophones, etc. This situation will frequently
        # happen when a script adds a non-lemma form to an existing page
        # without split etymologies, because it wraps everything in an
        # "Etymology 1" section.
        # FIXME: When we move the whole section to the top, it could be
        # incorrect to do so if the ru-IPA isn't just the headword, e.g. if
        # it has a strange spelling, or phon= or gem=, etc. We should probably
        # check for this.
        if not need_per_section_pronuns:
          # Check for a single pronunciation section that we can move
          num_secs_with_pronun = 0
          first_sec_with_pronun = 0
          for k in xrange(2, len(etymsections), 2):
            if "===Pronunciation===" in etymsections[k]:
              num_secs_with_pronun += 1
              if not first_sec_with_pronun:
                first_sec_with_pronun = k
          if num_secs_with_pronun == 1:
            # Section ends with another section start, end of text, a wikilink
            # or category link, or section divider. (Normally there should
            # always be another section following.)
            m = re.search(r"(^===+Pronunciation===+\n.*?)(\Z|^==|^\[\[|^--)",
                etymsections[first_sec_with_pronun], re.M | re.S)
            if not m:
              pagemsg("WARNING: Can't find ====Pronunciation==== section when it should be there, logic error?")
            else:
              # Set indentation of Pronunciation to 3
              pronunsec = re.sub(r"===+Pronunciation===+",
                  "===Pronunciation===", m.group(1))
              etymsections[first_sec_with_pronun] = re.sub(
                  r"^(===+Pronunciation===+\n.*?)(\Z|^==|^\[\[|^--)", r"\2",
                  etymsections[first_sec_with_pronun], 1, re.M | re.S)
              etymsections[0] = ensure_two_trailing_nl(etymsections[0])
              etymsections[0] += pronunsec
              sections[j] = "".join(etymsections)
              text = "".join(sections)
              notes.append("move split pronun section to top to make combined")
              pagemsg("Moved split pronun section for ===Etymology %s=== to top" % (k//2))
          elif num_secs_with_pronun > 1:
            pagemsg("WARNING: need combined pronunciation section, but there are multiple split pronunciation sections, code to delete them not implemented; delete manually)")
              # FIXME: Implement me

        # Now add the per-section or combined pronunciation
        if need_per_section_pronuns:
          for k in xrange(2, len(etymsections), 2):
            # Skip processing if pronuns are None.
            if not etym_headword_pronuns[k]:
              continue
            result = process_section(etymsections[k], 4,
                etym_headword_pronuns[k], program_args, pagetitle,
                verbose, pagemsg, expand_text)
            if result is None:
              continue
            etymsections[k], etymsection_notes, etymsection_unable_to_match = result
            notes.extend(etymsection_notes)
            was_unable_to_match = was_unable_to_match or etymsection_unable_to_match
          sections[j] = "".join(etymsections)
          text = "".join(sections)
        else:
          need_l3_pronun = True

      else:
        need_l3_pronun = True

      if need_l3_pronun:
        # Get the headword pronunciations for the whole page.
        # NOTE: Perhaps when we've already computed per-section headword
        # pronunciations, as with multiple etymologies, we should combine
        # them rather than checking the whole page. This will make a
        # difference if there are headwords outside of the etymology sections,
        # but that shouldn't happen and is a malformed page if so.
        # NOTE NOTE: If we combine headword pronunciations with multiple
        # etymologies, we need to preserve the order as found on the page.
        headword_pronuns = get_headword_pronuns(blib.parse_text(text), pagetitle, pagemsg, expand_text)
        # If error, skip page.
        if headword_pronuns is None:
          return None

        # Process the section
        result = process_section(sections[j], 3, headword_pronuns,
            program_args, pagetitle, verbose, pagemsg, expand_text)
        if result is None:
          continue
        sections[j], section_notes, section_unable_to_match = result
        notes.extend(section_notes)
        was_unable_to_match = was_unable_to_match or section_unable_to_match
        text = "".join(sections)

  if not foundrussian:
    pagemsg("WARNING: Can't find Russian section")
    return None

  comment = None
  if notes:
    # Group identical notes together and append the number of such identical
    # notes if > 1
    # 1. Count items in notes[] and return a key-value list in descending order
    notescount = Counter(notes).most_common()
    # 2. Recreate notes
    def fmt_key_val(key, val):
      if val == 1:
        return "%s" % key
      else:
        return "%s (%s)" % (key, val)
    notes = [fmt_key_val(x, y) for x, y in notescount]
    comment = "; ".join(notes)

  return text, comment, was_unable_to_match

def process_page(index, page, save, verbose, program_args):
  pagetitle = unicode(page.title())

  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  pagemsg("Processing")

  if ":" in pagetitle:
    pagemsg("WARNING: Colon in page title, skipping")
    return

  for skip_regex in skip_pages:
    if re.search(skip_regex, pagetitle):
      pagemsg("WARNING: Skipping because page in skip_pages matching %s" %
          skip_regex)
      return

  if not page.exists():
    pagemsg("WARNING: Page doesn't exist")
    return

  text = unicode(page.text)
  result = process_page_text(index, text, pagetitle, verbose, program_args)
  if result is None:
    return

  newtext, comment, was_unable_to_match = result

  if newtext != text:
    assert comment
    if was_unable_to_match:
      pagemsg("WARNING: Would save and unable to match mapping")

  # Eliminate sequences of 3 or more newlines, which may come from
  # ensure_two_trailing_nl(). Add comment if none, in case of existing page
  # with extra newlines.
  newnewtext = re.sub(r"\n\n\n+", r"\n\n", newtext)
  if newnewtext != newtext and not comment:
    comment = "eliminate sequences of 3 or more newlines"
  newtext = newnewtext

  if newtext != text:
    if verbose:
      pagemsg("Replacing <%s> with <%s>" % (text, newtext))

    if save:
      pagemsg("Saving with comment = %s" % comment)
      page.text = newtext
      blib.try_repeatedly(lambda:page.save(comment=comment), pagemsg)
    else:
      pagemsg("Would save with comment = %s" % comment)

def process_lemma(index, pagetitle, forms, save, verbose, program_args):
  def pagemsg(txt):
    msg("Page %s %s: %s" % (index, pagetitle, txt))

  pagemsg("Processing")

  def expand_text(tempcall):
    return blib.expand_text(tempcall, pagetitle, pagemsg, verbose)

  page = pywikibot.Page(site, pagetitle)
  parsed = blib.parse(page)
  for t in parsed.filter_templates():
    tname = unicode(t.name)
    tempcall = None
    if tname == "ru-conj":
      tempcall = re.sub(r"^\{\{ru-conj", "{{ru-generate-verb-forms",
          unicode(t))
    elif tname == "ru-noun-table":
      tempcall = re.sub(r"^\{\{ru-noun-table", "{{ru-generate-noun-args",
          unicode(t))
    elif tname == "ru-decl-adj":
      tempcall = re.sub(r"^\{\{ru-decl-adj", "{{ru-generate-adj-forms",
          unicode(t))
    if tempcall:
      result = expand_text(tempcall)
      if not result:
        pagemsg("WARNING: Error generating forms, skipping")
        continue
      args = ru.split_generate_args(result)

      for form in forms:
        if form in args:
          for formpagename in re.split(",", args[form]):
            formpagename = ru.remove_accents(re.sub("//.*", "", formpagename))
            formpage = pywikibot.Page(site, formpagename)
            if not formpage.exists():
              pagemsg("WARNING: Form page %s doesn't exist, skipping" % formpagename)
            elif formpagename == pagetitle:
              pagemsg("WARNING: Skipping dictionary form")
            else:
              process_page(index, formpage, save, verbose, program_args)

def read_pages(filename, start, end):
  lines = [x.strip() for x in codecs.open(filename, "r", "utf-8")]
  for i, line in blib.iter_items(lines, start, end):
    if line.startswith("#"):
      continue
    m = re.search(r"^\* Page ([0-9]+) \[\[(.*?)\]\]: ", line)
    if m:
      page = m.group(2)
    else:
      m = re.search(r"^Page ([0-9]+) (.*?): ", line)
      if m:
        page = m.group(2)
      else:
        page = line
    yield i, page

parser = blib.create_argparser("Add pronunciation sections to Russian Wiktionary entries")
parser.add_argument('--pagefile', help="File containing pages to process, one per line")
parser.add_argument('--lemma-file', help="File containing lemmas to process, one per line; non-lemma forms will be done")
parser.add_argument('--lemmas', help="List of comma-separated lemmas to process; non-lemma forms will be done")
parser.add_argument("--forms", help="Form codes of non-lemma forms to process in conjunction with --lemmas and --lemma-file.")
parser.add_argument('--tempfile', help="File containing templates and headwords for quick offline reprocessing, one per line")
parser.add_argument('--override-IPA', action="store_true", help="Change IPA to ru-IPA even when pronunciations can't be reconciled")
parser.add_argument('--override-pronun', action="store_true", help="Override existing pronunciations")
parser.add_argument('--cats', default="lemmas,nonlemmas", help="Categories to do ('lemmas', 'nonlemmas', an arbitrary category minus the 'Russian ' prefix, or comma-separated list)")
args = parser.parse_args()
start, end = blib.parse_start_end(args.start, args.end)

form_aliases = {
  "pres": [
    "pres_1sg", "pres_2sg", "pres_3sg", "pres_1pl", "pres_2pl", "pres_3pl"
  ],
  "futr": [
    "futr_1sg", "futr_2sg", "futr_3sg", "futr_1pl", "futr_2pl", "futr_3pl"
  ],
  "impr": ["impr_sg", "impr_pl"],
  "past": [
    "past_m", "past_f", "past_n", "past_pl",
    "past_m_short", "past_f_short", "past_n_short", "past_pl_short"
  ],
  "part": [
    "pres_actv_part", "past_actv_part", "pres_pasv_part", "past_pasv_part",
    "pres_adv_part", "past_adv_part", "past_adv_part_short"
  ],
  "all-verb": ["pres", "futr", "impr", "past", "part"],
  "sg": [
    "nom_sg", "gen_sg", "dat_sg", "acc_sg", "acc_sg_an", "acc_sg_in",
      "ins_sg", "pre_sg"
  ],
  "pl": [
    "nom_pl", "gen_pl", "dat_pl", "acc_pl", "acc_pl_an", "acc_pl_in",
      "ins_pl", "pre_pl"
  ],
  "all-noun": ["sg", "pl"],
  "long": [
    "nom_m", "nom_f", "nom_n", "nom_p", "nom_mp", "nom_fp",
    "gen_m", "gen_f", "gen_p", "gen_mp", "gen_fp",
    "dat_m", "dat_f", "dat_p", "dat_mp", "dat_fp",
    "acc_m_an", "acc_m_in", "acc_f", "acc_n", "acc_p_an", "acc_p_in",
    "acc_mp_an", "acc_mp_in", "acc_fp_an", "acc_fp_in", "acc_mp", "acc_fp",
    "ins_m", "ins_f", "ins_p", "ins_mp", "ins_fp",
    "pre_m", "pre_f", "pre_p", "pre_mp", "pre_fp",
  ],
  "short": ["short_m", "short_f", "short_n", "short_p"],
  "all-adj": ["long", "short"],
  "all": ["all-verb", "all-noun", "all-adj"]
}

def parse_form_aliases(forms):
  retval = []
  def parse_one_form(form):
    if form in form_aliases:
      for f in form_aliases[form]:
        parse_one_form(f)
    else:
      if form not in retval:
        retval.append(form)
  for form in re.split(",", forms):
    parse_one_form(form)
  return retval

if args.lemma_file or args.lemmas:
  forms = parse_form_aliases(args.forms)

  if args.lemma_file:
    lemmas = read_pages(args.lemma_file, start, end)
  else:
    lemmas = blib.iter_items(re.split(",", args.lemmas.decode("utf-8")), start, end)
  for i, lemma in lemmas:
    process_lemma(i, ru.remove_accents(lemma), forms, args.save, args.verbose, args)

elif args.pagefile:
  for i, page in read_pages(args.pagefile, start, end):
    msg("Page %s %s: Processing" % (i, page))
    process_page(i, pywikibot.Page(site, page), args.save, args.verbose, args)

elif args.tempfile:
  lines = [x.strip() for x in codecs.open(args.tempfile, "r", "utf-8")]
  for line in lines:
    m = re.search(r"^Page ([0-9]+) (.*?): .*Processing raw IPA (.*) for headword\(s\) (.*)$", line)
    if not m:
      msg("WARNING: Unrecognized line: %s" % line)
    else:
      index, pagetitle, ipa_templates, headwords = m.groups()
      ipa_templates = re.split(r"\+\+", ipa_templates)
      headwords = re.split(r"\+\+", headwords)
      headword_args = []
      for i in xrange(len(headwords)):
        if i == 0:
          headword_args.append(headwords[i])
        else:
          headword_args.append("head%s=%s" % (i+1, headwords[i]))
      text = """
==Russian==

%s

{{ru-noun|%s}}
""" % ("\n".join(ipa_templates), "|".join(headword_args))
      process_page_text(index, text, pagetitle, args.verbose, args)

else:
  categories = []
  for cattype in re.split(",", args.cats):
    if cattype == "nonlemmas":
      categories.append("Russian non-lemma forms")
    else:
      categories.append("Russian " + cattype)
  for category in categories:
    msg("Processing category: %s" % category)
    for i, page in blib.cat_articles(category, start, end):
      process_page(i, page, args.save, args.verbose, args)

def subval_to_string(subval):
  if type(subval) is tuple:
    pron, pre, post = subval
    return unicode(FoundPronun(pron, pre, post))
  else:
    return subval

for regex, subvals in manual_pronun_mapping:
  if regex not in applied_manual_pronun_mappings:
    msg("WARNING: Unapplied manual_pronun_mapping %s->%s" % (regex,
      ",".join(subval_to_string(x) for x in subvals) if type(subvals) is list
      else subval_to_string(subvals)))

blib.elapsed_time()
