# culturaltranslationtool
Cultural Translation Tool (CTT)

I was inspired to write CTT when I worked out to back-translate in Homework Help Club to reach a "more" correct translation.  CTT gives better translations (and can be modified to give back translations, the translation back into the original language).  This tool can help businesses expand to other languages.  Requires Translate Shell.  If you back translate and you think the sentence either isn't grammatical or has the same meaning as the original, you can keep trying new variants, where the CTT saves the correct one.

Sentences are back-translated and saved in ctt-output.txt with the spaces and returns from the original.

NB. - Because of a limitation in Google Translate, CTT only improves, not perfects translations.  E.g. "I measured the dimensions of the object" is translated as "I measured the dimensions of the partner".  Additional verification is required.
- Sentences in texts given should not contain a full stop except at the end, but you can put them back in afterwards.
- Please remember that Google Translate translates by translating from English first.

# Prerequisites

* Please download and install SWI-Prolog for your machine at `https://www.swi-prolog.org/build/`.

* You may need to install gawk using Homebrew.

* Install <a href="https://github.com/soimort/translate-shell">Translation Shell</a> on Mac, etc.
Change line in
```
culturaltranslationtool/ctt2.pl
trans_location("../../../gawk/trans").
```
to correct location of <a href="https://github.com/soimort/translate-shell">trans</a>.

# 1. Install manually

Download <a href="http://github.com/luciangreen/culturaltranslationtool/">this repository</a> and <a href="http://github.com/luciangreen/listprologinterpreter/">List Prolog Interpreter</a>.

# 2. Or Install from List Prolog Package Manager (LPPM)

* Download the <a href="https://github.com/luciangreen/List-Prolog-Package-Manager">LPPM Repository</a>:

```
mkdir GitHub
cd GitHub/
git clone https://github.com/luciangreen/List-Prolog-Package-Manager.git
cd List-Prolog-Package-Manager
swipl
['lppm'].
lppm_install("luciangreen","culturaltranslationtool").
../
halt.
```

# Running

* In Shell:
`cd culturaltranslationtool`
`swipl`

Instructions

Enter the from and to language codes and the sentences to be translated in `ctt-input.txt` before running, e.g.:
`[["en"],["fr"],["The second variable is equal to the first variable with one added. I adore you."]]`

Load in SWI-Prolog using `['../listprologinterpreter/listprolog.pl'].`, then run using `ctt2.`.

Follow the prompts asking for modifications to a possible back translation and whether a non-identical back translated sentence from the original is grammatical and has the same meaning as the original.  You can save time using CTT instead of Google Translate because CTT uses translations with identical back translations without asking.  Web site and document translations can be automated, using the saved back translations, where I recommend entering short sentences that are more likely to translate correctly and which you can reuse in other documents.  You can save the `ctt-orig1-orig2.txt` (original language from input to original language with the correct back translation) and `ctt-orig-tran.txt` (back translation to translation) files separately for a particular document for fast translation into multiple languages.

