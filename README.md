# culturaltranslationtool
Cultural Translation Tool (CTT)

I was inspired to write CTT when I worked out to back-translate in Homework Help Club to reach a "more" correct translation.  CTT gives better translations (and can be modified to give back translations, the translation back into the original language).  This tool can help businesses expand to other languages.  Requires Google Translate Shell and Grammarly (Premium) or MacVim on Mac (which I have tested it for).  If you back translate and you think the sentence either isn't grammatical or has the same meaning as the original, you can keep trying new variants, where the CTT saves the correct one.

Sentences are back-translated and saved in ctt-output.txt with the spaces and returns from the original.

NB. - Because of a limitation in Google Translate, CTT only improves, not perfects translations.  E.g. "I measured the dimensions of the object" is translated as "I measured the dimensions of the partner".  Additional verification is required.
- Sentences in texts given should not contain a full stop except at the end, but you can put them back in afterwards.
- Please remember that Google Translate translates by translating from English first.

Languages available
See <a href="https://github.com/soimort/translate-shell">trans</a>.


# Installation from List Prolog Package Manager (LPPM)

* Optionally, you can install from LPPM by installing <a href="https://www.swi-prolog.org/build/">SWI-Prolog</a> for your machine, downloading the <a href="https://github.com/luciangreen/List-Prolog-Package-Manager">LPPM Repository</a>, loading LPPM with `['lppm'].` then installing the package by running `lppm_install("luciangreen","culturaltranslationtool").`. Requires the additional parts below.


Installation

Copy the files into your chosen folder.

Install Translation Shell on Mac
Change line in ctt.pl
`concat_list(["../../../trans ",FromLang,":",ToLang," '",Input1,"'"],F),` to correct location of <a href="https://github.com/soimort/translate-shell">trans</a>.

You may need to install gawk using Homebrew.


Instructions

Enter the from and to language codes and the sentences to be translated in ctt-input.txt before running, e.g.:
[[en],[fr],[The second variable is equal to the first variable with one added. I adore you.]]

Load in SWI-Prolog using [ctt]. and [edit]., then run using ctt.

Follow the prompts asking for modifications to a possible back translation and whether a non-identical back translated sentence from the original is grammatical and has the same meaning as the original.  You can save time using CTT instead of Google Translate because CTT uses translations with identical back translations without asking.  Web site and document translations can be automated, using the saved back translations, where I recommend entering short sentences that are more likely to translate correctly and which you can reuse in other documents.  You can save the ctt-orig1-orig2.txt (original language from input to original language with the correct back translation) and ctt-orig-tran.txt (back translation to translation) files separately for a particular document for fast translation into multiple languages.

* Instructions for using Grammarly
NB. The algorithm will open Grammarly by itself when you need to enter a new suggestion to translate.  Import the file in the tmp folder.  Select text to correct as shown:
<img width="1171" alt="Screen Shot 2020-07-21 at 3 53 38 pm" src="https://user-images.githubusercontent.com/15845542/88051159-829e4000-cb9b-11ea-95a2-a7a4a01858ff.png">
Copy it here:
<img width="628" alt="Screen Shot 2020-07-21 at 3 53 57 pm" src="https://user-images.githubusercontent.com/15845542/88051166-88942100-cb9b-11ea-9e8a-e765eefcb58a.png">
Enter a new suggestion to translate:
<img width="634" alt="Screen Shot 2020-07-21 at 3 54 19 pm" src="https://user-images.githubusercontent.com/15845542/88051171-89c54e00-cb9b-11ea-96d3-69fe28bf4cdc.png">
Copy the contents into the file in BBEdit (ensure it is saved before the next step):
<img width="1149" alt="Screen Shot 2020-07-21 at 3 56 47 pm" src="https://user-images.githubusercontent.com/15845542/88051178-8c27a800-cb9b-11ea-97c4-d127db3f75a5.png">
Quit Grammarly:
<img width="32" alt="Screen Shot 2020-07-21 at 3 58 00 pm" src="https://user-images.githubusercontent.com/15845542/88051181-8df16b80-cb9b-11ea-9a26-5cab913ef960.png">

See <a href="https://github.com/luciangreen/culturaltranslationtool/edit/master/walkthrough.txt">walkthrough.txt</a>. (It is a different example.)
