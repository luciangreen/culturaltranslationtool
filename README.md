# Cultural Translation Tool (CTT)

* CTT allows translating folders and files of text and Prolog files from one language to another. It is used only with files without filenames as strings in Prolog, so if you are translating an algorithm, use it with individual files that contain the text from your algorithm.
* Requires ChatGPT API key (from the ChatGPT website) in `Text-to-Breasonings/chatgpt_qa_key.txt`.

# Prerequisites

* Use a search engine to find the Homebrew (or other) Terminal install command for your platform and install it, and search for the Terminal command to install swipl using Homebrew and install it or download and install SWI-Prolog for your machine at <a href="https://www.swi-prolog.org/build/">SWI-Prolog</a>.

# Mac, Linux and Windows (with Linux commands installed): Prepare to run swipl

* In Terminal settings (Mac), make Bash the default shell:

```
/bin/bash
```

* In Terminal, edit the text file `~/.bashrc` using the text editor Nano:

```
nano ~/.bashrc
```

* Add the following to the file `~/.bashrc`:

```
export PATH="$PATH:/opt/homebrew/bin/"
```

* Link to swipl in Terminal:

```
sudo ln -s /opt/homebrew/bin/swipl /usr/local/bin/swipl
```

# 1. Install manually

Download <a href="http://github.com/luciangreen/culturaltranslationtool/">this repository</a> and repositories listed in `List-Prolog-Package-Manager/lppm_registry.txt` under `"Cultural Translation Tool"`.

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
`['ctt3.pl'].`
`ctt3("source/", "destination/", "English", "French").`

* where `"source/"` is the source directory or file, `"destination/"` is the destination directory or file, `"English"` is the original language (optional) and `"French"` is the destination language.

