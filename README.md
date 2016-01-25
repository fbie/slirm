# Systematic Literature Review Mode for Emacs #

SLIRM is a little tool for annotating entries in BibTeX files and is
designed for performing systematic literature reviews. Everything is
happening in plain-text BibTeX files, such that you can easily work
together and synchronize via Git or the like.

## Installation ##

### MELPA ###

[![MELPA](https://melpa.org/packages/slirm-badge.svg)](https://melpa.org/#/slirm)

SLIRM is available on [MELPA](http://melpa.org)!

### Manually ###

To install SLIRM manually, clone this repository (e.g. into ```~/slirm```):

```
$ cd
$ git clone git@github.com:fbie/slirm.git
 Cloning into 'slirm'...
 Checking connectivity... done.
$ ls slirm/
 README.md slirm.el
```

Then, add the following or similar to your ```.emacs``` file:

```
(add-to-list 'load-path "~/slirm/")
(load "slirm.el")

(require 'slirm)
```

## Usage ##

Open a BibTeX file with ```C-x C-f```. From within the new buffer, run
```M-x slirm-start RET```. The BibTeX buffer will be replaced with a SLIRM
buffer, which shows the first not yet reviewed entry.

Press ```SPC``` and enter *yes* or *no* to keep or reject the
entry. You will automatically proceed to the next first not yet
reviewed entry.

Further navigation consists of:

- ```n```: Next entry
- ```p```: Previous entry
- ```C-n```: Next not yet reviewed entry
- ```C-f```: First not yet reviewed entry
- ```C-c C-t```: Show full text PDF. This might download the file and cache it locally.
- ```C-c C-n```: Edit notes in the mini-buffer.
- ```C-c C-u```: Browse to the current entry's URL.

## Extending URL Handlers ##

SLIRM automatically downloads the abstract and link to the full text
PDF, if available. This is done by just scraping the HTML file
returned by getting the URL stored in the BibTeX ```url```
field. Currently, there are only handlers for ACM, but you can add
more handlers, one handler for the abstract and one for the full text
link similarly to this:

```
(slirm-add-handlers "acm.org" 'slirm--acm-get-links 'slirm--acm-get-abstract)
```

```slirm--acm-get-links``` returns a pair of URLs, where the ```car```
is the link to the abstract and ```cdr``` the link to the full-text
file. It may be necessary to generalize this a bit further, such that
the handler itself writes the values to the current entry.

## TODOs ##

- [x] Download and cache full text PDF.
- [x] Export accepted entries to new file.
- [x] Default handlers for unknown base URLs to prevent crashing.
- [x] Edit annotations.
- [ ] Extend navigation to next/previous/first accepted/rejected entry.
- [ ] Add options for auto-commit with Magit.
- [x] Run ```slirm-start``` with filename as parameter.
- [ ] Generalize downloading of full-text URLs and abstracts.
