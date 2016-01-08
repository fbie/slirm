# Systematic Literature Review Mode for Emacs #

SLIRM is a little tool for annotating entries in BibTeX files and is
designed for performing systematic literature reviews. Everything is
happening in plain-text BibTeX files, such that you can easily work
together and synchronize via Git or the like.

## Usage ##

To use SLIRM, add the following or similar to your ```.emacs``` file:

```
(add-to-list 'load-path "~/slirm/")
(load "slirm.el")

(require 'slirm)
```

Now open a BibTeX file with ```C-x f```. From within the new buffer,
run ```M-x slirm-start```. The BibTeX buffer will be replaced with a
SLIRM buffer, which shows the first not yet reviewed entry.

Press ```SPC``` and enter *yes* or *no* to keep or reject the
entry. You will automatically proceed to the next first not yet
reviewed entry.

Further navigation consists of:

- ```n```: Next entry
- ```p```: Previous entry
- ```C-n```: Next not yet reviewed entry
- ```C-f```: First not yet reviewed entry
- ```C-c C-t```: Show full text PDF. This might download the file and cache it locally.

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

## TODOs ##

- [x] Download and cache full text PDF.
- [ ] Export accepted entries to new file.
- [x] Default handlers for unknown base URLs to prevent crashing.
- [ ] Edit annotations.
- [ ] Extend navigation to next/previous/first accepted/rejected entry.
- [ ] Add options for auto-commit with Magit.
