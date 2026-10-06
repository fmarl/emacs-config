;;; config-elfeed.el --- My RSS reader config -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package elfeed
  :defer t
  :custom
  (elfeed-feeds
   '(("https://danluu.com/atom.xml" eng)
     ("https://jvns.ca/atom.xml" eng)
     ("https://rachelbythebay.com/w/atom.xml" eng ops)
     ("https://eli.thegreenplace.net/feeds/all.atom.xml" eng systems)
     ("https://antirez.com/rss" eng systems)
     ("https://matklad.github.io/feed.xml" eng systems)
     ("https://buttondown.com/hillelwayne/rss" eng)
     ("https://www.seangoedecke.com/rss.xml" eng)
     ("https://lucumr.pocoo.org/feed.atom" eng)
     ("https://samwho.dev/rss.xml" eng)
     ("https://fasterthanli.me/index.xml" eng systems)
     ("https://xeiaso.net/blog.rss" eng ops)
     ("https://www.scattered-thoughts.net/atom.xml" eng)
     ("https://brooker.co.za/blog/rss.xml" eng distsys)
     ("https://martin.kleppmann.com/feed.xml" eng distsys)
     ("https://aphyr.com/posts.atom" eng distsys)
     ("https://notes.eatonphil.com/rss.xml" eng systems)
     ("https://bernsteinbear.com/feed.xml" eng systems)
     ("https://nullprogram.com/feed/" eng systems emacs)
     ("https://blog.nelhage.com/atom.xml" eng systems)
     ("https://lemire.me/blog/feed/" eng perf)
     ("https://randomascii.wordpress.com/feed/" eng perf)
     ("https://devblogs.microsoft.com/oldnewthing/feed" eng systems)
     ("https://drewdevault.com/blog/index.xml" eng)
     ("https://mitchellh.com/feed.xml" eng)
     ("https://tonsky.me/atom.xml" eng)
     ("https://wingolog.org/feed/atom" eng systems)
     ("https://fabiensanglard.net/rss.xml" eng systems)
     ("https://ciechanow.ski/atom.xml" eng)
     ("https://martinfowler.com/feed.atom" eng)
     ("https://sachachua.com/blog/category/emacs-news/feed/" emacs)
     ("https://protesilaos.com/codelog.xml" emacs)
     ("https://blog.trailofbits.com/feed/" security research)
     ("https://soatok.blog/feed/" security crypto)
     ("https://www.schneier.com/feed/atom/" security crypto)
     ("https://portswigger.net/research/rss" security websec)
     ("https://googleonlinesecurity.blogspot.com/atom.xml" security)
     ("https://feeds.arstechnica.com/arstechnica/technology-lab" news)
     ("https://krebsonsecurity.com/feed/" news security)
     ("https://risky.biz/feeds/risky-business-news/" news security))))

(provide 'config-elfeed)
