;;; webjump-conf.el -- Settings for `webjump' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'webjump)

(custom-set-variables
 '(webjump-use-internal-browser t)
 '(webjump-sites
   '(("DuckDuckGo" .
      [simple-query "duckduckgo.com"
		    "duckduckgo.com/?q=" ""])
     ("Google" .
      [simple-query "www.google.com"
                    "www.google.com/search?q=" ""])
     ("Google Scholar" .
      [simple-query "scholar.google.com"
                    "scholar.google.com/scholar?q=" ""])
     ("Hackage" .
      [simple-query "hackage.haskell.org"
                    "hackage.haskell.org/packages/search?terms=" ""])
     ("Hoogle" .
      [simple-query "hoogle.haskell.org"
                    "hoogle.haskell.org/?scope=set%3Astackage&hoogle=" ""])
     ("The Movie Database" .
      [simple-query "www.themoviedb.org"
                    "www.themoviedb.org/search?query=" ""])
     ("NixOS Options" .
      [simple-query "search.nixos.org/options"
                    "search.nixos.org/options?query=" ""])
     ("NixOS Packages" .
      [simple-query "search.nixos.org/packages"
                    "search.nixos.org/packages?query=" ""])
     ("YouTube" .
      [simple-query "www.youtube.com"
                    "www.youtube.com/results?search_query=" ""])
     ("Wikipedia" .
      [simple-query "en.wikipedia.org"
                    "en.wikipedia.org/w/index.php?search=" ""])
     ("Wiktionary" .
      [simple-query "en.wiktionary.org"
                    "en.wiktionary.org/w/index.php?search=" ""]))))

;;; webjump-conf.el ends here
