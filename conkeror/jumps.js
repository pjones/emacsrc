// Search the EmacsWiki
define_webjump("emacswiki",
               "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi" +
               "&q=%s&sa=Search&siteurl=emacswiki.org%2F",
               $alternative="http://www.emacswiki.org/");

// Google Images Search
define_webjump("images", "http://www.google.com/images?q=%s",
               $alternative = "http://www.google.com/imghp");

// Check Weather
define_webjump("weather", "http://www.wunderground.com/" +
               "cgi-bin/findweather/getForecast?query=%s");

// Ping Feedburner
define_webjump("pingfb", "http://feedburner.google.com/fb/a/" +
               "pingSubmit?bloglink=http%3A%2F%2F" +
               "feeds.feedburner.com%2Fdevalot-all");

// Amazon MP3 Store
define_webjump("amazon-mp3", "http://www.amazon.com/exec/obidos/external-search" +
               "/?url=search-alias%3Ddigital-music&field-keywords=%s");

// Translation
define_webjump("it-en", "http://translate.google.com/#it|en|%s");
define_webjump("en-it", "http://translate.google.com/#en|it|%s");

// Searching Hackage via Google
define_webjump("hackage", "http://www.google.co.uk/search?hl=en&" +
               "as_sitesearch=hackage.haskell.org/package&as_q=%s");

// Other search engines
define_webjump("hoogle", "http://haskell.org/hoogle/?hoogle=%s");
define_webjump("duckduckgo", "http://duckduckgo.com/?q=%s");
define_webjump("github", "http://github.com/search?q=%s&type=Everything");
define_webjump("imdb", "http://www.imdb.com/find?s=all&q=%s");
