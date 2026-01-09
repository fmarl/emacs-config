(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
	'(("https://feeds.arstechnica.com/arstechnica/technology-lab" news)
	  ("https://bleepingcomputer.com/feed/" news)
	  ("https://googleonlinesecurity.blogspot.com/atom.xml" news google)
	  ;;("https://googleprojectzero.blogspot.com/feeds/posts/default" news google)
	  ("https://bughunters.google.com/feed/en" google)
	  "https://www.grahamcluley.com/feed/"
	  "https://jfrog.com/blog/feed"
	  "https://krebsonsecurity.com/feed/"
	  "https://www.schneier.com/feed/atom/"
	  "https://feeds.feedburner.com/TroyHunt"
	  ("https://feeds.feedburner.com/TheHackersNews" news)
	  ("https://lobste.rs/top/rss" tech)
	  "https://risky.biz/feeds/risky-business/"
	  "https://risky.biz/feeds/risky-business-news/"
	  ("https://www.theregister.com/security/research/headlines.atom" news)
	  ("https://www.theregister.com/security/patches/headlines.atom" news)
	  ("https://www.theregister.com/security/cyber_crime/headlines.atom" news)
	  ("https://www.darkreading.com/rss.xml" news)
	  ("https://soatok.blog/feed/" crypto)
	  ("https://portswigger.net/research/rss" vuln)
	  ("https://ngailong.wordpress.com/feed/" vuln)
	  ("https://edoverflow.com/index.xml" vuln)
	  ("https://a13xp0p0v.github.io/feed" vuln)
	  ("https://blog.exodusintel.com/feed/" vuln)
	  ("https://blog.isosceles.com/rss/" vuln)
	  ("https://connormcgarr.github.io/feed.xml" vuln)
	  ("https://blog.pi3.com.pl/?feed=rss2" vuln)
	  ("https://www.matteomalvica.com/index.xml" vuln)
	  ("https://blog.badsectorlabs.com/feeds/all.atom.xml" vuln)
	  ("https://fareedfauzi.github.io/feed" vuln))))
