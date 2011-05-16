all: $(HOME)/.spamassassin $(HOME)/.spamassassin/user_prefs

$(HOME)/.spamassassin:
	mkdir -p $@

$(HOME)/.spamassassin/user_prefs: user_prefs
	cp -p $< $@
