#!/usr/bin/env ruby

################################################################################
require('rubygems')
require('appscript')

################################################################################
safari = Appscript.app('Safari.app')
url = safari.documents[1].get.URL.get

Appscript.app('Emacs.app').activate
Appscript.app("System Events.app").keystroke(url)
