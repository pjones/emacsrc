function history_clear () {
  var history = Cc["@mozilla.org/browser/nav-history-service;1"]
    .getService(Ci.nsIBrowserHistory);
  history.removeAllPages();
}

interactive("history-clear", "Clear the history.", history_clear);


define_key(text_keymap, "C-w", "cmd_deleteWordBackward");
define_key(content_buffer_normal_keymap, "c", "copy", $browser_object = browser_object_dom_node);
