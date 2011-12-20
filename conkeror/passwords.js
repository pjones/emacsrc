session_pref("signon.rememberSignons", true);
session_pref("signon.expireMasterPassword", false);
Cc["@mozilla.org/login-manager;1"].getService(Ci.nsILoginManager);
