// Load/reload user and agent CSS files
function reload_all_css () {
  var user = get_home_directory();
  user.appendRelativePath(".conkerorrc/user.css");
  unregister_user_stylesheet(make_uri(user));
  register_user_stylesheet(make_uri(user))
}

// Register it as an interactive command.
interactive("reload-all-css",
            "Reloads all of the user and agent CSS files.",
            function (I) {reload_all_css();});

// And call the function to load the CSS files.
reload_all_css();
