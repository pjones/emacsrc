// Load URLs from the command line in new buffers instead of new
// windows.
url_remoting_fn = load_url_in_new_buffer;

// Load download buffers in the background in the current window,
// instead of in new windows.
download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;

// Automatically follow unambiguous links
hints_auto_exit_delay = 500;
hints_ambiguous_auto_exit_delay = 0;

// Display properties of the current selected node during
// the hints interaction.
hints_display_url_panel = true;

// Default directory for downloads and shell commands.
cwd = get_home_directory();
cwd.append("download");

// Automatically handle some MIME types internally.
content_handlers.set("application/pdf", content_handler_save);

// External programs for handling various MIME types.
external_content_handlers.set("application/pdf", "xpdf");
//external_content_handlers.set("video/*", "urxvtc -e mplayer");

// External editor, Emacs of course.
editor_shell_command = "e -c --wait";

// View source in your editor.
view_source_use_external_editor = true;

// Auto-completion options when using find-url
url_completion_use_history = true;
url_completion_use_bookmarks = true;
url_completion_use_webjumps = true;
