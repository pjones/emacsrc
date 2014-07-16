# Emacs Configuration

This repository contains my [Emacs] [] configuration.  I thought I'd
share it with the rest of the world because I'm doing a few
interesting things:

  * As you can see, configuration is split among several files instead
    of having one giant configuration file.  More on this below.

  * All files are compiled into byte code and installed in
    `~/.emacs.d` automatically using [GNU Make] [].

  * I'm slowly switching to the modern Emacs package system.  Some of
    the libraries I rely on are not yet available as packages so for
    those I have scripts and Makefiles for downloading and installing
    them.

  * Libraries that I use that are available as packages are
    automatically installed by Make.

## Configuration Organization

  * `bin`: Custom tools I use with Emacs.

  * `download`: Libraries that are not yet available as packages (or I
    haven't updated this directory yet) but are available as tarballs
    or source code repositories.  Automatically installed by Make.

  * `lisp`: Basic configuration files organized by function.

  * `modes`: One configuration file for each mode that I have
    customized.  These files are loaded automatically by Emacs, as
    needed.  See `lisp/modes.el` for more information.  This makes a
    big difference for Emacs start-up time.

  * `scripts`: Scripts I use for compiling the configuration, etc.

  * `themes`: Custom color themes.

  * `third-party`: Libraries acquired via the [Emacs Wiki] [] that may
    now be available as packages, but I haven't needed to update this
    repo yet.  I only plan on removing files from here as things break
    after Emacs upgrades or I need a new feature.

  * The `dot.emacs.el` file bootstraps all of this.

[emacs]: http://www.gnu.org/software/emacs/
[gnu make]: http://www.gnu.org/software/make/
[emacs wiki]: http://www.emacswiki.org/emacs/
