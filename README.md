# Emacs Configuration

This repository contains my [Emacs][] configuration.  I thought I'd
share it with the rest of the world because I'm doing a few
interesting things:

  * As you can see, configuration is split among several files instead
    of having one giant configuration file.  More on this below.

  * All files are compiled into byte code for faster loading

## Configuration Organization

  * `bin`: Custom tools I use with Emacs.

  * `lisp`: Basic configuration files organized by function.

  * `modes`: One configuration file for each mode that I have
    customized.  These files are loaded automatically by Emacs, as
    needed.  See `lisp/modes.el` for more information.  This makes a
    big difference for Emacs start-up time.

  * `themes`: Custom color themes.

  * The `dot.emacs.el` file bootstraps all of this.

## Installation

What to try this configuration out without having to change your
existing configuartion in any way?

  1. Install the [Nix Package Manager](https://nixos.org/nix/)

  2. Run the following command:

         nix build

  3. Start Emacs:

         ./result/bin/emacs -q --load ./result/dot.emacs.el -f pjones:configure-new-frame

[emacs]: http://www.gnu.org/software/emacs/
