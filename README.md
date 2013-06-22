ruby-dev.el
===========

`ruby-dev.el` is a module that tries to bring some of SLIME's features to
Ruby. It provides:

  - Evaluation of code to a live process (C-x C-e, etc.)
  - Copying of results of the evaluated ruby code into buffers via the kill chain (preface eval commands with C-u)
  - Access to Ruby documentation (C-c C-d)
  - A wrapper around a Ruby REPL (C-c C-i)

For more details, see the help within Emacs.

Installation
------------

Add this directory to the load path and Something like this in your
configuration:

    (autoload 'turn-on-ruby-dev "ruby-dev" nil t)
    (add-hook 'ruby-mode-hook 'turn-on-ruby-dev)

For the server to start properly you need to install a couple of gems:

    gem install pry
    gem install yard
