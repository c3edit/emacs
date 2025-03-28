# Set up multiple Emacs instances for testing.
test:
    # Use server for first instance
    emacsclient --eval '(load "./test.el")'
    # Start second instance.
    emacs -l ./test.el
