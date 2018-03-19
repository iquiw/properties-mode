#! /bin/sh

cask emacs -Q -L . -l properties-mode-tests.el --batch -f ert-run-tests-batch-and-exit
