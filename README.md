# Properties Mode

## Overview

This is an enhancement of `conf-mode`, focusing on Java language resource files.
It automatically decode unicode escape characters at load and encode back at save.

## Setup

## Configuration

With [use-package](https://github.com/jwiegley/use-package),

``` emacs-lisp
(use-package properties-mode
  :mode "\\.properties\\'")
```

To use uppercase characters in unicode escape sequence, set `properties-unicode-escape-uppercase` to `t`.
