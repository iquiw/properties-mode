# Properties Mode

[![CircleCI](https://circleci.com/gh/iquiw/properties-mode.svg?style=svg)](https://circleci.com/gh/iquiw/properties-mode)
[![Coverage Status](https://coveralls.io/repos/github/iquiw/properties-mode/badge.svg?branch=master)](<https://coveralls.io/github/iquiw/properties-mode?branch=master>)

## Overview

This is an enhancement of `conf-javaprop-mode`, focusing on Java language resource files.
It automatically decode unicode escape characters at load and encode back at save.

If Eldoc mode is enabled, it displays reference property value in minibuffer,
which is useful for translation.
For example, when editing "foo.bar" property on "message_ja.properties" file,
corresponding value in "message_en.properties" is displayed as Eldoc.

## Setup

## Configuration

With [use-package](https://github.com/jwiegley/use-package),

``` emacs-lisp
(use-package properties-mode
  :mode "\\.properties\\'")
```

### Customization

#### `properties-enable-auto-unicode-escape`

To disable automatic unicode escape conversion, set `properties-enable-auto-unicode-escape` to `nil`.

Default is `t`.

#### `properties-unicode-escape-uppercase`

To use uppercase characters in unicode escape sequence, set `properties-unicode-escape-uppercase` to `t`.

Default is `nil`.

#### `properties-reference-language`

Specify language name that is used as reference for translation.
It is used for language part of name of reference file, `<prefix>_<language>.<suffix>`.

Default is `en`, which means if one edits `message_ja.properties` then reference file is `message_en.properties`.

## Usage

### Key Bindings

| Key                | Function                               | Description                                                                                                  |
| ---                | ---                                    | ---                                                                                                          |
| <kbd>C-c C-d</kbd> | `properties-decode-buffer`             | Decode unicode escape characters in the current buffer.                                                      |
| <kbd>C-c C-e</kbd> | `properties-encode-buffer`             | Encode non-ASCII characters to unicode escape characters in the current buffer.                              |
| <kbd>C-c C-l</kbd> | `properties-change-reference-language` | Change reference language of the current buffer. With a prefix argument, change all properties-mode buffers. |
| <kbd>C-c C-v</kbd> | `properties-view-reference-file`       | View reference file using `display-buffer`.                                                                  |
