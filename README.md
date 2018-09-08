# Submode

Utilities for a major mode to manage multiple major modes as submodes
in the same buffer.

Two main functions are introduced `submode-construct-submode`
and `submode-construct-main-mode`.

## Submode-construct-submode

Constructs a submode from a major mode with a number of keywords
specifying how the major mode will be applied and operate as a submode.

Example:

```elisp
 (defconst *--the-major-mode-as-submode
   (submode-construct-submode
    'the-major-mode
    :name "(The Major Mode)"
    :relative-indent 'block
    :syntax-propertize-rules
    (syntax-propertize-precompile-rules
     ("<start-tag>"
      (0 (ignore
          (goto-char (match-end 0))
          (submode-syntax-propertize *--the-major-mode-as-submode end)))))
    :end-tag "<end-tag>"
    :syntax-table the-major-mode-syntax-table
    :propertize #'the-major-mode-syntax-propertize
    :keymap the-major-mode-map))
```

The function will apply the mode and capture buffer-local variables.

## Submode-construct-main-mode

Constructs a main-mode with a colletion of submodes and a number of
keywords specifying how submodes will be applied and indented etc.

This function is supposed to be called in the corresponding major
mode hook to activate the major mode as main mode.

Example:

```elisp
 (add-hook 'the-main-major-mode-hook
  (lambda ()
    (submode-construct-main-mode
     :name '("The Main Major Mode" (:eval (submode-lighter)))
     :indent-basic c-basic-offset
     :submodes '(*--the-major-mode-as-submode))))
```

# Acknowledgments

This library is inspired from the mhtml-mode introduced in Emacs 26.1.
