# GLSL Major Mode

This is a major mode for Emacs providing editing conveniences for the OpenGL
Shading Language (GLSL). It currently handles GLSL version 4.6, including many
recent Vulkan extensions.

In brief, it provides the following features:

- Syntax high-lighting for symbols and built-in functions and variables.

- Indentation for the current line (<kbd>TAB</kbd>) and selected region
  (<kbd>C-M-\\</kbd>).

- Fast switching between 'file.vert' and 'file.frag' with `ff-find-other-file`
  (<kbd>S-iso-lefttab</kbd>).

- Interactive function `glsl-find-man-page` prompts for a GLSL built-in
  functions which it formats to an `opengl.org` URL and finally passes that to
  `browse-url`.

- An initial tree-sitter implementation.

## Installation

The easiest way to install `glsl-mode` via [MELPA](https://melpa.org/), which
can be setup with the following snippet:

```
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
```

Once that is done, then just refresh the packages and install it with:

* <kbd>M-x package-refresh-contents</kbd>
* <kbd>M-x package-install glsl-mode</kbd>

Alternatively, you can also install the package using e.g.
[use-package](https://github.com/jwiegley/use-package) with the following
snippet in your `.emacs`:

```
(use-package glsl-mode
    :ensure t)
```

# Attributions and References

This major mode was originally authored by Xavier.Decoret@imag.fr and extended
by Jim Hourihan.

Original GLSL mode website:
(https://maverick.inria.fr/~Xavier.Decoret/resources/glsl-mode/index.html)

See [GLSL 4.6 Spec
(PDF)](https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.4.60.pdf)
the official reference document for GLSL 4.6.
