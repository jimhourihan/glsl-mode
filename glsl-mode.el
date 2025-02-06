;;; glsl-mode.el --- Major mode for Open GLSL shader files -*- lexical-binding: t -*-

;; Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
;; Copyright (C) 2011, 2014, 2019 Jim Hourihan
;; Copyright (C) 2024 Gustaf Waldemarson
;;
;; Authors: Gustaf Waldemarson <gustaf.waldemarson ~at~ gmail.com>
;;          Jim Hourihan <jimhourihan ~at~ gmail.com>
;;          Xavier.Decoret@imag.fr,
;; Keywords: languages OpenGL GPU SPIR-V Vulkan
;; Version: 3.0
;; URL: https://github.com/jimhourihan/glsl-mode
;; Package-Requires: ((emacs "26.1"))
;;
;; Original URL: http://artis.inrialpes.fr/~Xavier.Decoret/resources/glsl-mode/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing GLSL files.  Is is based on c-mode plus some features
;; and pre-specified fontifications.
;;
;; Modifications from 2.5 of glsl-mode:
;;  * Moved most lists of GLSL syntax constants to separate file.
;;  * Numerous new constructs for modern GLSL and Vulkan.
;;  * glsl-mode is now a "proper" cc-mode, greatly improving the syntax
;;  * highlighting.
;;  * Initial work on a tree-sitter mode for GLSL.
;;
;; Modifications from the 1.0 version of glsl-mode (jimhourihan):
;;  * Removed original optimized regexps for font-lock-keywords and
;;    replaced with keyword lists for easier maintenance
;;  * Added customization group and faces
;;  * Preprocessor faces
;;  * Updated to GLSL 4.6
;;  * Separate deprecated symbols
;;  * Made _ part of a word
;;  * man page lookup at opengl.org

;;; Code:

(require 'cc-mode)
(require 'align)
(require 'glsl-db)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts)
  (require 'cl-lib)
  (require 'find-file))


(defgroup glsl nil
  "OpenGL Shading Language Major Mode."
  :group 'languages)

(defconst glsl-language-version "4.6"
  "GLSL language version number.")

(defconst glsl-version "4.6"
  "OpenGL major mode version number.")

(defvar glsl-mode-menu nil "Menu for GLSL mode.")

(defvar glsl-mode-hook nil "GLSL mode hook.")

(defvar glsl-extension-color "#A82848"
  "Color used for extension specifiers.")

(defvar glsl-extension-face 'glsl-extension-face)
(defface glsl-extension-face
  `((t (:foreground ,glsl-extension-color :weight bold)))
  "Custom face for GLSL extension."
  :group 'glsl)

(defvar glsl-shader-variable-name-face 'glsl-shader-variable-name-face)
(defface glsl-shader-variable-name-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "GLSL type face."
  :group 'glsl)

(defvar glsl-type-face 'glsl-type-face)
(defface glsl-type-face
  '((t (:inherit font-lock-type-face)))
  "GLSL type face."
  :group 'glsl)

(defvar glsl-builtins-face 'glsl-builtins-face)
(defface glsl-builtins-face
  '((t (:inherit font-lock-builtin-face)))
  "GLSL builtins face."
  :group 'glsl)

(defvar glsl-deprecated-builtins-face 'glsl-deprecated-builtins-face)
(defface glsl-deprecated-builtins-face
  '((t (:inherit font-lock-warning-face)))
  "GLSL deprecated builtins face."
  :group 'glsl)

(defvar glsl-qualifier-face 'glsl-qualifier-face)
(defface glsl-qualifier-face
  '((t (:inherit font-lock-keyword-face)))
  "GLSL qualifiers face."
  :group 'glsl)

(defvar glsl-keyword-face 'glsl-keyword-face)
(defface glsl-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "GLSL keyword face."
  :group 'glsl)

(defvar glsl-deprecated-keyword-face 'glsl-deprecated-keyword-face)
(defface glsl-deprecated-keyword-face
  '((t (:inherit font-lock-warning-face)))
  "GLSL deprecated keywords face."
  :group 'glsl)

(defvar glsl-variable-name-face 'glsl-variable-name-face)
(defface glsl-variable-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "GLSL variable face."
  :group 'glsl)

(defvar glsl-deprecated-variable-name-face 'glsl-deprecated-variable-name-face)
(defface glsl-deprecated-variable-name-face
  '((t (:inherit font-lock-warning-face)))
  "GLSL deprecated variable face."
  :group 'glsl)

(defvar glsl-reserved-keyword-face 'glsl-reserved-keyword-face)
(defface glsl-reserved-keyword-face
  '((t (:inherit glsl-keyword-face)))
  "GLSL reserved keyword face."
  :group 'glsl)

(defvar glsl-preprocessor-face 'glsl-preprocessor-face)
(defface glsl-preprocessor-face
  '((t (:inherit font-lock-preprocessor-face)))
  "GLSL preprocessor face."
  :group 'glsl)

(defcustom glsl-indent-offset 4
  "Number of spaces for each indentation step in `glsl-mode'and `glsl-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'glsl)

(defcustom glsl-additional-types nil
  "List of additional keywords to be considered types.

 These keywords are added to the `glsl-type-list' and are fontified
using the `glsl-type-face'.  Examples of existing types include
\"float\", \"vec4\", and \"int\"."
  :type '(repeat (string :tag "Type Name"))
  :group 'glsl)

(defcustom glsl-additional-qualifiers nil
  "List of additional keywords to be considered qualifiers.

 These are added to the `glsl-qualifier-list' and are fontified using
the `glsl-qualifier-face'.  Examples of existing qualifiers include
\"const\", \"in\", and \"out\"."
  :type '(repeat (string :tag "Qualifier Name"))
  :group 'glsl)

(defcustom glsl-additional-keywords nil
  "List of additional GLSL keywords.

 These are added to the `glsl-keyword-list' and are fontified using
the `glsl-keyword-face'.  Example existing keywords include
\"while\", \"if\", and \"return\"."
  :type '(repeat (string :tag "Keyword"))
  :group 'glsl)

(defcustom glsl-additional-built-ins nil
  "List of additional functions to be considered built-in.

These are added to the `glsl-builtins-list' and are fontified using
the `glsl-builtins-face'."
  :type '(repeat (string :tag "Keyword"))
  :group 'glsl)

(defvar glsl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map c-mode-map)
    (define-key map [S-iso-lefttab] 'ff-find-other-file)
    map)
  "Keymap for GLSL major mode.")

(defcustom glsl-browse-url-function #'browse-url
  "Function used to display GLSL man pages.

E.g. the function used by calls to 'browse-url', eww, w3m, etc."
  :type 'function
  :group 'glsl)

(defcustom glsl-man-pages-base-url "http://www.opengl.org/sdk/docs/man/html/"
  "Location of GL man pages."
  :type 'string
  :group 'glsl)

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.tesc\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.tese\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.mesh\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.task\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.rgen\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.rint\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.rchit\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.rahit\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.rcall\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.rmiss\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode)))

(defvar glsl--type-rx (regexp-opt glsl-type-list 'symbols))
(defvar glsl--deprecated-keywords-rx (regexp-opt glsl-deprecated-qualifier-list 'symbols))
(defvar glsl--reserved-keywords-rx (regexp-opt glsl-reserved-list 'symbols))
(defvar glsl--keywords-rx (regexp-opt glsl-keyword-list 'symbols))
(defvar glsl--qualifier-rx (regexp-opt glsl-qualifier-list 'symbols))
(defvar glsl--deprecated-builtins-rx (regexp-opt glsl-deprecated-builtins-list 'symbols))
(defvar glsl--builtins-rx (regexp-opt glsl-all-shader-builtins 'symbols))
(defvar glsl--deprecated-variables-rx (regexp-opt glsl-deprecated-variables-list 'symbols))
(defvar glsl--constants-rx (regexp-opt glsl-all-shader-constants 'symbols))
(defvar glsl--variables-rx (regexp-opt glsl-all-shader-variables 'symbols))
(defvar glsl--extensions-rx
  (rx (group-n 1 "#extension")
      (+ (in space))
      (group-n 2 (seq "GL_" (+ (in "A-Z")) "_" (in "A-Za-z") (+ (in "A-Za-z0-9_"))))
      (* (in space)) ":" (* (in space))
      (or (group-n 3 (or "require" "enable"))
          (group-n 4 (or "warn" "disable")))))



(eval-and-compile (c-add-language 'glsl-mode 'c-mode))


(c-lang-defconst c-primitive-type-kwds
  "Primitive type keywords.  As opposed to the other keyword lists, the
keywords listed here are fontified with the type face instead of the
keyword face.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Do not try to modify this list for end user customizations; the
`*-font-lock-extra-types' variable, where `*' is the mode prefix, is
the appropriate place for that."
  glsl
  (append
   glsl-type-list
   ;; Use append to not be destructive on the return value below.
   (c-lang-const c-primitive-type-kwds c)))

(c-lang-defconst c-constant-kwds
  "Keywords for constants."
  glsl (append (c-lang-const c-constant-kwds c)))

(c-lang-defconst c-modifier-kwds
  glsl
  (append (c-lang-const c-modifier-kwds)
          '("in" "out" "inout" "uniform" "buffer"
            "coherent" "readonly" "writeonly"
            "hitAttributeEXT" "rayPayloadEXT" "rayPayloadInEXT")))

(c-lang-defconst c-paren-nontype-kwds
  "Keywords that may be followed by a parenthesis expression."
  glsl    '("layout"))

(defconst glsl-font-lock-keywords-1 (c-lang-const c-matchers-1 glsl))
(defconst glsl-font-lock-keywords-2 (c-lang-const c-matchers-2 glsl))
(defconst glsl-font-lock-keywords-3 (c-lang-const c-matchers-3 glsl))
(defvar glsl-font-lock-keywords (c-lang-const c-matchers-3 glsl))
(defun glsl-font-lock-keywords ()
  "Compose a list of font-locking keywords."
  (c-compose-keywords-list glsl-font-lock-keywords))


(defvar glsl-mode-syntax-table
  (let ((tbl (make-syntax-table c-mode-syntax-table)))
    tbl)
  "Syntax table for glsl-mode.")

(defvar glsl-other-file-alist
  '(("\\.frag$" (".vert"))
    ("\\.vert$" (".frag")))
  "Alist of extensions to find given the current file's extension.")

(defun glsl-man-completion-list ()
  "Return list of all GLSL keywords."
  (append glsl-builtins-list glsl-deprecated-builtins-list))

(defun glsl-find-man-page (thing)
  "Collects and displays manual entry for GLSL built-in function THING."
  (interactive
   (let ((word (current-word nil t)))
     (list
      (completing-read
       (concat "OpenGL.org GLSL man page: (" word "): ")
       (glsl-man-completion-list)
       nil nil nil nil word))))
  (save-excursion
    (apply glsl-browse-url-function
           (list (concat glsl-man-pages-base-url thing ".xhtml")))))

(easy-menu-define glsl-menu glsl-mode-map
  "GLSL Menu."
    `("GLSL"
      ["Comment Out Region"     comment-region
       (c-fn-region-is-active-p)]
      ["Uncomment Region"       (comment-region (region-beginning)
						(region-end) '(4))
       (c-fn-region-is-active-p)]
      ["Indent Expression"      c-indent-exp
       (memq (char-after) '(?\( ?\[ ?\{))]
      ["Indent Line or Region"  c-indent-line-or-region t]
      ["Fill Comment Paragraph" c-fill-paragraph t]
      "----"
      ["Backward Statement"     c-beginning-of-statement t]
      ["Forward Statement"      c-end-of-statement t]
      "----"
      ["Up Conditional"         c-up-conditional t]
      ["Backward Conditional"   c-backward-conditional t]
      ["Forward Conditional"    c-forward-conditional t]
      "----"
      ["Backslashify"           c-backslash-region (c-fn-region-is-active-p)]
      "----"
      ["Find GLSL Man Page"  glsl-find-man-page t]))

;;;###autoload
(define-derived-mode glsl-mode prog-mode "GLSL"
  "Major mode for editing GLSL shader files.

\\{glsl-mode-map}"
  :syntax-table glsl-mode-syntax-table
  :after-hook (progn (c-make-noise-macro-regexps)
		     (c-make-macro-with-semi-re)
		     (c-update-modeline))
  (c-initialize-cc-mode t)
  (c-init-language-vars glsl-mode)
  (c-common-init 'glsl-mode)
  (cc-imenu-init cc-imenu-c++-generic-expression)

  (c-run-mode-hooks 'c-mode-common-hook)
  (run-mode-hooks 'glsl-mode-hook)

  (setq-local abbrev-mode t)
  (setq-local ff-other-file-alist 'glsl-other-file-alist)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-padding "")
  (add-to-list 'align-c++-modes 'glsl-mode)

  (font-lock-add-keywords
   nil
   `((,glsl--deprecated-builtins-rx  . glsl-deprecated-builtins-face)
     (,glsl--builtins-rx             . glsl-builtins-face)
     (,glsl--deprecated-variables-rx . glsl-deprecated-variable-name-face)
     (,glsl--variables-rx            . glsl-shader-variable-name-face)
     (,glsl--constants-rx            . font-lock-constant-face)
     (,glsl--deprecated-keywords-rx  . glsl-deprecated-keyword-face)
     (,glsl--reserved-keywords-rx    . glsl-reserved-keyword-face)
     (,glsl--extensions-rx (2 'glsl-extension-face nil lax)
                       (3 '(face font-lock-keyword-face) nil lax)
                       (4 '(face font-lock-warning-face) nil lax))))

  (let* ((rx-extra '((glsl-additional-keywords   . glsl-keyword-face)
                     (glsl-additional-qualifiers . glsl-qualifer-face)
                     (glsl-additional-built-ins  . glsl-builtins-face)))
         (fl-extras (cl-loop for (key . value) in rx-extra
                             when (eval key)
                             collect (cons (regexp-opt (eval key) 'symbol) value))))
    (font-lock-add-keywords nil fl-extras)))


(provide 'glsl-mode)

;;; glsl-mode.el ends here
