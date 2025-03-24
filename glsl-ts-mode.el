;;; glsl-ts-mode.el --- Major mode for GLSL shaders using tree-sitter -*- lexical-binding: t -*-

;; Copyright (C) 2024 Gustaf Waldemarson
;;
;; Authors: Gustaf Waldemarson <gustaf.waldemarson ~at~ gmail.com>
;; Keywords: languages OpenGL GPU SPIR-V Vulkan
;; Version: 1.0
;; URL: https://github.com/jimhourihan/glsl-mode
;; Package-Requires: ((emacs "29"))
;;

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

;; Major mode for editing OpenGL GLSL shader files using Tree-sitter.

;;; Code:

(require 'cc-mode)
(require 'treesit)
(require 'c-ts-mode)
(require 'glsl-mode)


(defcustom glsl-ts-all-shader-variables t
  "Always highlight all shader variables."
  :type 'boolean
  :safe 'booleanp
  :group 'glsl)

(defcustom glsl-ts-all-shader-constants t
  "Always highlight all shader constants."
  :type 'boolean
  :safe 'booleanp
  :group 'glsl)

(defcustom glsl-ts-all-shader-builtins t
  "Always highlight all shader builtins."
  :type 'boolean
  :safe 'booleanp
  :group 'glsl)

(defcustom glsl-ts-indent-style t
  "Style to use for indentation.

This style is passed directly to the "
  :type '(choice (symbol :tag "Gnu" gnu)
                 (symbol :tag "K&R" k&r)
                 (symbol :tag "Linux" linux)
                 (symbol :tag "BSD" bsd)
                 (function :tag "A function for user customized style" ignore))
  :set #'c-ts-mode--indent-style-setter
  :safe 'c-ts-indent-style-safep
  :group 'glsl)

;; TODO: Add Keyword "discard" to GLSL grammar.
(defvar glsl-ts-keywords
  '("break" "continue" "do" "for" "while" "if" "else" ;; "discard"
    "subroutine" "return" "switch" "default" "case")
  "Keywords that shoud be high-lighted.")


(defun glsl-ts--shader-constants (shader-type)
  "Create a list of special variables and constants for SHADER-TYPE."
  (pcase (if glsl-ts-all-shader-constants :all shader-type)
    (:vert  (append glsl-common-shader-constants))
    (:frag  (append glsl-common-shader-constants))
    (:geom  (append glsl-common-shader-constants))
    (:tesc  (append glsl-common-shader-constants))
    (:tese  (append glsl-common-shader-constants))
    (:mesh  (append glsl-common-shader-constants))
    (:task  (append glsl-common-shader-constants))
    (:comp  (append glsl-common-shader-constants))
    (:rgen  (append glsl-common-shader-constants glsl-ray-tracing-shader-constants))
    (:rint  (append glsl-common-shader-constants glsl-ray-tracing-shader-constants))
    (:rchit (append glsl-common-shader-constants glsl-ray-tracing-shader-constants))
    (:rahit (append glsl-common-shader-constants glsl-ray-tracing-shader-constants))
    (:rcall (append glsl-common-shader-constants glsl-ray-tracing-shader-constants))
    (:rmiss (append glsl-common-shader-constants glsl-ray-tracing-shader-constants))
    (:all   (append glsl-all-shader-constants))
    (_ nil)))

(defun glsl-ts--shader-variables (shader-type)
  "Create a list of special variables and constants for SHADER-TYPE."
  (pcase (if glsl-ts-all-shader-variables :all shader-type)
    (:vert  (append glsl-vertex-shader-variables))
    (:frag  (append glsl-fragment-shader-variables))
    (:geom  (append glsl-geometry-shader-variables))
    (:tesc  (append glsl-tesellation-control-shader-variables))
    (:tese  (append glsl-tesellation-evaluation-shader-variables))
    (:mesh  (append glsl-mesh-shader-variables))
    (:task  (append glsl-task-shader-variables))
    (:comp  (append glsl-compute-shader-variables))
    (:rgen  (append glsl-ray-tracing-ray-gen-shader-variables))
    (:rint  (append glsl-ray-tracing-intersection-shader-variables))
    (:rchit (append glsl-ray-tracing-closest-hit-shader-variables))
    (:rahit (append glsl-ray-tracing-any-hit-shader-variables))
    (:rcall (append glsl-ray-tracing-callable-shader-variables))
    (:rmiss (append glsl-ray-tracing-miss-shader-variables))
    (:all   (append glsl-all-shader-variables))
    (_ nil)))

(defun glsl-ts--shader-builtins (shader-type)
  "Create a list of shader builtin functions for SHADER-TYPE."
  (pcase (if glsl-ts-all-shader-builtins :all shader-type)
    (:vert  (append glsl-builtins-list))
    (:frag  (append glsl-builtins-list))
    (:geom  (append glsl-builtins-list))
    (:tesc  (append glsl-builtins-list))
    (:tese  (append glsl-builtins-list))
    (:mesh  (append glsl-builtins-list glsl-mesh-builtins))
    (:task  (append glsl-builtins-list glsl-mesh-builtins))
    (:comp  (append glsl-builtins-list))
    (:rgen  (append glsl-builtins-list glsl-ray-tracing-builtins))
    (:rint  (append glsl-builtins-list glsl-ray-tracing-builtins))
    (:rchit (append glsl-builtins-list glsl-ray-tracing-builtins))
    (:rahit (append glsl-builtins-list glsl-ray-tracing-builtins))
    (:rcall (append glsl-builtins-list glsl-ray-tracing-builtins))
    (:rmiss (append glsl-builtins-list glsl-ray-tracing-builtins))
    (:all   (append glsl-all-shader-builtins))
    (_ nil)))

(defun glsl-ts-font-lock-rules (shader-type)
  "Generate tree-sitter font-locking rules for the given SHADER-TYPE."
  `(:language glsl
    :feature comment
    ((comment) @font-lock-comment-face)

    :language glsl
    :feature preprocessor
    ;; TODO: Probably want to extend the GLSL grammar with a dedicated rule for
    ;; extension specifications.
    ((preproc_call (preproc_directive) @glsl-preprocessor-face
                   ((preproc_arg) @glsl-extension-face))
     (preproc_ifdef "#ifndef" @glsl-preprocessor-face
                    name: ((identifier) @font-lock-variable-name-face))
     (["#endif"] @glsl-preprocessor-face)
     (preproc_def "#define" @glsl-preprocessor-face
                  name: ((identifier) @font-lock-variable-name-face))
     (preproc_function_def "#define" @glsl-preprocessor-face
                           name: ((identifier) @font-lock-function-name-face))
     (preproc_include "#include" @glsl-preprocessor-face
                      ((string_literal) @font-lock-string-face))
     (preproc_extension (preproc_directive) @glsl-preprocessor-face
                        extension: (identifier) @glsl-extension-face
                        ((extension_behavior) @font-lock-keyword-face
                         (:match "require\\|enable" @font-lock-keyword-face)))
     (preproc_extension (preproc_directive) @glsl-preprocessor-face
                        extension: (identifier) @glsl-extension-face
                        ((extension_behavior) @font-lock-warning-face
                         (:match "warn\\|disable" @font-lock-warning-face)))
     (preproc_params
      (identifier) @font-lock-variable-name-face)
     (preproc_defined
      "defined" @glsl-preprocessor-face
      "(" @glsl-preprocessor-face
      (identifier) @font-lock-variable-name-face
      ")" @glsl-preprocessor-face))

    :language glsl
    :feature definition
    ((function_declarator declarator: (_) @font-lock-function-name-face)
     (struct_specifier "struct" @font-lock-keyword-face)
     (declaration (layout_specification "layout" @glsl-qualifier-face)
                  ["buffer" @font-lock-keyword-face
                   "uniform" @font-lock-keyword-face]
                  (identifier) @font-lock-variable-name-face)
     (declaration (layout_specification "layout" @glsl-qualifier-face)
                  (extension_storage_class) @font-lock-keyword-face
                  (identifier) @font-lock-variable-name-face)
     (declaration (layout_specification "layout" @glsl-qualifier-face)
                  ["in" @font-lock-keyword-face
                   "out" @font-lock-keyword-face]
                  (identifier) @font-lock-variable-name-face)
     (declaration (layout_specification "layout" @glsl-qualifier-face))
     (declaration (extension_storage_class ["hitAttributeEXT"] @glsl-qualifier-face))
     (declaration type: (_) declarator: (identifier) @font-lock-variable-name-face)
     (init_declarator declarator: (identifier) @font-lock-variable-name-face)
     (parameter_declaration type: (_) declarator: (identifier) @font-lock-variable-name-face)
     (parameter_declaration (["in" "out" "inout"] @font-lock-keyword-face)
                            type: (_) declarator: (identifier) @font-lock-variable-name-face)
     (field_declaration type: (_)
                        declarator: [(field_identifier) @font-lock-variable-name-face
                                     (array_declarator declarator: (field_identifier) @font-lock-variable-name-face)])
     (array_declarator declarator: (identifier) @font-lock-variable-name-face)
     (call_expression function:
                      ((identifier) @font-lock-type-face
                       (:match ,(rx-to-string `(seq bol (or ,@glsl-type-list) eol)) @font-lock-type-face))))

    :feature keyword
    :language glsl
    ([,@glsl-ts-keywords] @font-lock-keyword-face)

    :feature builtin
    :language glsl
    (((identifier) @font-lock-builtin-face
      (:match ,(rx-to-string `(seq bol (or ,@(glsl-ts--shader-builtins shader-type)) eol))
              @font-lock-builtin-face)))

    :language glsl
    :feature qualifier
    (((type_qualifier) @font-lock-keyword-face))

    :language glsl
    :feature type
    (((primitive_type) @font-lock-type-face)
     ((type_identifier) @font-lock-type-face))

    :language glsl
    :feature constant
    (((identifier) @font-lock-constant-face
      (:match ,(rx-to-string `(seq bol (or ,@(glsl-ts--shader-constants shader-type)))) @font-lock-constant-face))
     ((identifier) @glsl-shader-variable-name-face
      (:match ,(rx-to-string `(seq bol (or ,@(glsl-ts--shader-variables shader-type)))) @glsl-shader-variable-name-face)))

    :language glsl
    :feature delimiter        ; TODO: Other brackets?
    (["(" ")" "{" "}" "[" "]"] @font-lock-bracket-face)))


(defvar glsl-ts-indent-rules nil
  "Tree-sitter indentation rules for GLSL mode.")


(defvar glsl-ts-buffer-shader-type nil
  "The current buffer shader-type.")


(defun glsl-ts--detect-shader-type ()
  "Attempt to detect which GLSL shader type is active in the current buffer."
  (pcase (file-name-extension (buffer-file-name))
    ((or "vert" "vs") :vert)
    ((or "frag" "fs") :frag)
    ((or "geom") :geom)
    ((or "tesc") :tesc)
    ((or "tese") :tese)
    ((or "mesh") :mesh)
    ((or "task") :task)
    ((or "comp") :comp)
    ((or "rgen") :rgen)
    ((or "rint") :rint)
    ((or "rchit") :rchit)
    ((or "rahit") :rahit)
    ((or "rcall") :rcall)
    ((or "rmiss") :rmiss)
    ((or "glsl") nil)
    (_ nil)))


(defvar glsl-ts--imenu-rules
  (let ((pred #'c-ts-mode--defun-valid-p))
    `(("Enum" "\\`enum_specifier\\'" ,pred nil)
      ("Struct" "\\`struct_specifier\\'" ,pred nil)
      ("Union" "\\`union_specifier\\'" ,pred nil)
      ("Variable" ,(rx bos "declaration" eos) ,pred nil)
      ("Function" "\\`function_definition\\'" ,pred nil)))
  "Treesitter rules used to lookup IMenu related items.")


(defvar glsl-ts--defun-navigation-regexp
  (cons (regexp-opt (append '("function_definition"
                              "type_definition"
                              "struct_specifier"
                              "enum_specifier"
                              "union_specifier"
                              "class_specifier"
                              "namespace_definition")))
        #'c-ts-mode--defun-valid-p)
  "Regular expression used to navigate to the next defun.")


(defun glsl-ts-setup ()
  "Setup treesitter for glsl-ts-mode."

  ;; Syntax-highlighting.
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     (glsl-ts-font-lock-rules glsl-ts-buffer-shader-type)))

  ;; Indentation.
  (setq-local treesit-simple-indent-rules
              (treesit--indent-rules-optimize
               (c-ts-mode--simple-indent-rules 'c c-ts-mode-indent-style)))

  (setq-local c-ts-mode-indent-offset glsl-indent-offset)

  ;; Navigation
  (setq-local treesit-defun-type-regexp glsl-ts--defun-navigation-regexp)
  (setq-local treesit-defun-skipper #'c-ts-mode--defun-skipper)
  (setq-local treesit-defun-name-function #'c-ts-mode--defun-name)

  ;; Nodes like struct/enum/union_specifier can appear in
  ;; function_definitions, so we need to find the top-level node.
  (setq-local treesit-defun-prefer-top-level t)
  (setq-local treesit-defun-tactic 'top-level)

  ;; IMenu.
  (setq-local treesit-simple-imenu-settings glsl-ts--imenu-rules)

  (treesit-major-mode-setup))


(defvar-keymap glsl-ts-mode-map
  :doc "Keymap for GLSL."
  :parent prog-mode-map)


;;;###autoload
(define-derived-mode glsl-ts-mode c-ts-mode "GLSL[ts]"
  "Major mode for editing GLSL shaders with tree-sitter.

\\{glsl-ts-mode-map}"
  :syntax-table glsl-mode-syntax-table

  (setq-local glsl-ts-buffer-shader-type (glsl-ts--detect-shader-type))

  ;; Find-file.
  (setq-local ff-other-file-alist 'glsl-other-file-alist)

  ;; Comment.
  (c-ts-common-comment-setup)
  (setq-local comment-start "/* ")
  (setq-local comment-end " */")

  ;; Electric
  (setq-local electric-indent-chars (append "{}():;,#" electric-indent-chars))

  ;; Align.
  (add-to-list 'align-c++-modes 'glsl-ts-mode)

  ;; Font-lock settings.
  (setq-local font-lock-defaults nil)
  (setq-local treesit-font-lock-feature-list
              '((comment document definition)
                (keyword preprocessor string type qualifier builtin)
                (assignment constant escape-sequence literal)
                (bracket delimiter error function operator property variable)))

  (when (treesit-ready-p 'glsl)
    (treesit-parser-create 'glsl)
    (glsl-ts-setup)))

(provide 'glsl-ts-mode)

;;; glsl-ts-mode.el ends here
