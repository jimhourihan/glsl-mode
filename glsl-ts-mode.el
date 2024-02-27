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
(require 'glsl-mode)



;; TODO: Add Keyword "discard" to GLSL grammar.
(defvar glsl-ts-keywords
  '("break" "continue" "do" "for" "while" "if" "else" ;; "discard"
    "subroutine" "return" "switch" "default" "case")
  "Keywords that shoud be high-lighted.")


(defun glsl-ts--shader-variables (shader-type)
  "Create a list of special shader variables and constants for SHADER-TYPE."
  (pcase shader-type
    (:vert nil)
    (:frag nil)
    (:geom nil)
    (:tesc nil)
    (:tese nil)
    (:mesh nil)
    (:task nil)
    (:comp nil)
    (:rgen '("gl_RayFlagsSkipClosestHitShaderEXT" "gl_RayFlagsTerminateOnFirstHitEXT"))
    (:rchit nil)
    (:rahit nil)
    (:rmiss nil)
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
     (preproc_def "#define" @glsl-preprocessor-face
                  name: ((identifier) @font-lock-variable-name-face))
     (preproc_include "#include" @glsl-preprocessor-face
                      ((string_literal) @font-lock-string-face)))

    :language glsl
    :feature definition
    ((function_declarator declarator: (_) @font-lock-function-name-face)
     (declaration (layout_specification "layout" @glsl-qualifier-face)
                  ["buffer" @font-lock-keyword-face
                   "uniform" @font-lock-keyword-face]
                  (identifier) @font-lock-variable-name-face)
     (declaration (layout_specification "layout" @glsl-qualifier-face)
                  (extension_storage_class) @font-lock-keyword-face
                  (identifier) @font-lock-variable-name-face)
     (declaration type: (_) declarator: (identifier) @font-lock-variable-name-face)
     (init_declarator declarator: (identifier) @font-lock-variable-name-face)
     (parameter_declaration type: (_) declarator: (identifier) @font-lock-variable-name-face)
     (parameter_declaration (["in" "out" "inout"] @font-lock-keyword-face)
                            type: (_) declarator: (identifier) @font-lock-variable-name-face)
     (field_declaration type: (_)
                        declarator: [(field_identifier) @font-lock-variable-name-face
                                     (array_declarator declarator: (field_identifier) @font-lock-variable-name-face)])
     (call_expression function:
                      ((identifier) @font-lock-type-face
                       (:match ,(rx-to-string `(seq bol (or ,@glsl-type-list) eol)) @font-lock-type-face))))

    :feature keyword
    :language glsl
    ([,@glsl-ts-keywords] @font-lock-keyword-face)

    :feature builtin
    :language glsl
    (((identifier) @font-lock-builtin-face
      (:match ,(rx-to-string `(seq bol (or ,@glsl-builtin-list) eol))
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
     (:match ,(rx-to-string `(seq bol (or ,@(glsl-ts--shader-variables :rgen)))) @font-lock-constant-face)))

    :language glsl
    :feature delimiter        ; TODO: Other brackets?
    (["(" ")" "{" "}"] @font-lock-bracket-face)))


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


(defun glsl-ts-setup ()
  "Setup treesitter for glsl-ts-mode."
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     (glsl-ts-font-lock-rules glsl-ts-buffer-shader-type)))

  (setq-local treesit-simple-indent-rules glsl-ts-indent-rules)

  (treesit-major-mode-setup))


;;;###autoload
(define-derived-mode glsl-ts-mode prog-mode "GLSL[ts]"
  "Major mode for editing GLSL shaders with tree-sitter."
  :syntax-table glsl-mode-syntax-table

  (setq-local glsl-ts-buffer-shader-type (glsl-ts--detect-shader-type))

  ;; TODO: imenu settings.

  ;; Font-lock settings.
  (setq-local font-lock-defaults nil)
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword preprocessor string type qualifier builtin)
                (assignment constant escape-sequence literal)
                (delimiter variable)))

  ;; TODO: Indentation settings.

  (when (treesit-ready-p 'glsl)
    (treesit-parser-create 'glsl)
    (glsl-ts-setup)))

(provide 'glsl-ts-mode)

;;; glsl-ts-mode.el ends here
