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

(defvar glsl-ts--common-shader-constants
  '("gl_MaxVertexAttribs"
    "gl_MaxVertexUniformVectors"
    "gl_MaxVertexUniformComponents"
    "gl_MaxVertexOutputComponents"
    "gl_MaxVaryingComponents"
    "gl_MaxVaryingVectors"
    "gl_MaxVertexTextureImageUnits"
    "gl_MaxVertexImageUniforms"
    "gl_MaxVertexAtomicCounters"
    "gl_MaxVertexAtomicCounterBuffers"

    "gl_MaxTessPatchComponents"
    "gl_MaxPatchVertices"
    "gl_MaxTessGenLevel"

    "gl_MaxTessControlInputComponents"
    "gl_MaxTessControlOutputComponents"
    "gl_MaxTessControlTextureImageUnits"
    "gl_MaxTessControlUniformComponents"
    "gl_MaxTessControlTotalOutputComponents"
    "gl_MaxTessControlImageUniforms"
    "gl_MaxTessControlAtomicCounters"
    "gl_MaxTessControlAtomicCounterBuffers"

    "gl_MaxTessEvaluationInputComponents"
    "gl_MaxTessEvaluationOutputComponents"
    "gl_MaxTessEvaluationTextureImageUnits"
    "gl_MaxTessEvaluationUniformComponents"
    "gl_MaxTessEvaluationImageUniforms"
    "gl_MaxTessEvaluationAtomicCounters"
    "gl_MaxTessEvaluationAtomicCounterBuffers"

    "gl_MaxGeometryInputComponents"
    "gl_MaxGeometryOutputComponents"
    "gl_MaxGeometryImageUniforms"
    "gl_MaxGeometryTextureImageUnits"
    "gl_MaxGeometryOutputVertices"
    "gl_MaxGeometryTotalOutputComponents"
    "gl_MaxGeometryUniformComponents"
    "gl_MaxGeometryVaryingComponents"       ; Deprecated
    "gl_MaxGeometryAtomicCounters"
    "gl_MaxGeometryAtomicCounterBuffers"

    "gl_MaxFragmentImageUniforms"
    "gl_MaxFragmentInputComponents"
    "gl_MaxFragmentUniformVectors"
    "gl_MaxFragmentUniformComponents"
    "gl_MaxFragmentAtomicCounters"
    "gl_MaxFragmentAtomicCounterBuffers"

    "gl_MaxDrawBuffers"
    "gl_MaxTextureImageUnits"
    "gl_MinProgramTexelOffset"
    "gl_MaxProgramTexelOffset"
    "gl_MaxImageUnits"
    "gl_MaxSamples"
    "gl_MaxImageSamples"
    "gl_MaxClipDistances"
    "gl_MaxCullDistances"
    "gl_MaxViewports"

    "gl_MaxComputeImageUniforms"
    "gl_MaxComputeWorkGroupCount"
    "gl_MaxComputeWorkGroupSize"
    "gl_MaxComputeUniformComponents"
    "gl_MaxComputeTextureImageUnits"
    "gl_MaxComputeAtomicCounters"
    "gl_MaxComputeAtomicCounterBuffers"

    "gl_MaxCombinedTextureImageUnits"
    "gl_MaxCombinedImageUniforms"
    "gl_MaxCombinedImageUnitsAndFragmentOutputs" ; Deprecated.
    "gl_MaxCombinedShaderOutputResources"
    "gl_MaxCombinedAtomicCounters"
    "gl_MaxCombinedAtomicCounterBuffers"
    "gl_MaxCombinedClipAndCullDistances"
    "gl_MaxAtomicCounterBindings"
    "gl_MaxAtomicCounterBufferSize"

    "gl_MaxTransformFeedbackBuffers"
    "gl_MaxTransformFeedbackInterleavedComponents"

    "gl_MaxInputAttachments") ; Only present when targeting Vulkan.
  "Special constants available to all GLSL shaders.")


(defvar glsl-ts--vertex-shader-variables
  '("gl_VertexID"                       ; Without GL_KHR_vulkan_glsl
    "gl_InstanceID"                     ; Without GL_KHR_vulkan_glsl
    "gl_VertexIndex"                    ; With GL_KHR_vulkan_glsl
    "gl_InstanceIndex"                  ; With GL_KHR_vulkan_glsl
    "gl_DrawID"
    "gl_BaseVertex"
    "gl_BaseInstance"

    ;; "gl_PerVertex"
    "gl_Position"                       ; Output variables.
    "gl_PointSize"
    "gl_ClipDistance"
    "gl_CullDistance")
  "Special variables in vertex shaders.")

(defvar glsl-ts--fragment-shader-variables
  '("gl_FragCoord"
    "gl_FrontFacing"
    "gl_ClipDistance"
    "gl_CullDistance"
    "gl_PointCoord"
    "gl_PrimitiveID"
    "gl_SampleID"
    "gl_SamplePosition"
    "gl_SampleMaskIn"
    "gl_Layer"
    "gl_ViewportIndex"
    "gl_HelperInvocation"

    "gl_FragDepth"     ; Output variables.
    "gl_SampleMask")
  "Special variables in fragment shaders.")

(defvar glsl-ts--geometry-shader-variables
  '("gl_Position"
    "gl_PointSize"
    "gl_ClipDistance"
    "gl_CullDistance"
    "gl_in"

    "gl_PrimitiveIDIn"
    "gl_InvocationID"

    ;; "gl_PerVertex"
    "gl_Position"
    "gl_PointSize"
    "gl_ClipDistance"
    "gl_CullDistance"

    "gl_PrimitiveID"
    "gl_Layer"
    "gl_ViewportIndex")
  "Special variables in geometry shaders.")

(defvar glsl-ts--tesellation-control-shader-variables
  '("gl_Position"
    "gl_PointSize"
    "gl_ClipDistance"
    "gl_CullDistance"
    "gl_in"

    "gl_PatchVerticesIn"
    "gl_PrimitiveID"
    "gl_InvocationID"

    ;; "gl_PerVertex"
    "gl_Position"
    "gl_PointSize"
    "gl_ClipDistance"
    "gl_CullDistance"
    "gl_out"

    "gl_TessLevelOuter"
    "gl_TessLevelInner")
  "Special variables in tesellation control shaders.")

(defvar glsl-ts--tesellation-evaluation-shader-variables
  '(
    "gl_PerVertex"
    "gl_Position"
    "gl_PointSize"
    "gl_ClipDistance"
    "gl_CullDistance"
    "gl_in"

    "gl_PatchVerticesIn"
    "gl_PrimitiveID"
    "gl_TessCoord"
    "gl_TessLevelOuter"
    "gl_TessLevelInner"

    ;; "gl_PerVertex"
    "gl_Position"
    "gl_PointSize"
    "gl_ClipDistance"
    "gl_CullDistance")
  "Special variables in tesellation evaluation shaders.")

(defvar glsl-ts--mesh-shader-variables
  '("gl_NumWorkGroups"
    "gl_WorkGroupSize"
    "gl_WorkGroupID"
    "gl_LocalInvocationID"
    "gl_GlobalInvocationID"
    "gl_LocalInvocationIndex"
    "gl_PrimitivePointIndicesEXT"
    "gl_PrimitiveLineIndicesEXT"
    "gl_PrimitiveTriangleIndicesEXT"

    ;; "gl_MeshPerVertexEXT"
    ;; "gl_MeshVerticesEXT"
    "gl_MeshPerPrimitiveEXT"
    "gl_MeshPrimitivesEXT"

    "gl_Position"
    "gl_PointSize"
    "gl_ClipDistance"
    "gl_CullDistance"

    "gl_PrimitiveID"
    "gl_Layer"
    "gl_ViewportIndex"
    "gl_CullPrimitiveEXT"
    "gl_PrimitiveShadingRateEXT")
  "Special variables in mesh shaders.")

(defvar glsl-ts--task-shader-variables
  '("gl_NumWorkGroups"
    "gl_WorkGroupSize"
    "gl_WorkGroupID"
    "gl_LocalInvocationID"
    "gl_GlobalInvocationID"
    "gl_LocalInvocationIndex")
  "Special variables in task shaders.")

(defvar glsl-ts--compute-shader-variables
  '("gl_NumWorkGroups"
    "gl_WorkGroupSize"
    "gl_WorkGroupID"
    "gl_LocalInvocationID"
    "gl_GlobalInvocationID"
    "gl_LocalInvocationIndex")
  "Special variables in compute shaders.")

(defvar glsl-ts--ray-tracing-shader-constants
  '("gl_RayFlagsNoneEXT"
    "gl_RayFlagsOpaqueEXT"
    "gl_RayFlagsNoOpaqueEXT"
    "gl_RayFlagsTerminateOnFirstHitEXT"
    "gl_RayFlagsSkipClosestHitShaderEXT"
    "gl_RayFlagsCullBackFacingTrianglesEXT"
    "gl_RayFlagsCullFrontFacingTrianglesEXT"
    "gl_RayFlagsCullOpaqueEXT"
    "gl_RayFlagsCullNoOpaqueEXT"

    "gl_HitKindFrontFacingTriangleEXT"
    "gl_HitKindBackFacingTriangleEXT")
  "Special constants used in ray-tracing shaders.")

(defvar glsl-ts--ray-tracing-ray-gen-shader-variables
  '("gl_LaunchIDEXT" "gl_LaunchSizeEXT")
  "Special variables in ray-tracing ray-gen shaders.")

(defvar glsl-ts--ray-tracing-intersection-shader-variables
  '("gl_LaunchIDEXT"
    "gl_LaunchSizeEXT"
    "gl_PrimitiveID"
    "gl_InstanceID"
    "gl_InstanceCustomIndexEXT"
    "gl_GeometryIndexEXT"
    "gl_WorldRayOriginEXT"
    "gl_WorldRayDirectionEXT"
    "gl_ObjectRayOriginEXT"
    "gl_ObjectRayDirectionEXT"
    "gl_RayTminEXT"
    "gl_RayTmaxEXT"
    "gl_IncomingRayFlagsEXT"
    "gl_ObjectToWorldEXT"
    "gl_ObjectToWorld3x4EXT"
    "gl_WorldToObjectEXT"
    "gl_WorldToObject3x4EXT")
  "Special variables in ray-tracing ray-gen shaders.")

(defvar glsl-ts--ray-tracing-closest-hit-shader-variables
  '("gl_LaunchIDEXT"
    "gl_LaunchSizeEXT"
    "gl_PrimitiveID"
    "gl_InstanceID"
    "gl_InstanceCustomIndexEXT"
    "gl_GeometryIndexEXT"
    "gl_WorldRayOriginEXT"
    "gl_WorldRayDirectionEXT"
    "gl_ObjectRayOriginEXT"
    "gl_ObjectRayDirectionEXT"
    "gl_RayTminEXT"
    "gl_RayTmaxEXT"
    "gl_IncomingRayFlagsEXT"
    "gl_HitTEXT"
    "gl_HitKindEXT"
    "gl_ObjectToWorldEXT"
    "gl_WorldToObjectEXT"
    "gl_WorldToObject3x4EXT"
    "gl_ObjectToWorld3x4EXT"

    "gl_HitTriangleVertexPositionsEXT")  ; GL_EXT_ray_tracing_position_fetch
  "Special variables in ray-tracing closest-hit shaders.")

(defvar glsl-ts--ray-tracing-any-hit-shader-variables
  glsl-ts--ray-tracing-closest-hit-shader-variables
  "Special variables in ray-tracing any-hit shaders.")

(defvar glsl-ts--ray-tracing-miss-shader-variables
  '("gl_LaunchIDEXT"
    "gl_LaunchSizeEXT"
    "gl_WorldRayOriginEXT"
    "gl_WorldRayDirectionEXT"
    "gl_RayTminEXT"
    "gl_RayTmaxEXT"
    "gl_IncomingRayFlagsEXT")
  "Special variables in ray-tracing miss shaders.")

(defvar glsl-ts--ray-tracing-callable-shader-variables
  '("gl_LaunchIDEXT" "gl_LaunchSizeEXT")
  "Special variables in ray-tracing ray-gen shaders.")

(defvar glsl-ts--ray-tracing-builtins
  '("traceRayEXT"
    "reportIntersectionEXT"
    "ignoreIntersectionEXT"      ; Technically a keyword.
    "terminateRayEXT"            ; Technically a keyword.
    "executeCallableEXT")
  "Special built-in functions available to ray-tracing shaders.")

(defvar glsl-ts--mesh-builtins
  '("EmitMeshTasksEXT" "SetMeshOutputsEXT")
  "Special built-in functions available mesh shaders.")


(defun glsl-ts--shader-constants (shader-type)
  "Create a list of special variables and constants for SHADER-TYPE."
  (pcase shader-type
    (:vert  (append glsl-ts--common-shader-constants))
    (:frag  (append glsl-ts--common-shader-constants))
    (:geom  (append glsl-ts--common-shader-constants))
    (:tesc  (append glsl-ts--common-shader-constants))
    (:tese  (append glsl-ts--common-shader-constants))
    (:mesh  (append glsl-ts--common-shader-constants))
    (:task  (append glsl-ts--common-shader-constants))
    (:comp  (append glsl-ts--common-shader-constants))
    (:rgen  (append glsl-ts--common-shader-constants glsl-ts--ray-tracing-shader-constants))
    (:rint  (append glsl-ts--common-shader-constants glsl-ts--ray-tracing-shader-constants))
    (:rchit (append glsl-ts--common-shader-constants glsl-ts--ray-tracing-shader-constants))
    (:rahit (append glsl-ts--common-shader-constants glsl-ts--ray-tracing-shader-constants))
    (:rcall (append glsl-ts--common-shader-constants glsl-ts--ray-tracing-shader-constants))
    (:rmiss (append glsl-ts--common-shader-constants glsl-ts--ray-tracing-shader-constants))
    (_ nil)))

(defun glsl-ts--shader-variables (shader-type)
  "Create a list of special variables and constants for SHADER-TYPE."
  (pcase shader-type
    (:vert  (append glsl-ts--vertex-shader-variables))
    (:frag  (append glsl-ts--fragment-shader-variables))
    (:geom  (append glsl-ts--geometry-shader-variables))
    (:tesc  (append glsl-ts--tesellation-control-shader-variables))
    (:tese  (append glsl-ts--tesellation-evaluation-shader-variables))
    (:mesh  (append glsl-ts--mesh-shader-variables))
    (:task  (append glsl-ts--task-shader-variables))
    (:comp  (append glsl-ts--compute-shader-variables))
    (:rgen  (append glsl-ts--ray-tracing-ray-gen-shader-variables))
    (:rint  (append glsl-ts--ray-tracing-intersection-shader-variables))
    (:rchit (append glsl-ts--ray-tracing-closest-hit-shader-variables))
    (:rahit (append glsl-ts--ray-tracing-any-hit-shader-variables))
    (:rcall (append glsl-ts--ray-tracing-callable-shader-variables))
    (:rmiss (append glsl-ts--ray-tracing-miss-shader-variables))
    (_ nil)))

(defun glsl-ts--shader-builtins (shader-type)
  "Create a list of shader builtin functions for SHADER-TYPE."
  (pcase shader-type
    (:vert  (append glsl-builtin-list))
    (:frag  (append glsl-builtin-list))
    (:geom  (append glsl-builtin-list))
    (:tesc  (append glsl-builtin-list))
    (:tese  (append glsl-builtin-list))
    (:mesh  (append glsl-builtin-list glsl-ts--mesh-builtins))
    (:task  (append glsl-builtin-list glsl-ts--mesh-builtins))
    (:comp  (append glsl-builtin-list))
    (:rgen  (append glsl-builtin-list glsl-ts--ray-tracing-builtins))
    (:rint  (append glsl-builtin-list glsl-ts--ray-tracing-builtins))
    (:rchit (append glsl-builtin-list glsl-ts--ray-tracing-builtins))
    (:rahit (append glsl-builtin-list glsl-ts--ray-tracing-builtins))
    (:rcall (append glsl-builtin-list glsl-ts--ray-tracing-builtins))
    (:rmiss (append glsl-builtin-list glsl-ts--ray-tracing-builtins))
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
     (declaration (layout_specification "layout" @glsl-qualifier-face)
                  ["in" @font-lock-keyword-face
                   "out" @font-lock-keyword-face]
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
