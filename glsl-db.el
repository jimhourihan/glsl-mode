;;; glsl-db.el --- GLSL keyword database -*- lexical-binding: t -*-

;; Copyright (C) 2024 Gustaf Waldemarson
;;
;; Authors: Gustaf Waldemarson <gustaf.waldemarson ~at~ gmail.com>
;;          Jim Hourihan <jimhourihan ~at~ gmail.com>
;; Keywords: languages OpenGL GPU SPIR-V Vulkan
;; Version: 3.0
;; URL: https://github.com/jimhourihan/glsl-mode
;; Package-Requires: ((emacs "26.1"))
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

;; Collection of variables for working with GLSL files.

;;; Code:

(defvar glsl-type-list
  '("float" "double" "int" "void" "bool" "mat2" "mat3"
    "mat4" "dmat2" "dmat3" "dmat4" "mat2x2" "mat2x3" "mat2x4" "dmat2x2"
    "dmat2x3" "dmat2x4" "mat3x2" "mat3x3" "mat3x4" "dmat3x2" "dmat3x3"
    "dmat3x4" "mat4x2" "mat4x3" "mat4x4" "dmat4x2" "dmat4x3" "dmat4x4" "vec2"
    "vec3" "vec4" "ivec2" "ivec3" "ivec4" "bvec2" "bvec3" "bvec4" "dvec2"
    "dvec3" "dvec4" "uint" "uvec2" "uvec3" "uvec4" "atomic_uint"
    "sampler1D" "sampler2D" "sampler3D" "samplerCube" "sampler1DShadow"
    "sampler2DShadow" "samplerCubeShadow" "sampler1DArray" "sampler2DArray"
    "sampler1DArrayShadow" "sampler2DArrayShadow" "isampler1D" "isampler2D"
    "isampler3D" "isamplerCube" "isampler1DArray" "isampler2DArray"
    "usampler1D" "usampler2D" "usampler3D" "usamplerCube" "usampler1DArray"
    "usampler2DArray" "sampler2DRect" "sampler2DRectShadow" "isampler2DRect"
    "usampler2DRect" "samplerBuffer" "isamplerBuffer" "usamplerBuffer"
    "sampler2DMS" "isampler2DMS" "usampler2DMS" "sampler2DMSArray"
    "isampler2DMSArray" "usampler2DMSArray" "samplerCubeArray"
    "samplerCubeArrayShadow" "isamplerCubeArray" "usamplerCubeArray"
    "image1D" "iimage1D" "uimage1D" "image2D" "iimage2D" "uimage2D" "image3D"
    "iimage3D" "uimage3D" "image2DRect" "iimage2DRect" "uimage2DRect"
    "imageCube" "iimageCube" "uimageCube" "imageBuffer" "iimageBuffer"
    "uimageBuffer" "image1DArray" "iimage1DArray" "uimage1DArray"
    "image2DArray" "iimage2DArray" "uimage2DArray" "imageCubeArray"
    "iimageCubeArray" "uimageCubeArray" "image2DMS" "iimage2DMS" "uimage2DMS"
    "image2DMSArray" "iimage2DMSArray" "uimage2DMSArray"))

(defvar glsl-qualifier-list
  '("attribute" "const" "uniform" "varying" "buffer" "shared" "coherent"
    "volatile" "restrict" "readonly" "writeonly" "layout" "centroid" "flat"
    "smooth" "noperspective" "patch" "sample" "in" "out" "inout"
    "invariant" "lowp" "mediump" "highp"))

(defvar glsl-keyword-list
  '("break" "continue" "do" "for" "while" "if" "else" "subroutine"
    "discard" "return" "precision" "struct" "switch" "default" "case"))

(defvar glsl-reserved-list
  '("input" "output" "asm" "class" "union" "enum" "typedef" "template" "this"
    "packed" "resource" "goto" "inline" "noinline"
    "common" "partition" "active" "long" "short" "half" "fixed" "unsigned" "superp"
    "public" "static" "extern" "external" "interface"
    "hvec2" "hvec3" "hvec4" "fvec2" "fvec3" "fvec4"
    "filter" "sizeof" "cast" "namespace" "using"
    "sampler3DRect"))

(defvar glsl-deprecated-qualifier-list
  '("varying" "attribute")) ; centroid is deprecated when used with varying

(defvar glsl-builtins-list
  '("abs" "acos" "acosh" "all" "any" "anyInvocation" "allInvocations"
    "allInvocationsEqual" "asin" "asinh" "atan" "atanh"
    "atomicAdd" "atomicMin" "atomicMax" "atomicAnd" "atomicOr"
    "atomicXor" "atomicExchange" "atomicCompSwap"
    "atomicCounter" "atomicCounterDecrement" "atomicCounterIncrement"
    "atomicCounterAdd" "atomicCounterSubtract" "atomicCounterMin"
    "atomicCounterMax" "atomicCounterAnd" "atomicCounterOr"
    "atomicCounterXor" "atomicCounterExchange" "atomicCounterCompSwap"
    "barrier" "bitCount" "bitfieldExtract" "bitfieldInsert" "bitfieldReverse"
    "ceil" "clamp" "cos" "cosh" "cross" "degrees" "determinant" "dFdx" "dFdy"
    "dFdyFine" "dFdxFine" "dFdyCoarse" "dFdxCoarse" "distance" "dot"
    "fwidthFine" "fwidthCoarse"
    "EmitStreamVertex" "EmitStreamPrimitive" "EmitVertex" "EndPrimitive"
    "EndStreamPrimitive" "equal" "exp" "exp2" "faceforward" "findLSB"
    "findMSB" "floatBitsToInt" "floatBitsToUint" "floor" "fma" "fract"
    "frexp" "fwidth" "greaterThan" "greaterThanEqual" "groupMemoryBarrier"
    "imageAtomicAdd" "imageAtomicAnd" "imageAtomicCompSwap" "imageAtomicExchange"
    "imageAtomicMax" "imageAtomicMin" "imageAtomicOr" "imageAtomicXor"
    "imageLoad" "imageSize" "imageStore" "imulExtended" "intBitsToFloat"
    "imageSamples" "interpolateAtCentroid" "interpolateAtOffset" "interpolateAtSample"
    "inverse" "inversesqrt" "isinf" "isnan" "ldexp" "length" "lessThan"
    "lessThanEqual" "log" "log2" "matrixCompMult" "max" "memoryBarrier"
    "memoryBarrierAtomicCounter" "memoryBarrierBuffer"
    "memoryBarrierShared" "memoryBarrierImage" "memoryBarrier"
    "min" "mix" "mod" "modf" "normalize" "not" "notEqual" "outerProduct"
    "packDouble2x32" "packHalf2x16" "packSnorm2x16" "packSnorm4x8"
    "packUnorm2x16" "packUnorm4x8" "pow" "radians" "reflect" "refract"
    "round" "roundEven" "sign" "sin" "sinh" "smoothstep" "sqrt" "step" "tan"
    "tanh" "texelFetch" "texelFetchOffset" "texture" "textureGather"
    "textureGatherOffset" "textureGatherOffsets" "textureGrad" "textureSamples"
    "textureGradOffset" "textureLod" "textureLodOffset" "textureOffset"
    "textureProj" "textureProjGrad" "textureProjGradOffset" "textureProjLod"
    "textureProjLodOffset" "textureProjOffset" "textureQueryLevels" "textureQueryLod"
    "textureSize" "transpose" "trunc" "uaddCarry" "uintBitsToFloat"
    "umulExtended" "unpackDouble2x32" "unpackHalf2x16" "unpackSnorm2x16"
    "unpackSnorm4x8" "unpackUnorm2x16" "unpackUnorm4x8" "usubBorrow"))

(defvar glsl-deprecated-builtins-list
  '("noise1" "noise2" "noise3" "noise4"
    "texture1D" "texture1DProj" "texture1DLod" "texture1DProjLod"
    "texture2D" "texture2DProj" "texture2DLod" "texture2DProjLod"
    "texture2DRect" "texture2DRectProj"
    "texture3D" "texture3DProj" "texture3DLod" "texture3DProjLod"
    "shadow1D" "shadow1DProj" "shadow1DLod" "shadow1DProjLod"
    "shadow2D" "shadow2DProj" "shadow2DLod" "shadow2DProjLod"
    "textureCube" "textureCubeLod"))

(defvar glsl-deprecated-variables-list
  '("gl_FragColor" "gl_FragData" "gl_MaxVarying" "gl_MaxVaryingFloats"
    "gl_MaxVaryingComponents"))

(defvar glsl-preprocessor-directive-list
  '("define" "undef" "if" "ifdef" "ifndef" "else" "elif" "endif"
    "error" "pragma" "extension" "version" "line"))

(defvar glsl-preprocessor-expr-list
  '("defined" "##"))

(defvar glsl-preprocessor-builtins-list
  '("__LINE__" "__FILE__" "__VERSION__"))


(defvar glsl-common-shader-constants
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


(defvar glsl-vertex-shader-variables
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

(defvar glsl-fragment-shader-variables
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

(defvar glsl-geometry-shader-variables
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

(defvar glsl-tesellation-control-shader-variables
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

(defvar glsl-tesellation-evaluation-shader-variables
  '("gl_PerVertex"
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

(defvar glsl-mesh-shader-variables
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

(defvar glsl-task-shader-variables
  '("gl_NumWorkGroups"
    "gl_WorkGroupSize"
    "gl_WorkGroupID"
    "gl_LocalInvocationID"
    "gl_GlobalInvocationID"
    "gl_LocalInvocationIndex")
  "Special variables in task shaders.")

(defvar glsl-compute-shader-variables
  '("gl_NumWorkGroups"
    "gl_WorkGroupSize"
    "gl_WorkGroupID"
    "gl_LocalInvocationID"
    "gl_GlobalInvocationID"
    "gl_LocalInvocationIndex")
  "Special variables in compute shaders.")

(defvar glsl-ray-tracing-shader-constants
  '("gl_RayFlagsNoneEXT"
    "gl_RayFlagsOpaqueEXT"
    "gl_RayFlagsNoOpaqueEXT"
    "gl_RayFlagsTerminateOnFirstHitEXT"
    "gl_RayFlagsSkipClosestHitShaderEXT"
    "gl_RayFlagsCullBackFacingTrianglesEXT"
    "gl_RayFlagsCullFrontFacingTrianglesEXT"
    "gl_RayFlagsCullOpaqueEXT"
    "gl_RayFlagsCullNoOpaqueEXT"

    "gl_RayFlagsForceOpacityMicromap2StateEXT" ; GL_EXT_opacity_micromap.

    "gl_RayFlagsSkipTrianglesEXT"       ; GL_EXT_ray_flags_primitive_culling.
    "gl_RayFlagsSkipAABBEXT"            ; GL_EXT_ray_flags_primitive_culling.

    "gl_HitKindFrontFacingMicroTriangleNV" ; GL_NV_displacement_micromap.
    "gl_HitKindBackFacingMicroTriangleNV"  ; GL_NV_displacement_micromap.

    "gl_HitKindFrontFacingTriangleEXT"
    "gl_HitKindBackFacingTriangleEXT")
  "Special constants used in ray-tracing shaders.")

(defvar glsl-ray-tracing-ray-gen-shader-variables
  '("gl_LaunchIDEXT" "gl_LaunchSizeEXT")
  "Special variables in ray-tracing ray-gen shaders.")

(defvar glsl-ray-tracing-intersection-shader-variables
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

(defvar glsl-ray-tracing-closest-hit-shader-variables
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

    "gl_HitMicroTriangleVertexPositionsNV"    ; GL_NV_displacement_micromap.
    "gl_HitMicroTriangleVertexBarycentricsNV" ; GL_NV_displacement_micromap.

    "gl_CullMaskEXT"                    ; GL_EXT_ray_cull_mask.
    "gl_HitTriangleVertexPositionsEXT") ; GL_EXT_ray_tracing_position_fetch.
  "Special variables in ray-tracing closest-hit shaders.")

(defvar glsl-ray-tracing-any-hit-shader-variables
  glsl-ray-tracing-closest-hit-shader-variables
  "Special variables in ray-tracing any-hit shaders.")

(defvar glsl-ray-tracing-miss-shader-variables
  '("gl_LaunchIDEXT"
    "gl_LaunchSizeEXT"
    "gl_WorldRayOriginEXT"
    "gl_WorldRayDirectionEXT"
    "gl_RayTminEXT"
    "gl_RayTmaxEXT"
    "gl_IncomingRayFlagsEXT"

    "gl_CullMaskEXT")                   ; GL_EXT_ray_cull_mask.
  "Special variables in ray-tracing miss shaders.")

(defvar glsl-ray-tracing-callable-shader-variables
  '("gl_LaunchIDEXT" "gl_LaunchSizeEXT")
  "Special variables in ray-tracing ray-gen shaders.")

(defvar glsl-ray-tracing-builtins
  '("traceRayEXT"
    "reportIntersectionEXT"
    "ignoreIntersectionEXT"             ; Technically a keyword.
    "terminateRayEXT"                   ; Technically a keyword.
    "executeCallableEXT")
  "Special built-in functions available to ray-tracing shaders.")

(defvar glsl-mesh-builtins
  '("EmitMeshTasksEXT" "SetMeshOutputsEXT")
  "Special built-in functions available mesh shaders.")


(defvar glsl-all-shader-constants
  (delete-dups
   (append glsl-common-shader-constants glsl-ray-tracing-shader-constants))
  "List of all special constants accross all shader types.")

(defvar glsl-all-shader-variables
  (delete-dups
   (append glsl-vertex-shader-variables
           glsl-fragment-shader-variables
           glsl-geometry-shader-variables
           glsl-tesellation-control-shader-variables
           glsl-tesellation-evaluation-shader-variables
           glsl-mesh-shader-variables
           glsl-task-shader-variables
           glsl-compute-shader-variables
           glsl-ray-tracing-ray-gen-shader-variables
           glsl-ray-tracing-intersection-shader-variables
           glsl-ray-tracing-closest-hit-shader-variables
           glsl-ray-tracing-any-hit-shader-variables
           glsl-ray-tracing-miss-shader-variables
           glsl-ray-tracing-callable-shader-variables))
  "List of all special variables accross all shader types.")

(defvar glsl-all-shader-builtins
  (delete-dups
   (append
    glsl-builtins-list
    glsl-ray-tracing-builtins
    glsl-mesh-builtins))
  "List of all special variables accross all shader types.")


(provide 'glsl-db)

;;; glsl-db.el ends here
