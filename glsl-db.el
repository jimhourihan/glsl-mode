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
  '("float" "double" "int" "void" "bool" "true" "false" "mat2" "mat3"
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

(defvar glsl-builtin-list
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

(defvar glsl-deprecated-builtin-list
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

(defvar glsl-preprocessor-builtin-list
  '("__LINE__" "__FILE__" "__VERSION__"))

(provide 'glsl-db)

;;; glsl-db.el ends here
