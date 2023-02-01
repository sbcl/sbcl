(in-package #:sb-simd-internals)

(defconstant most-positive-f32 most-positive-single-float)
(defconstant most-negative-f32 most-negative-single-float)
(defconstant most-positive-f64 most-positive-double-float)
(defconstant most-negative-f64 most-negative-double-float)

(defconstant most-positive-u1  (1- (expt 2  1)))
(defconstant most-positive-u2  (1- (expt 2  2)))
(defconstant most-positive-u4  (1- (expt 2  4)))
(defconstant most-positive-u8  (1- (expt 2  8)))
(defconstant most-positive-u16 (1- (expt 2 16)))
(defconstant most-positive-u32 (1- (expt 2 32)))
(defconstant most-positive-u64 (1- (expt 2 64)))

(defconstant  +u8-true+ most-positive-u8)
(defconstant +u16-true+ most-positive-u16)
(defconstant +u32-true+ most-positive-u32)
(defconstant +u64-true+ most-positive-u64)

(defconstant  +u8-false+ 0)
(defconstant +u16-false+ 0)
(defconstant +u32-false+ 0)
(defconstant +u64-false+ 0)

(defconstant most-positive-s8  (1- (expt 2 (1-  8))))
(defconstant most-positive-s16 (1- (expt 2 (1- 16))))
(defconstant most-positive-s32 (1- (expt 2 (1- 32))))
(defconstant most-positive-s64 (1- (expt 2 (1- 64))))

(defconstant most-negative-s8  (- (expt 2 (1-  8))))
(defconstant most-negative-s16 (- (expt 2 (1- 16))))
(defconstant most-negative-s32 (- (expt 2 (1- 32))))
(defconstant most-negative-s64 (- (expt 2 (1- 64))))

(defconstant  +s8-true+ -1)
(defconstant +s16-true+ -1)
(defconstant +s32-true+ -1)
(defconstant +s64-true+ -1)

(defconstant  +s8-false+ 0)
(defconstant +s16-false+ 0)
(defconstant +s32-false+ 0)
(defconstant +s64-false+ 0)

(defconstant +f32-true+ (sb-kernel:make-single-float +s32-true+))
(defconstant +f64-true+ (sb-kernel:make-double-float +s32-true+ +u32-true+))

(defconstant +f32-false+ (sb-kernel:make-single-float +s32-false+))
(defconstant +f64-false+ (sb-kernel:make-double-float +s32-false+ +u32-false+))
