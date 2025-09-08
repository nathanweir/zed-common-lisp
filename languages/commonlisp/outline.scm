;; Functions
(defun
  (fn_name) @name
  (documentation)? @annotation
) @item

;; Macros
(defmacro
  (_symbol) @name
  (documentation)? @annotation
) @item

(defmethod
  (fn_name) @name
  (documentation)? @annotation
) @item

(defgeneric
  (fn_name) @name
  (documentation)? @annotation
) @item

;; Variables/constants
(defvar
  (_symbol) @name
  (documentation)? @annotation
) @item

(defparameter
  (_symbol) @name
  (documentation)? @annotation
) @item

(defconstant
  (_symbol) @name
  (documentation)? @annotation
) @item

;; Classes and slots
(defclass
  (_symbol) @name
  (superclass_list) @context
  (slot_list
    (slot
      (symbol) @context.extra
    )*
  )
  (documentation)? @annotation
) @item

;; Package definitions
(list
  (symbol) @_defpackage
  (_symbol) @name
  (#eq? @_defpackage "defpackage")
) @item
