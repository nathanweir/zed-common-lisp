;; Outline query for Zed

;; Function/macro/generic/method definitions
(
  (defun
    (defun_header
      function_name: [(sym_lit) (package_lit)] @name
      .
      (_)*)
    @item
  )
)

;; Docstring as annotation (if present as first value in defun)
;; Doesn't quite work, but I think I'm just not using the outliner correctly?
(
  (defun
    (defun_header
      function_name: [(sym_lit) (package_lit)] @name)
    value: (str_lit) @annotation
    @item
  )
)
