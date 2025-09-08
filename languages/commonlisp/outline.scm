;; TODO: Revisit

; ;; Package definitions
; (defpackage_form
;   (name: (_) @name)
;   (operator: (_) @context))

; (in_package_form
;   (name: (_) @name)
;   (operator: (_) @context))

; ;; Function definitions
; (defun_form
;   (name: (_) @name)
;   (parameters: (_) @context)
;   (body: (_) @item)
;   (operator: (_) @context.extra))

; ;; Let bindings
; (let_form
;   (bindings: (_) @context)
;   (body: (_) @item)
;   (operator: (_) @context.extra))

; (let_star_form
;   (bindings: (_) @context)
;   (body: (_) @item)
;   (operator: (_) @context.extra))

; ;; Handler-case
; (handler_case_form
;   (protected_form: (_) @context)
;   (handler: (_) @item)
;   (operator: (_) @context.extra))

; ;; Handler clause (nested in handler-case)
; (handler_clause
;   (condition: (_) @context)
;   (lambda_list: (_) @context.extra)
;   (body: (_) @item))

; ;; Arithmetic forms
; (arithmetic_form
;   (operator: (_) @name)
;   (operand: (_) @item))

; ;; Comments as annotations
; (line_comment) @annotation
; (block_comment) @annotation
