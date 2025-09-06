;; Comments
(comment) @comment
(block_comment) @comment

;; Strings and Characters
(str_lit) @string
(char_lit) @character
(path_lit) @string.special
;; Throws an error b/c it's not referenced in the source.
; (regex_lit) @string.regex
(fancy_literal) @string.special

;; Numbers
(num_lit) @number
(complex_num_lit) @number

;; Keywords and Symbols
(kwd_lit) @constant
(kwd_symbol) @constant
(sym_lit) @variable
(package_lit) @namespace

;; Lists, Vectors, Sets, Maps
(list_lit) @punctuation.bracket
(vec_lit) @punctuation.bracket
(set_lit) @punctuation.bracket
(map_lit) @punctuation.bracket

;; Metadata
(meta_lit) @attribute
(old_meta_lit) @attribute

;; Reader Macros
(var_quoting_lit) @macro
(quoting_lit) @macro
(syn_quoting_lit) @macro
(unquote_splicing_lit) @macro
(unquoting_lit) @macro
(include_reader_macro) @macro
(self_referential_reader_macro) @macro
(dis_expr) @macro
;; Throws an error b/c it's not referenced in the source.
; (evaling_lit) @macro
;; Throws an error b/c it's not referenced in the source.
; (tagged_or_ctor_lit) @macro
; (derefing_lit) @macro

;; Special forms and macros
(defun_keyword) @keyword
; (defun_header (function_name: (_) @function)
(defun_header (_)) @function
(loop_macro) @repeat

;; Format specifiers (inside strings)
(format_specifier) @string.special

;; Nil and booleans
(nil_lit) @constant.builtin

;;
