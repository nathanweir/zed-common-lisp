;; Atoms
(symbol) @variable
(simple_symbol) @variable
; (package_symbol
;  (package) @namespace
;  (symbol) @variable)
(escaped_symbol) @string.special.symbol
(gensym) @variable.special
(keyword) @attribute
(number) @number
(integer) @number
(float) @number
(ratio) @number
(complex) @number
(string) @string
(character) @string.special
(simple_character) @string.special
(named_character) @string.special
(unicode_character) @string.special
(boolean) @boolean
(nil) @constant

;; Lists and vectors
(list
  "(" @punctuation.bracket
  ")" @punctuation.bracket)
(dotted_list
  "(" @punctuation.bracket
  "." @punctuation.delimiter
  ")" @punctuation.bracket)
(vector
  "#(" @punctuation.special
  ")" @punctuation.bracket)

;; Quote forms
(quoted_expression
  "'" @operator)
(quasiquoted_expression
  "`" @operator)
(unquoted_expression
  "," @operator)
(spliced_expression
  ",@" @operator)

;; Comments
(line_comment) @comment
(block_comment) @comment.doc

;; Reader macros and literals
(bit_vector) @string.special
(pathname) @string.special
(array_literal) @type
(structure_literal) @type
(function_reference) @function
(radix_number) @number
(octal_number) @number
(hex_number) @number
(binary_number) @number

;; Lambda list
(keyword) @keyword
(lambda_keyword) @keyword
(defun_keyword) @keyword
(in_package_keyword) @keyword
(defpackage_keyword) @keyword
(use_keyword) @keyword
(export_keyword) @keyword
