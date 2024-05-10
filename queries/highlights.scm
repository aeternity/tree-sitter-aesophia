(line_comment) @comment
(block_comment) @comment
(doc_comment) @comment.doc

(lit_integer) @number

(scope_name) @scope.name

(type_contract) @scope.name

(function_clause name: (identifier) @function.name)


(expr_op op: _ @operator)

(lit_string) @string

(expr_variable) @variable.name

(expr_projection field: _ @projection.name)

[
  "true"
  "false"
] @constant.builtin

;; TODO: Commented keywords cause Invalid node type error
[
  "contract"
  "include"
  "let"
  "switch"
  "type"
  "record"
  "datatype"
  "if"
  "elif"
  "else"
  "function"
  "stateful"
  "payable"
  "entrypoint"
  "private"
  ;; "indexed"
  "namespace"
  ;; "interface"
  "main"
  "using"
  "as"
  "for"
  "hiding"
  ] @keyword

[
 "return"
 "while"
 "for"
] @keyword.invalid
