(line_comment) @comment.line
(block_comment) @comment.block

(lit_integer) @number

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
  ;; "mod"
  ;; "public"
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
