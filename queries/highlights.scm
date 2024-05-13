
;; Comments

(line_comment) @comment
(block_comment) @comment
(doc_comment) @comment.doc

;; Constants / literals

(lit_integer) @literal.number
(lit_string) @literal.string
(lit_char) @literal.char
(lit_bool) @literal.bool
(lit_address) @literal.address
(lit_bytes) @literal.bytes

;; Identifiers

(scope_name) @id.scope
(constructor) @id.constructor

(function_clause name: (identifier) @id.function)
(function_signature name: (identifier) @id.function)

;; Types

(type_contract) @type.contract
(type_variable_poly) @type.variable_poly
(type_variable) @type.variable
(type_variable "state") @type.variable.state

;; Type declarations

(type_alias name: (identifier) @id.typedecl.alias)
(record_declaration name: (identifier) @id.typedecl.record)
(variant_declaration name: (identifier) @id.typedecl.variant)

(field_declaration name: (identifier) @field.typedecl)
(constructor_declaration name: (identifier) @constructor.typedecl)

;; Expressions

(expr_op op: _ @expr.operator)
(expr_variable) @id.expr
(expr_variable "state") @id.expr.state
(expr_projection field: _ @field.expr)
(expr_application fun: (expr_variable) @id.expr.function)

;; Patterns

(map_update old_value: (identifier) @pattern.map_old_value)
(expr_letval pattern: (identifier) @pattern.letval)


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
  "interface"
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
