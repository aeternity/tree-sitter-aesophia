
;; Comments

(line_comment) @comment
(block_comment) @comment
(doc_comment) @comment.doc


;; Top level

(pragma) @pragma


;; Functions

(function_clause name: (identifier) @variable.function)
(function_signature name: (identifier) @variable.function)


;; Type declarations

(type_alias name: (identifier) @variable.typedecl.alias)
(record_declaration name: (identifier) @variable.typedecl.record)
(variant_declaration name: (identifier) @variable.typedecl.variant)

(field_declaration name: (identifier) @field.typedecl)
(constructor_declaration name: (constructor) @constructor.typedecl)


;; Expressions

(expr_variable) @variable.expr
(((expr_variable) @expr.state)
 (#match? @expr.state "state")
 )
(expr_application fun: (expr_variable) @variable.expr.function)
(member_assign old_value: (identifier) @variable.map_old_value)
(expr_hole) @symbol.hole.expr

(expr_projection field: (identifier) @field)
(member_assign path_head: (identifier) @field)
(member_path (identifier) @field)
(member_access (identifier) @field)

;; Patterns

(pat_variable) @variable.pattern
(pat_hole) @symbol.hole.pattern
(pat_wildcard) @symbol.wildcard.pattern


;; Operators

(expr_op op: _ @operator.expr)
(expr_typed ":" @operator.type.expr)
(pat_typed ":" @operator.type.pattern)
(function_signature ":" @operator.type.function)
(function_clause ":" @operator.type.function)
(field_declaration ":" @operator.type.field)
(scope_declaration ":" @operator.type.scope)
(member_assign "@" @operator.expr.old_value)


;; Types

(type_contract) @type.contract
(type_variable_poly) @type.variable_poly
(type_variable) @type.variable
(((type_variable) @type.variable.state)
 (#match? @type.variable.state "state")
 )
(type_wildcard) @symbol.wildcard.type


;; Constants / literals

(lit_integer) @literal.number
(lit_string) @literal.string
(lit_char) @literal.char
(lit_bool) @literal.bool
(lit_address) @literal.address
(lit_bytes) @literal.bytes


;; Identifiers

(identifier) @variable
(scope_name) @variable.scope
(constructor) @variable.constructor


;; Misc

"(" @punctuation.bracket.paren
")" @punctuation.bracket.paren
"[" @punctuation.bracket.square
"]" @punctuation.bracket.square
"{" @punctuation.bracket.curly
"}" @punctuation.bracket.curly

["," "." ";"] @punctuation.delimiter

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
  "indexed"
  "namespace"
  (interface)
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
