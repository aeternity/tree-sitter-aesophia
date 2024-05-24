[
 (module)
 (scope_declaration)
 (type_alias name: (identifier) @local.definition.type)
 (record_declaration name: (identifier) @local.definition.type)
 (variant_declaration name: (identifier) @local.definition.type)
 (expr_lambda)
 (expr_list_comprehension)
 (switch_case)
 ] @local.scope

((function_signature name: (identifier) @local.definition.function) @local.scope
  (#set! definition.function.scope "parent")
  )

((function_clause name: (identifier) @local.definition.function) @local.scope
  (#set! definition.function.scope "parent")
  )

((expr_letval) (_)* @local.scope)

(pat_variable) @local.definition.var

(type_param) @local.definition.typevar

;; REFERENCES

(type_variable) @local.reference.type
(type_contract) @local.reference.type
(type_variable_poly) @local.reference.typevar

(expr_variable) @local.reference.var
(expr_projection field: (identifier) @local.reference.field)
(member_assign path_head: (identifier) @local.reference.field)
(member_path (identifier) @local.reference.field)
(member_access (identifier) @local.reference.field)
