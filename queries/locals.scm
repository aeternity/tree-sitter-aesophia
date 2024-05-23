;; SCOPES

[
 (scope_declaration)
 (function_clause)
 (type_alias)
 (record_declaration)
 (variant_declaration)
 (expr_lambda)
 (switch_case)
 ] @local.scope

((expr_letval) (_)* @local.scope)

;; DEFINITIONS

(pat_variable) @local.definition.var

(function_signature name: (identifier) @local.definition.function)
(function_clause name: (identifier) @local.definition.function)

(type_param) @local.definition.typevar
(type_alias name: (identifier) @local.definition.type)
(record_declaration name: (identifier) @local.definition.type)
(variant_declaration name: (identifier) @local.definition.type)


;; REFERENCES

(type_variable) @local.reference.type
(type_contract) @local.reference.type
(type_variable_poly) @local.reference.typevar

(expr_variable) @local.reference.var
(expr_projection field: (identifier) @local.reference.field)
(member_assign path_head: (identifier) @local.reference.field)
(member_path (identifier) @local.reference.field)
(member_access (identifier) @local.reference.field)
