;; Documentation comments - inject markdown
((doc_comment) @injection.content
 (#set! injection.language "markdown")
 (#set! injection.include-children))

;; String literals might contain other languages
((lit_string) @injection.content
 (#match? @injection.content "^\"(html|css|json|sql):")
 (#lua-match? @injection.content "^\"(%w+):")
 (#set! injection.language capture.1))

;; Pragma compiler directives - inject version syntax
((pragma_compiler_vsn version: (version) @injection.content)
 (#set! injection.language "semver"))
