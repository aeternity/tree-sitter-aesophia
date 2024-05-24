{
  "targets": [
    {
      "target_name": "tree_sitter_aesophia_binding",
      "dependencies": [
        "<!(node -p \"require('node-addon-api').targets\"):node_addon_api_except",
      ],
      "include_dirs": [
        "src",
      ],
      "sources": [
        "bindings/node/binding.cc",
        "src/parser.c",
        "src/scanner.c",
      ],
      "conditions": [
        ["OS!='win'", {
          "cflags_c": [
            "-std=c11",
            "-Weverything",
            "-Wno-padded",
            "-Wno-declaration-after-statement",
            "-Wno-old-style-declaration",
            "-Werror",
          ],
        }, { # OS == "win"
          "cflags_c": [
            "/std:c11",
            "/utf-8",
          ],
        }],
      ],
    }
  ]
}
