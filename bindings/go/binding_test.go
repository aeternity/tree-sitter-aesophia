package tree_sitter_aesophia_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-aesophia"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_aesophia.Language())
	if language == nil {
		t.Errorf("Error loading Aesophia grammar")
	}
}
