grammar edu:umn:cs:melt:exts:ableC:search;

{--
 - Here we export the concrete and abstract syntax of this extension in the
 - top-level extension grammar, so that an extension user can simply add this
 - grammar to a parser specification, or an extension writer can import both the
 - abstract syntax and concrete syntax of an extension by importing this grammar.
 -}
exports edu:umn:cs:melt:exts:ableC:search:abstractsyntax;
exports edu:umn:cs:melt:exts:ableC:search:concretesyntax;

-- Also export the closure extension, since its concrete syntax is needed for parsing header definitions
exports edu:umn:cs:melt:exts:ableC:closure;
