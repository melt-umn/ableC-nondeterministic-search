grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

imports edu:umn:cs:melt:ableC:concretesyntax as cst;
import edu:umn:cs:melt:ableC:abstractsyntax:construction:parsing as hostParsing;

-- Parsers to replace those defined in construction:parsing, including the concrete syntax of the closure extension
parser declsParser :: cst:TranslationUnit_c {
  edu:umn:cs:melt:ableC:concretesyntax;
  edu:umn:cs:melt:ableC:concretesyntax:construction;
  edu:umn:cs:melt:exts:ableC:closure;
}

parser declParser :: cst:ExternalDeclaration_c {
  edu:umn:cs:melt:ableC:concretesyntax;
  edu:umn:cs:melt:ableC:concretesyntax:construction;
  edu:umn:cs:melt:exts:ableC:closure;
}

parser stmtParser :: cst:BlockItemList_c {
  edu:umn:cs:melt:ableC:concretesyntax;
  edu:umn:cs:melt:ableC:concretesyntax:construction;
  edu:umn:cs:melt:exts:ableC:closure;
}

parser exprParser :: cst:Expr_c {
  edu:umn:cs:melt:ableC:concretesyntax;
  edu:umn:cs:melt:ableC:concretesyntax:construction;
  edu:umn:cs:melt:exts:ableC:closure;
}

-- Wrapper functions to call parsers and return asts
global parseDecls::(Decls ::= String) = \ text::String -> foldDecl(hostParsing:parseInline("Decls", declsParser, text).ast);
global parseDecl::(Decl ::= String) = \ text::String -> hostParsing:parseInline("Decl", declParser, text).ast;
global parseStmt::(Stmt ::= String) = \ text::String -> foldStmt(hostParsing:parseInline("Stmt", stmtParser, text).ast);
global parseExpr::(Expr ::= String) = \ text::String -> hostParsing:parseInline("Expr", exprParser, text).ast;

-- Other construction helpers
function foldSeqSearchStmt
SearchStmt ::= l::[SearchStmt]
{
  return if null(l) then nullSearchStmt(location=builtin) else foldr1(seqSearchStmt(_, _, location=builtin), l);
}

function foldChoiceSearchStmt
SearchStmt ::= l::[SearchStmt]
{
  return if null(l) then nullSearchStmt(location=builtin) else foldr1(choiceSearchStmt(_, _, location=builtin), l);
}
