grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

imports silver:langutil;
imports silver:langutil:pp;

imports edu:umn:cs:melt:ableC:abstractsyntax:host;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction:parsing;
imports edu:umn:cs:melt:ableC:abstractsyntax:substitution;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;
imports edu:umn:cs:melt:exts:ableC:closure:abstractsyntax;

global builtin::Location = builtinLoc("nondeterministic-search");

abstract production searchFunctionDeclaration
top::Decl ::= f::SearchFunctionDecl
{
  forwards to
    decls(
      foldDecl([
        defsDecl([searchFunctionDef(f.name, searchFunctionItem(f))]),
        f.host]));
}

synthesized attribute resultType::Type;
synthesized attribute parameterTypes::[Type];

nonterminal SearchFunctionDecl with env, pp, host<Decl>, errors, name, resultType, parameterTypes, sourceLocation;

abstract production searchFunctionDecl
top::SearchFunctionDecl ::= ret::TypeName params::Parameters body::SearchStmt
{
  top.pp =
    ppConcat([
      text("search"), space(), ret.pp, space(),
      parens(ppImplode(text(", "), params.pps)),
      braces(cat(line(), nestlines(2, cat(body.pp, line()))))]);
  
}
