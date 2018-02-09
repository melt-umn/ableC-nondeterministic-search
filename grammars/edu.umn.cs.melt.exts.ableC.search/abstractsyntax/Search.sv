grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

imports silver:langutil;
imports silver:langutil:pp;

imports edu:umn:cs:melt:ableC:abstractsyntax:host;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction;
imports edu:umn:cs:melt:ableC:abstractsyntax:substitution;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;
imports edu:umn:cs:melt:exts:ableC:closure:abstractsyntax;

global builtin::Location = builtinLoc("nondeterministic-search");

abstract production searchFunctionDeclaration
top::Decl ::= f::SearchFunctionDecl
{
  local localErrors::[Message] = {-checkSearchXHInclude(f.sourceLocation, top.env) ++-} f.errors;
  local hostTrans::Decl = f.host;
  local hostErrorTrans::Decl =
    defsDecl([valueDef("_search_function_" ++ f.name, errorValueItem())]);
  
  forwards to
    decls(
      foldDecl([
        defsDecl([searchFunctionDef(f.name, searchFunctionItem(f))]),
        if !null(localErrors)
        then decls(foldDecl([warnDecl(localErrors), hostErrorTrans]))
        else hostTrans]));
}

synthesized attribute resultType::Type;
synthesized attribute parameterTypes::[Type];

nonterminal SearchFunctionDecl with env, pp, host<Decl>, errors, name, resultType, parameterTypes, sourceLocation;

abstract production searchFunctionDecl
top::SearchFunctionDecl ::= ret::TypeName id::Name params::Parameters body::SearchStmt
{
  top.pp =
    ppConcat([
      text("search"), space(), ret.pp, space(), id.pp,
      parens(ppImplode(text(", "), params.pps)),
      braces(cat(line(), nestlines(2, cat(body.pp, line()))))]);
  top.host =
    substDecl(
      [typedefSubstitution("__res_type__", typeModifierTypeExpr(ret.bty, ret.mty)),
       parametersSubstitution("__params__", params.host),
       stmtSubstitution("__body__", body.translation.asStmt)],
      decls(parseDecls(s"""
proto_typedef bool, __res_type__, __params__;

static __res_type__ _search_function_${id.name}(task_buffer_t _schedule, __cont_type__ _continuation, __params__) {
  __body__;
}
""")));
  top.errors := ret.errors ++ params.errors ++ body.errors;
  top.errors <- id.valueRedeclarationCheckNoCompatible; -- TODO: Prototypes
  top.name = id.name;
  top.resultType = ret.typerep;
  top.parameterTypes = params.typereps;
  top.sourceLocation = id.location;
  
  params.env = addEnv(ret.defs, ret.env);
  body.env = addEnv(params.defs, params.env);
}
