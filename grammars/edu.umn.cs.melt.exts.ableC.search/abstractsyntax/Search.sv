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
  -- TODO: check header include
  top.name = id.name;
  top.resultType = ret.typerep;
  top.parameterTypes = params.typereps;
  top.sourceLocation = id.location;
  
  params.env = addEnv(ret.defs, ret.env);
  body.env = addEnv(params.defs, params.env);
}

abstract production invokeExpr
top::Expr ::= driver::Name result::Expr f::Name a::Exprs
{
  top.pp = pp"invoke(${driver.pp}, ${result.pp}, ${f.pp}(${ppImplode(pp", ", a.pps)}))";
  
  local resType::Type = f.searchFunctionItem.resultType;
  local expectedDriverType::Type =
    functionType(
      builtinType(nilQualifier(), voidType()),
      protoFunctionType(
        [head(lookupValue("task_t", top.env)).typerep,
         closureType(nilQualifier(), [], builtinType(nilQualifier(), voidType()))],
        false),
      nilQualifier());
  local expectedResultType::Type = pointerType(nilQualifier(), resType);
  local localErrors::[Message] =
    f.searchFunctionLookupCheck ++ a.errors ++ driver.valueLookupCheck ++
    -- TODO: check header include first
    (if !compatibleTypes(expectedDriverType, driver.valueItem.typerep, true, false)
     then [err(driver.location, s"Unexpected search driver type (expected ${showType(expectedDriverType)}, got ${showType(driver.valueItem.typerep)})")]
     else []) ++
    (if !compatibleTypes(result.typerep, expectedResultType, true, false)
     then [err(driver.location, s"Unexpected search result type (expected ${showType(expectedResultType)}, got ${showType(result.typerep)})")]
     else []);
  local fwrd::Expr =
    substExpr(
      [typedefSubstitution("__res_type__", directTypeExpr(resType)),
       exprsSubstitution("__args__", a)],
      parseExpr(s"""
({proto_typedef task_t, task_buffer_t;
  _Bool _is_success[1] = {0};
  __res_type__ *_result = __result__;
  closure<() -> void> _notify_success[1];
  closure<(__res_type__) -> void> _success_continuation =
    lambda (__res_type_ result) -> (void) {
      *_is_success = 1;
      *_result = result;
      (*_notify_success)();
    };
  task_t _task =
    lambda (task_buffer_t _schedule) ->
      (_search_function_${f.name}(_schedule, _success_continuation, __args__));
  ${driver.name}(_task, _notify_success);
  *_is_success;})"""));
  
  forwards to mkErrorCheck(localErrors, fwrd);
}
