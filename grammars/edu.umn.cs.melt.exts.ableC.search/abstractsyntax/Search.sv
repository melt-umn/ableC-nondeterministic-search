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
  propagate substituted;
  top.pp = f.pp;

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

nonterminal SearchFunctionDecl with env, substitutions, pp, substituted<SearchFunctionDecl>, host<Decl>, errors, name, resultType, parameterTypes, sourceLocation;
flowtype SearchFunctionDecl = decorate {env}, pp {}, substituted {substitutions}, host {decorate}, errors {decorate}, name {decorate}, resultType {decorate}, parameterTypes {decorate}, sourceLocation {decorate};

abstract production searchFunctionDecl
top::SearchFunctionDecl ::= bty::BaseTypeExpr mty::TypeModifierExpr id::Name body::SearchStmt
{
  propagate substituted;
  top.pp =
    ppConcat([
      text("search"), space(), bty.pp, space(), mty.lpp, id.pp, mty.rpp, line(),
      braces(cat(line(), nestlines(2, cat(body.pp, line()))))]);
  
  local params::Decorated Parameters =
    case mty of
    | functionTypeExprWithArgs(result, params, variadic, q) -> params
    | functionTypeExprWithoutArgs(result, ids, q) ->
      -- TODO: Raise an error if ids isn't null
      decorate nilParameters() with {env = top.env; returnType = nothing();}
    | _ -> error("mty should always be a functionTypeExpr")
    end;
  local result::Decorated TypeModifierExpr =
    case mty of
    | functionTypeExprWithArgs(result, params, variadic, q) -> result
    | functionTypeExprWithoutArgs(result, ids, q) -> result
    | _ -> error("mty should always be a functionTypeExpr")
    end;
  local variadic::Boolean =
    case mty of
    | functionTypeExprWithArgs(result, params, variadic, q) -> variadic
    | functionTypeExprWithoutArgs(result, ids, q) -> false
    | _ -> error("mty should always be a functionTypeExpr")
    end;
  local q::Qualifiers =
    case mty of
    | functionTypeExprWithArgs(result, params, variadic, q) -> q
    | functionTypeExprWithoutArgs(result, ids, q) -> q
    | _ -> error("mty should always be a functionTypeExpr")
    end;
  
  local thisSearchFunctionDef::[Def] = [searchFunctionDef(id.name, searchFunctionItem(top))];
  top.host =
    functionDeclaration(
      functionDecl(
        [], nilSpecialSpecifier(),
        bty,
        functionTypeExprWithArgs(
          new(result),
          consParameters(
            parameterDecl(
              [],
              typedefTypeExpr(nilQualifier(), name("task_buffer_t", location=builtin)),
              baseTypeExpr(),
              justName(name("_schedule", location=builtin)),
              nilAttribute()),
            consParameters(
              parameterDecl(
                [],
                closureTypeExpr(
                  nilQualifier(),
                  case result.typerep of
                    builtinType(_, voidType()) -> nilParameters()
                  | _ ->
                    consParameters(
                      parameterDecl(
                        [],
                        directTypeExpr(result.typerep),
                        baseTypeExpr(),
                        nothingName(),
                        nilAttribute()),
                      nilParameters())
                  end,
                  typeName(builtinTypeExpr(nilQualifier(), voidType()), baseTypeExpr())),
                baseTypeExpr(),
                justName(name("_continuation", location=builtin)),
                nilAttribute()),
              new(params))),
          variadic, q),
        name("_search_function_" ++ id.name, location=builtin),
        nilAttribute(),
        nilDecl(),
        body.translation.asStmt));
  top.errors := bty.errors ++ mty.errors ++ body.errors;
  top.errors <- id.valueRedeclarationCheckNoCompatible; -- TODO: Prototypes
  -- TODO: check header include
  top.name = id.name;
  top.resultType = result.typerep;
  top.parameterTypes = params.typereps;
  top.sourceLocation = id.location;
  
  bty.returnType = nothing();
  bty.givenRefId = nothing();
  mty.env = openScopeEnv(addEnv(bty.defs ++ thisSearchFunctionDef, bty.env));
  mty.returnType = nothing();
  mty.baseType = bty.typerep;
  mty.typeModifiersIn = bty.typeModifiers;
  body.env = addEnv(params.defs, mty.env);
  body.expectedResultType = result.typerep;
  body.nextTranslation = stmtTranslation(nullStmt());
}

abstract production invokeExpr
top::Expr ::= driver::Name result::Expr f::Name a::Exprs
{
  propagate substituted;
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
  local localErrors::[Message] =
    f.searchFunctionLookupCheck ++ a.errors ++ driver.valueLookupCheck ++
    -- TODO: check header include first
    (if !typeAssignableTo(expectedDriverType, driver.valueItem.typerep)
     then [err(driver.location, s"Unexpected search driver type (expected ${showType(expectedDriverType)}, got ${showType(driver.valueItem.typerep)})")]
     else []) ++
    case result.typerep of
      pointerType(_, subType) ->
        if !typeAssignableTo(subType, resType)
        then [err(driver.location, s"Unexpected search result type (expected ${showType(pointerType(nilQualifier(), resType))}, got ${showType(result.typerep)})")]
        else []
    | _ -> []
    end;
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
