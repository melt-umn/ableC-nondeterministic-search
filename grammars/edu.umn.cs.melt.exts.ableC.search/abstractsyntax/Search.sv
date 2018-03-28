grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

imports silver:langutil;
imports silver:langutil:pp;

imports edu:umn:cs:melt:ableC:abstractsyntax:host;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction;
imports edu:umn:cs:melt:ableC:abstractsyntax:substitution;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;
imports edu:umn:cs:melt:exts:ableC:refCountClosure:abstractsyntax;

global builtin::Location = builtinLoc("nondeterministic-search");

abstract production searchFunctionDeclaration
top::Decl ::= f::SearchFunctionDecl
{
  propagate substituted;
  top.pp = f.pp;

  local localErrors::[Message] = checkSearchInclude(f.sourceLocation, top.env) ++ f.errors;
  
  forwards to
    decls(
      foldDecl([
        defsDecl([searchFunctionDef(f.name, searchFunctionItem(f))]),
        if !null(localErrors)
        then warnDecl(localErrors)
        else f.host]));
}

synthesized attribute resultType::Type;
synthesized attribute parameterTypes::[Type];

nonterminal SearchFunctionDecl with env, substitutions, pp, substituted<SearchFunctionDecl>, host<Decl>, errors, name, resultType, parameterTypes, sourceLocation;
flowtype SearchFunctionDecl = decorate {env}, pp {}, substituted {substitutions}, host {decorate}, errors {decorate}, name {decorate}, resultType {decorate}, parameterTypes {decorate}, sourceLocation {decorate};

abstract production searchFunctionProto
top::SearchFunctionDecl ::= bty::BaseTypeExpr mty::TypeModifierExpr id::Name
{
  propagate substituted;
  top.pp =
    ppConcat([
      text("search"), space(), bty.pp, space(), mty.lpp, id.pp, mty.rpp, semi()]);
  
  local result::Decorated TypeModifierExpr =
    case mty of
    | functionTypeExprWithArgs(result, params, variadic, q) -> result
    | functionTypeExprWithoutArgs(result, ids, q) -> result
    | _ -> error("mty should always be a functionTypeExpr")
    end;
  local params::Decorated Parameters =
    case mty of
    | functionTypeExprWithArgs(result, params, variadic, q) -> params
    | functionTypeExprWithoutArgs(result, ids, q) ->
      -- TODO: Raise an error if ids isn't null
      decorate nilParameters() with {env = top.env; returnType = nothing();}
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
  
  top.host = makeSearchFunctionProto(id.name, bty, result, params, variadic, q);
  top.errors := bty.errors ++ mty.errors;
  top.errors <- id.searchFunctionRedeclarationCheck(result.typerep, params.typereps);
  top.name = id.name;
  top.resultType = result.typerep;
  top.parameterTypes = params.typereps;
  top.sourceLocation = id.location;
  
  bty.returnType = nothing();
  bty.givenRefId = nothing();
  mty.env = openScopeEnv(addEnv(bty.defs, bty.env));
  mty.returnType = nothing();
  mty.baseType = bty.typerep;
  mty.typeModifiersIn = bty.typeModifiers;
}

abstract production searchFunctionDecl
top::SearchFunctionDecl ::= bty::BaseTypeExpr mty::TypeModifierExpr id::Name body::SearchStmt
{
  propagate substituted;
  top.pp =
    ppConcat([
      text("search"), space(), bty.pp, space(), mty.lpp, id.pp, mty.rpp, line(),
      braces(cat(line(), nestlines(2, cat(body.pp, line()))))]);
  
  local result::Decorated TypeModifierExpr =
    case mty of
    | functionTypeExprWithArgs(result, params, variadic, q) -> result
    | functionTypeExprWithoutArgs(result, ids, q) -> result
    | _ -> error("mty should always be a functionTypeExpr")
    end;
  local params::Decorated Parameters =
    case mty of
    | functionTypeExprWithArgs(result, params, variadic, q) -> params
    | functionTypeExprWithoutArgs(result, ids, q) ->
      -- TODO: Raise an error if ids isn't null
      decorate nilParameters() with {env = top.env; returnType = nothing();}
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
  
  top.host =
    decls(
      foldDecl([
        makeSearchFunctionProto(id.name, bty, result, params, variadic, q),
        functionDeclaration(
          functionDecl(
            [staticStorageClass()], consSpecialSpecifier(inlineQualifier(), nilSpecialSpecifier()),
            builtinTypeExpr(nilQualifier(), voidType()),
            functionTypeExprWithArgs(
              baseTypeExpr(),
              foldr(
                consParameters,
                directTypeParameters(params),
                [parameterDecl(
                   [],
                   typedefTypeExpr(nilQualifier(), name("task_buffer_t", location=builtin)),
                   pointerTypeExpr(
                     consQualifier(constQualifier(location=builtin), nilQualifier()),
                     baseTypeExpr()),
                   justName(name("_schedule", location=builtin)),
                   nilAttribute()),
                 parameterDecl(
                   [],
                   refCountClosureTypeExpr(
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
                     typeName(builtinTypeExpr(nilQualifier(), voidType()), baseTypeExpr()),
                     builtin),
                   baseTypeExpr(),
                   justName(name("_continuation", location=builtin)),
                   nilAttribute()),
                 parameterDecl(
                   [],
                   builtinTypeExpr(nilQualifier(), boolType()),
                   pointerTypeExpr(
                     consQualifier(constQualifier(location=builtin), nilQualifier()),
                     baseTypeExpr()),
                   justName(name("_cancelled", location=builtin)),
                   nilAttribute())]),
              variadic, q),
            name("_search_function_" ++ id.name, location=builtin),
        nilAttribute(),
        nilDecl(),
        seqStmt(
          body.translation.asStmt,
          parseStmt("_continuation.remove_ref();"))))]));
  top.errors := bty.errors ++ mty.errors ++ body.errors;
  -- TODO: so long as the original wasn't also a definition
  top.errors <- id.searchFunctionRedeclarationCheck(result.typerep, params.typereps);
  top.name = id.name;
  top.resultType = result.typerep;
  top.parameterTypes = params.typereps;
  top.sourceLocation = id.location;
  
  bty.returnType = nothing();
  bty.givenRefId = nothing();
  mty.env = openScopeEnv(addEnv(searchFunctionDef(id.name, searchFunctionItem(top)) :: bty.defs, bty.env));
  mty.returnType = nothing();
  mty.baseType = bty.typerep;
  mty.typeModifiersIn = bty.typeModifiers;
  body.env = addEnv(params.defs, mty.env);
  body.expectedResultType = result.typerep;
  body.nextTranslation = stmtTranslation(nullStmt());
}

function makeSearchFunctionProto
Decl ::= id::String bty::Decorated BaseTypeExpr result::Decorated TypeModifierExpr params::Decorated Parameters variadic::Boolean q::Qualifiers 
{
  return
    variableDecls(
      [staticStorageClass()], nilAttribute(),
      builtinTypeExpr(nilQualifier(), voidType()),
      consDeclarator(
        declarator(
          name("_search_function_" ++ id, location=builtin),
          functionTypeExprWithArgs(
            baseTypeExpr(),
            foldr(
              consParameters,
              new(params),
              [parameterDecl(
                 [],
                 typedefTypeExpr(nilQualifier(), name("task_buffer_t", location=builtin)),
                 pointerTypeExpr(
                   consQualifier(constQualifier(location=builtin), nilQualifier()),
                   baseTypeExpr()),
                 justName(name("_schedule", location=builtin)),
                 nilAttribute()),
               parameterDecl(
                 [],
                 refCountClosureTypeExpr(
                   nilQualifier(),
                   case result.typerep of
                     builtinType(_, voidType()) -> nilParameters()
                   | _ ->
                     consParameters(
                       parameterDecl([], new(bty), new(result), nothingName(), nilAttribute()),
                       nilParameters())
                   end,
                   typeName(builtinTypeExpr(nilQualifier(), voidType()), baseTypeExpr()),
                   builtin),
                 baseTypeExpr(),
                 justName(name("_continuation", location=builtin)),
                 nilAttribute()),
               parameterDecl(
                 [],
                 builtinTypeExpr(nilQualifier(), boolType()),
                 pointerTypeExpr(
                   consQualifier(constQualifier(location=builtin), nilQualifier()),
                   baseTypeExpr()),
                 justName(name("_cancelled", location=builtin)),
                 nilAttribute())]),
              variadic, q),
            nilAttribute(),
            nothingInitializer()),
          nilDeclarator()));
}

function directTypeParameters
Parameters ::= p::Decorated Parameters
{
  return
    case p of
      consParameters(parameterDecl(storage, bty, mty, n, attrs), t) ->
        consParameters(
          parameterDecl(storage, directTypeExpr(mty.typerep), baseTypeExpr(), n, attrs),
          directTypeParameters(t))
    | nilParameters() -> nilParameters()
    end;
}

abstract production invokeExpr
top::Expr ::= driver::Name driverArgs::Exprs result::MaybeExpr f::Name a::Exprs
{
  propagate substituted;
  top.pp = pp"invoke(${ppImplode(pp", ", (if driverArgs.count > 0 then pp"${driver.pp}(${ppImplode(pp", ", driverArgs.pps)})" else driver.pp) :: (if result.isJust then [result.pp] else []) ++ [pp"${f.pp}(${ppImplode(pp", ", a.pps)})"])})";
  
  driverArgs.returnType = nothing();
  driverArgs.expectedTypes =
    case driver.valueItem.typerep of
      functionType(_, protoFunctionType(parameterTypes, _), _) -> tail(tail(parameterTypes)) 
    | _ -> []
    end;
  driverArgs.argumentPosition = 1;
  driverArgs.callExpr = decorate declRefExpr(f, location=f.location) with {env = top.env; returnType = nothing();};
  driverArgs.callVariadic = 
    case driver.valueItem.typerep of
      functionType(_, protoFunctionType(_, variadic), _) -> variadic 
    | _ -> true
    end;
  
  a.returnType = nothing();
  a.expectedTypes = f.searchFunctionItem.parameterTypes;
  a.argumentPosition = 1;
  a.callExpr = decorate declRefExpr(f, location=f.location) with {env = top.env; returnType = nothing();};
  a.callVariadic = false;
  
  local localBaseErrors::[Message] =
    driver.valueLookupCheck ++ driverArgs.errors ++
    result.errors ++
    f.searchFunctionLookupCheck ++ a.errors ++
    checkSearchInclude(top.location, top.env);
  
  local resType::Type = f.searchFunctionItem.resultType;
  local expectedDriverType::Type =
    functionType(
      builtinType(nilQualifier(), voidType()),
      protoFunctionType(
        [head(lookupValue("task_t", top.env)).typerep,
         pointerType(
           nilQualifier(),
           refCountClosureType(nilQualifier(), [], builtinType(nilQualifier(), voidType())))],
        true),
      nilQualifier());
  local isDriverTypeValid::Boolean =
    case driver.valueItem.typerep of
      functionType(r, protoFunctionType(p1 :: p2 :: _, _), _) ->
        compatibleTypes(builtinType(nilQualifier(), voidType()), r, true, true) &&
        compatibleTypes(head(lookupValue("task_t", top.env)).typerep, p1, true, true) &&
        isRefCountClosureType(p2)
    | _ -> false
    end;
  local localTypeErrors::[Message] =
    (if isDriverTypeValid
     then [err(driver.location, s"Unexpected search driver type (expected ${showType(expectedDriverType)}, got ${showType(driver.valueItem.typerep)})")]
     else driverArgs.argumentErrors) ++
    case resType, result of
      builtinType(nilQualifier(), voidType()), justExpr(e) ->
        [err(e.location, s"Unexpected search result (invoked function returns void)")]
    | builtinType(nilQualifier(), voidType()), nothingExpr() -> []
    | _, justExpr(e) ->
        if !typeAssignableTo(pointerType(nilQualifier(), resType), e.typerep)
        then [err(e.location, s"Unexpected search result type (expected ${showType(pointerType(nilQualifier(), resType))}, got ${showType(e.typerep)})")]
        else []
    | _, nothingExpr() ->
        [err(top.location, s"Expected a search result of type ${showType(pointerType(nilQualifier(), resType))}")]
    end ++
    a.argumentErrors;
  
  local localErrors::[Message] =
    if !null(localBaseErrors) then localBaseErrors else localTypeErrors;
  local fwrd::Expr =
    substExpr(
      [typedefSubstitution("__res_type__", directTypeExpr(resType)),
       declRefSubstitution("__result__", result.justTheExpr.fromJust),
       exprsSubstitution("__args__", a),
       exprsSubstitution("__driver_args__", driverArgs)],
      parseExpr(s"""
({proto_typedef task_t, task_buffer_t, __res_type__;
  _Bool _is_success[1] = {0};
  ${if result.isJust then "__res_type__ *_result = __result__;" else ""}
  closure<() -> void> _notify_success[1];
  closure<(${if result.isJust then "__res_type__" else "void"}) -> void> _success_continuation =
    lambda (${if result.isJust then "__res_type__ result" else "void"}) -> (void) {
      *_is_success = 1;
      ${if result.isJust then "*_result = result;" else ""}
      (*_notify_success)();
    };
  task_t _task =
    lambda (task_buffer_t *const _schedule) ->
      (_search_function_${f.name}(_schedule, _success_continuation, (void*)0, __args__));
  ${driver.name}(_task, _notify_success, __driver_args__);
  *_is_success;})"""));
  
  forwards to
    if !null(localErrors) || null(lookupValue(s"_search_function_${f.name}", top.env))
    then errorExpr(localErrors, location=builtin)
    else fwrd;
}

function checkSearchInclude
[Message] ::= loc::Location env::Decorated Env
{
  return
    if !null(lookupValue("task_buffer_t", env)) then []
    else [err(loc, "Nondeterministic search requires <search.xh> to be included.")];
}
