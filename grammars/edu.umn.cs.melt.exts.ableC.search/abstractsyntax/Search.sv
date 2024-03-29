grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

imports silver:langutil;
imports silver:langutil:pp;
imports silver:util:treemap as tm;

imports edu:umn:cs:melt:ableC:abstractsyntax:host;
imports edu:umn:cs:melt:ableC:abstractsyntax:overloadable as ovrld;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;
imports edu:umn:cs:melt:exts:ableC:refCountClosure:abstractsyntax;

abstract production searchFunctionDeclaration
top::Decl ::= f::SearchFunctionDecl
{
  propagate env, controlStmtContext;
  top.pp = f.pp;

  local localErrors::[Message] = checkSearchInclude(top.env) ++ f.errors;
  local hostErrorTrans::Decl =
    defsDecl([valueDef("_search_function_" ++ f.name, errorValueItem())]);
  
  forwards to
    decls(
      foldDecl(
        defsDecl([searchFunctionDef(f.name, searchFunctionItem(f))]) ::
        if !null(localErrors)
        then [warnDecl(localErrors), hostErrorTrans]
        else [f.host]));
}

synthesized attribute resultType::Type;
synthesized attribute parameterTypes::[Type];

tracked nonterminal SearchFunctionDecl with env, pp, host<Decl>, errors, name, resultType, parameterTypes;
flowtype SearchFunctionDecl = decorate {env}, pp {}, host {decorate}, errors {decorate}, name {decorate}, resultType {decorate}, parameterTypes {decorate};

abstract production searchFunctionProto
top::SearchFunctionDecl ::= storage::StorageClasses bty::BaseTypeExpr mty::TypeModifierExpr id::Name
{
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
      decorate nilParameters() with {env = top.env; controlStmtContext = initialControlStmtContext; position = 0;}
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
  
  top.host = makeSearchFunctionProto(storage, id.name, bty, result, params, variadic, q);
  top.errors := bty.errors ++ mty.errors;
  top.errors <- id.searchFunctionRedeclarationCheck(result.typerep, params.typereps);
  top.name = id.name;
  top.resultType = result.typerep;
  top.parameterTypes = params.typereps;
 
  bty.env = top.env;
  bty.controlStmtContext = initialControlStmtContext;
  bty.givenRefId = nothing();
  mty.env = openScopeEnv(addEnv(bty.defs, bty.env));
  mty.controlStmtContext = initialControlStmtContext;
  mty.baseType = bty.typerep;
  mty.typeModifierIn = bty.typeModifier;
  id.env = top.env;
}

abstract production searchFunctionDecl
top::SearchFunctionDecl ::= storage::StorageClasses fnquals::SpecialSpecifiers bty::BaseTypeExpr mty::TypeModifierExpr id::Name body::SearchStmt
{
  top.pp =
    ppConcat([
      text("search"), space(), bty.pp, space(), mty.lpp, id.pp, mty.rpp, line(),
      braces(cat(line(), nestlines(2, cat(body.pp, line()))))]);
  attachNote extensionGenerated("ableC-nondeterministic-search");
  
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
      decorate nilParameters() with {env = top.env; controlStmtContext = initialControlStmtContext; position = 0;}
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
      ableC_Decls {
        proto_typedef task_buffer_t;
        $Decl{makeSearchFunctionProto(storage, id.name, bty, result, params, variadic, q)}
        $StorageClasses{storage} void $name{"_search_function_" ++ id.name}(
          task_buffer_t *const _schedule,
          refcount::closure<(
            task_buffer_t *const,
            $Parameters{
              case result.typerep of
                builtinType(_, voidType()) -> nilParameters()
              | _ -> ableC_Parameters { $directTypeExpr{result.typerep} }
              end}) -> void> _continuation,
          _Bool *_cancelled,
          $Parameters{directTypeParameters(params)}) {
            $Stmt{body.translation.asStmt}
            _continuation.remove_ref();
          }
      });
  top.errors := bty.errors ++ mty.errors ++ body.errors;
  -- TODO: so long as the original wasn't also a definition
  top.errors <- id.searchFunctionRedeclarationCheck(result.typerep, params.typereps);
  top.name = id.name;
  top.resultType = result.typerep;
  top.parameterTypes = params.typereps;
  
  production attribute implicitDefs::[Def] with ++;
  implicitDefs := [];
  
  local nameValueItem::ValueItem =
    builtinValueItem(
      pointerType(
        nilQualifier(),
        builtinType(
          consQualifier(constQualifier(), nilQualifier()),
          signedType(charType()))));
  implicitDefs <- map(valueDef(_, nameValueItem), ["__func__", "__FUNCTION__", "__PRETTY_FUNCTION__"]);
  
  bty.env = top.env;
  bty.controlStmtContext = initialControlStmtContext;
  bty.givenRefId = nothing();
  mty.env = addEnv(implicitDefs, openScopeEnv(addEnv(searchFunctionDef(id.name, searchFunctionItem(top)) :: bty.defs, bty.env)));
  mty.controlStmtContext = initialControlStmtContext;
  mty.baseType = bty.typerep;
  mty.typeModifierIn = bty.typeModifier;
  id.env = top.env;
  body.env = addEnv(mty.defs ++ params.functionDefs, mty.env);
  body.expectedResultType = result.typerep;
  body.nextTranslation = stmtTranslation(nullStmt());
}

function makeSearchFunctionProto
Decl ::= storage::StorageClasses id::String bty::Decorated BaseTypeExpr result::Decorated TypeModifierExpr params::Decorated Parameters variadic::Boolean q::Qualifiers 
{
  attachNote extensionGenerated("ableC-nondeterministic-search");
  return
    ableC_Decl {
      proto_typedef task_buffer_t;
      $StorageClasses{storage} void $name{"_search_function_" ++ id}(
        task_buffer_t *const _schedule,
        refcount::closure<(
          task_buffer_t *const,
          $Parameters{
            case result.typerep of
              builtinType(_, voidType()) -> nilParameters()
            | _ ->
              consParameters(
                parameterDecl(nilStorageClass(), new(bty), new(result), nothingName(), nilAttribute()),
                  nilParameters())
            end}) -> void> _continuation,
        _Bool *_cancelled,
        $Parameters{new(params)});
    };
}

function directTypeParameters
Parameters ::= p::Decorated Parameters
{
  return
    case p of
    | consParameters(parameterDecl(storage, bty, mty, n, attrs), t) ->
        consParameters(
          parameterDecl(storage, directTypeExpr(mty.typerep), baseTypeExpr(), n, attrs),
          directTypeParameters(t))
    | nilParameters() -> nilParameters()
    end;
}

abstract production concreteInvokeExpr
top::Expr ::= args::Exprs
{
  propagate env, controlStmtContext;
  top.pp = pp"invoke(${ppImplode(pp", ", args.pps)})";
  forwards to
    case args of
    -- TODO: Use concrete patterns here
    | consExpr(declRefExpr(driver), consExpr(result, consExpr(ovrld:callExpr(declRefExpr(f), a), nilExpr()))) ->
      invokeExpr(driver, nilExpr(), justExpr(result), f, a)
    | consExpr(declRefExpr(driver), consExpr(callExpr(declRefExpr(f), a), nilExpr())) ->
      invokeExpr(driver, nilExpr(), nothingExpr(), f, a)
    | consExpr(callExpr(declRefExpr(driver), driverArgs), consExpr(result, consExpr(ovrld:callExpr(declRefExpr(f), a), nilExpr()))) ->
      invokeExpr(driver, driverArgs, justExpr(result), f, a)
    | consExpr(callExpr(declRefExpr(driver), driverArgs), consExpr(ovrld:callExpr(declRefExpr(f), a), nilExpr())) ->
      invokeExpr(driver, driverArgs, nothingExpr(), f, a)
    | consExpr(declRefExpr(_), consExpr(_, consExpr(_, nilExpr()))) ->
      errorExpr([errFromOrigin(top, "Argument 3 of invoke must be a function call to an identifier")])
    | consExpr(declRefExpr(_), consExpr(_, nilExpr())) ->
      errorExpr([errFromOrigin(top, "Argument 2 of invoke must be a function call to an identifier")])
    | consExpr(_, consExpr(_, consExpr(_, nilExpr()))) ->
      errorExpr([errFromOrigin(top, "Argument 1 of invoke must be an identifier or function call to an identifier")])
    | consExpr(_, consExpr(_, nilExpr())) ->
      errorExpr([errFromOrigin(top, "Argument 1 of invoke must be an identifier or function call to an identifier")])
    | a -> errorExpr([errFromOrigin(top, s"Wrong number of arguments to invoke (expected 2 or 3, got ${toString(a.count)})")])
    end;
}

abstract production invokeExpr
top::Expr ::= driver::Name driverArgs::Exprs result::MaybeExpr f::Name a::Exprs
{
  top.pp = pp"invoke(${ppImplode(pp", ", (if driverArgs.count > 0 then pp"${driver.pp}(${ppImplode(pp", ", driverArgs.pps)})" else driver.pp) :: (if result.isJust then [result.pp] else []) ++ [pp"${f.pp}(${ppImplode(pp", ", a.pps)})"])})";
  attachNote extensionGenerated("ableC-nondeterministic-search");

  driver.env = top.env;

  driverArgs.env = top.env;
  driverArgs.controlStmtContext = initialControlStmtContext;
  driverArgs.expectedTypes =
    case driver.valueItem.typerep of
    | functionType(_, protoFunctionType(parameterTypes, _), _) -> tail(tail(parameterTypes))
    | _ -> []
    end;
  driverArgs.argumentPosition = 1;
  driverArgs.callExpr = decorate declRefExpr(f) with {env = top.env; controlStmtContext = initialControlStmtContext; };
  driverArgs.callVariadic = 
    case driver.valueItem.typerep of
    | functionType(_, protoFunctionType(_, variadic), _) -> variadic
    | _ -> true
    end;
    
  result.env = addEnv(driverArgs.defs, driverArgs.env);
  result.controlStmtContext = initialControlStmtContext;

  f.env = top.env;
  
  a.env = addEnv(result.defs, result.env);
  a.controlStmtContext = initialControlStmtContext;
  a.expectedTypes = f.searchFunctionItem.parameterTypes;
  a.argumentPosition = 1;
  a.callExpr = decorate declRefExpr(f) with {env = top.env; controlStmtContext = initialControlStmtContext; };
  a.callVariadic = false;
  
  local localBaseErrors::[Message] =
    driver.valueLookupCheck ++ driverArgs.errors ++
    result.errors ++
    f.searchFunctionLookupCheck ++ a.errors ++
    checkSearchInclude(top.env);
  
  local resType::Type = f.searchFunctionItem.resultType;
  local expectedDriverType::Type =
    functionType(
      builtinType(nilQualifier(), voidType()),
      protoFunctionType(
        [head(lookupValue("task_t", top.env)).typerep,
         pointerType(
           nilQualifier(),
           extType(nilQualifier(), refCountClosureType([], builtinType(nilQualifier(), voidType()))))],
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
     then [errFromOrigin(driver, s"Unexpected search driver type (expected ${showType(expectedDriverType)}, got ${showType(driver.valueItem.typerep)})")]
     else driverArgs.argumentErrors) ++
    case resType, result of
      builtinType(nilQualifier(), voidType()), justExpr(e) ->
        [errFromOrigin(e, s"Unexpected search result (invoked function returns void)")]
    | builtinType(nilQualifier(), voidType()), nothingExpr() -> []
    | _, justExpr(e) ->
        if !typeAssignableTo(pointerType(nilQualifier(), resType), e.typerep)
        then [errFromOrigin(e, s"Unexpected search result type (expected ${showType(pointerType(nilQualifier(), resType))}, got ${showType(e.typerep)})")]
        else []
    | _, nothingExpr() ->
        [errFromOrigin(top, s"Expected a search result of type ${showType(pointerType(nilQualifier(), resType))}")]
    end ++
    a.argumentErrors;
  
  local localErrors::[Message] =
    if !null(localBaseErrors) then localBaseErrors else localTypeErrors;
  local fwrd::Expr =
    ableC_Expr {
      ({proto_typedef task_t, task_buffer_t;
        _Bool _is_success[1] = {0};
        $Stmt{
          if result.isJust
          then
            ableC_Stmt { $directTypeExpr{resType} *_result = $Expr{result.justTheExpr.fromJust}; }
          else nullStmt()}
        refcount::closure<() -> void> _notify_success[1];
        refcount::closure<
          (task_buffer_t *const,
           $Parameters{
             if result.isJust
             then ableC_Parameters { $directTypeExpr{resType} }
             else nilParameters()}) -> void> _success_continuation =
          refcount::lambda (
            task_buffer_t *const _schedule,
            $Parameters{
              if result.isJust
              then ableC_Parameters { $directTypeExpr{resType} result }
              else nilParameters()}) -> (void) {
            $Stmt{
              if result.isJust
              then ableC_Stmt{ if (!*_is_success) *_result = result; }
              else nullStmt()}
            *_is_success = 1;
            (*_notify_success)();
          };
        task_t _task =
          refcount::lambda (task_buffer_t *const _schedule) ->
            ($name{"_search_function_" ++ f.name}(
               _schedule, _success_continuation, (void*)0, $Exprs{decExprs(a)}));
        $Name{driver}(_task, _notify_success, $Exprs{driverArgs});
        *_is_success;})
    };
  
  forwards to
    if !null(localErrors) || null(lookupValue(s"_search_function_${f.name}", top.env))
    then errorExpr(localErrors)
    else fwrd;
}

function checkSearchInclude
[Message] ::= env::Decorated Env
{
  return
    if !null(lookupValue("task_buffer_t", env)) then []
    else [errFromOrigin(ambientOrigin(), "Nondeterministic search requires <search.xh> to be included.")];
}
