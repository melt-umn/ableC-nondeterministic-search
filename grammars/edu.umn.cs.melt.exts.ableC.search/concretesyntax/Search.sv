grammar edu:umn:cs:melt:exts:ableC:search:concretesyntax;

imports silver:langutil;

imports edu:umn:cs:melt:ableC:concretesyntax;

imports edu:umn:cs:melt:ableC:abstractsyntax:host;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;

imports edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

marking terminal Search_t 'search' lexer classes {Ckeyword};
marking terminal Invoke_t 'invoke' lexer classes {Ckeyword};

concrete production searchFunctionDecl_c
top::Declaration_c ::= 'search' f::SearchFunctionDefinition_c
{
  top.ast = f.ast;
}

closed nonterminal SearchFunctionDefinition_c with location, ast<Decl>;
concrete productions top::SearchFunctionDefinition_c
| d::InitialSearchFunctionDefinition_c '{' ss::SearchStmts_c '}'
  {
    top.ast = d.ast;
    d.givenSearchStmt = foldSeqSearchStmt(ss.ast);
  }
  action {
    context = closeScope(context); -- Opened by InitialSearchFunctionDefinition_c.
  }
| ds::DeclarationSpecifiers_c d::Declarator_c ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    d.givenType = baseTypeExpr();
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);

    local specialSpecifiers :: SpecialSpecifiers =
      foldr(consSpecialSpecifier, nilSpecialSpecifier(), ds.specialSpecifiers);

    top.ast = searchFunctionDeclaration(searchFunctionProto(foldStorageClass(ds.storageClass), bt, d.ast, d.declaredIdent));
  }

inherited attribute givenSearchStmt::SearchStmt;

closed nonterminal InitialSearchFunctionDefinition_c with location, ast<Decl>, givenSearchStmt;
concrete productions top::InitialSearchFunctionDefinition_c
| ds::DeclarationSpecifiers_c d::Declarator_c
  {
    ds.givenQualifiers = ds.typeQualifiers;
    d.givenType = baseTypeExpr();

    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    local specialSpecifiers :: SpecialSpecifiers =
      foldr(consSpecialSpecifier, nilSpecialSpecifier(), ds.specialSpecifiers);
    
    top.ast =
      searchFunctionDeclaration(searchFunctionDecl(foldStorageClass(ds.storageClass), specialSpecifiers, bt, d.ast, d.declaredIdent, top.givenSearchStmt));
  }
  action {
    -- Function are annoying because we have to open a scope, then add the
    -- parameters, and close it after the brace.
    context = beginFunctionScope(d.declaredIdent, Identifier_t, d.declaredParamIdents, Identifier_t, context);
  }

concrete production invokeExpr_c
top::PrimaryExpr_c ::= 'invoke' '(' args::ArgumentExprList_c ')'
{
  local argExprs::Exprs = foldExpr(args.ast);
  argExprs.env = emptyEnv();
  argExprs.returnType = nothing();

  top.ast =
    case argExprs of
    | consExpr(declRefExpr(driver), consExpr(result, consExpr(callExpr(decExpr(declRefExpr(f)), a), nilExpr()))) ->
      invokeExpr(driver, nilExpr(), justExpr(result), f, a, location=top.location)
    | consExpr(declRefExpr(driver), consExpr(callExpr(decExpr(declRefExpr(f)), a), nilExpr())) ->
      invokeExpr(driver, nilExpr(), nothingExpr(), f, a, location=top.location)
    | consExpr(callExpr(decExpr(declRefExpr(driver)), driverArgs), consExpr(result, consExpr(callExpr(decExpr(declRefExpr(f)), a), nilExpr()))) ->
      invokeExpr(driver, driverArgs, justExpr(result), f, a, location=top.location)
    | consExpr(callExpr(decExpr(declRefExpr(driver)), driverArgs), consExpr(callExpr(decExpr(declRefExpr(f)), a), nilExpr())) ->
      invokeExpr(driver, driverArgs, nothingExpr(), f, a, location=top.location)
    | consExpr(declRefExpr(_), consExpr(_, consExpr(_, nilExpr()))) ->
      errorExpr([err(top.location, "Argument 3 of invoke must be a function call to an identifier")], location=top.location)
    | consExpr(declRefExpr(_), consExpr(_, nilExpr())) ->
      errorExpr([err(top.location, "Argument 2 of invoke must be a function call to an identifier")], location=top.location)
    | consExpr(_, consExpr(_, consExpr(_, nilExpr()))) ->
      errorExpr([err(top.location, "Argument 1 of invoke must be an identifier or function call to an identifier")], location=top.location)
    | consExpr(_, consExpr(_, nilExpr())) ->
      errorExpr([err(top.location, "Argument 1 of invoke must be an identifier or function call to an identifier")], location=top.location)
    | a -> errorExpr([err(top.location, s"Wrong number of arguments to invoke (expected 2 or 3, got ${toString(a.count)})")], location=top.location)
    end;
}
