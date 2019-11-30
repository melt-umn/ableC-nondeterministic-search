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
  top.ast = concreteInvokeExpr(foldExpr(args.ast), location=top.location);
}
