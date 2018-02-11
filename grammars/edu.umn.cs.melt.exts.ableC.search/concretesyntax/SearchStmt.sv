grammar edu:umn:cs:melt:exts:ableC:search:concretesyntax;

terminal Choice_t 'choice' lexer classes {Ckeyword};

{-
 - For double-brace-enclosed host statments
 - We need to use two RCurly_t terminals from ableC to close the block to avoid MDA follow spillage
 - The lexer hack parser action for '}' closes a scope, so we need to open two scopes here to
 - balance two '}' terminals
 -}
terminal DoubleLBrace_t '{{'
  action { context = head(context) :: head(context) :: context; },
  lexer classes {Ckeyword};

nonterminal SearchStmts_c with ast<[SearchStmt]>;

concrete productions top::SearchStmts_c
| h::SearchStmts_c t::SearchStmt_c
  { top.ast = h.ast ++ [t.ast]; }
| 
  { top.ast = []; }

nonterminal SearchStmt_c with ast<SearchStmt>;

concrete productions top::SearchStmt_c
| ';'
  { top.ast = nullSearchStmt(); }
| d::SearchDeclaration_c
  { top.ast = stmtSearchStmt(declStmt(d.ast)); }
| e::Expr_c ';'
  { top.ast = stmtSearchStmt(exprStmt(e.ast)); }
| '{{' b::BlockItemList_c '}' '}'
  { top.ast = stmtSearchStmt(compoundStmt(foldStmt(b.ast))); }
| '{' ss::SearchStmts_c '}'
  { top.ast = compoundSearchStmt(foldSeqSearchStmt(ss.ast)); }
| 'choice' '{' ss::SearchStmts_c '}'
  { top.ast = compoundSearchStmt(foldChoiceSearchStmt(ss.ast)); }
| 'if' '(' cond::Expr_c ')' tc::SearchStmt_c
  { top.ast = ifThenSearchStmt(cond.ast, tc.ast); }
| 'if' '(' cond::Expr_c ')' tc::SearchStmt_c 'else' ec::SearchStmt_c 
  { top.ast = ifThenElseSearchStmt(cond.ast, tc.ast, ec.ast); }

-- Mirrors Declaration_c, needed to avoid failing MDA by spilling follow set
closed nonterminal SearchDeclaration_c with location, ast<Decl>;
concrete productions top::SearchDeclaration_c
| ds::DeclarationSpecifiers_c  idcl::InitDeclaratorList_c  ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    local dcls :: Declarators =
      foldDeclarator(idcl.ast);
    
    top.ast = 
      if ds.isTypedef then
        if !null(ds.storageClass) then
          typedefDecls(ds.attributes, 
            warnTypeExpr(
              [err(ds.location, "Typedef declaration also claims another storage class")],
              bt),
            dcls)
        else
          typedefDecls(ds.attributes, bt, dcls)
      else
        variableDecls(ds.storageClass, ds.attributes, bt, dcls);
  }
  action {
    context =
      if ds.isTypedef
      then lh:addTypenamesToScope(idcl.declaredIdents, context)
      else lh:addIdentsToScope(idcl.declaredIdents, context);
  }
| ds::DeclarationSpecifiers_c  ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    top.ast =
      typeExprDecl(
        ds.attributes,
        figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers));
  }
