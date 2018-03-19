grammar edu:umn:cs:melt:exts:ableC:search:concretesyntax;

terminal Choice_t  'choice'  lexer classes {Ckeyword};
terminal Succeed_t 'succeed' lexer classes {Ckeyword};
terminal Fail_t    'fail'    lexer classes {Ckeyword};
terminal Choose_t  'choose'  lexer classes {Ckeyword};
terminal Finally_t 'finally' lexer classes {Ckeyword};
terminal Require_t 'require' lexer classes {Ckeyword};

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
| h::SearchStmt_c t::SearchStmts_c
  { top.ast = h.ast :: t.ast; }
| 
  { top.ast = []; }

nonterminal SearchStmt_c with ast<SearchStmt>, location;
concrete productions top::SearchStmt_c
| ';'
  { top.ast = nullSearchStmt(location=top.location); }
| d::SearchDeclaration_c
  { top.ast = stmtSearchStmt(declStmt(d.ast), location=top.location); }
| e::Expr_c ';'
  { top.ast = stmtSearchStmt(exprStmt(e.ast), location=top.location); }
| '{{' b::BlockItemList_c '}' '}'
  { top.ast = stmtSearchStmt(compoundStmt(foldStmt(b.ast)), location=top.location); }
| 'succeed' e::Expr_c ';'
  { top.ast = succeedSearchStmt(justExpr(e.ast), location=top.location); }
| 'succeed' ';'
  { top.ast = succeedSearchStmt(nothingExpr(), location=top.location); }
| 'fail' ';'
  { top.ast = failSearchStmt(location=top.location); }
| '{' ss::SearchStmts_c '}'
  { top.ast = compoundSearchStmt(foldSeqSearchStmt(ss.ast), location=top.location); }
| css::ChoiceSearchStmt_c
  { top.ast = css.ast; }
| css::ChoiceSearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(css.ast, foldStmt(b.ast), location=top.location); }
| 'if' '(' cond::Expr_c ')' tc::SearchStmt_c
  { top.ast = ifThenSearchStmt(cond.ast, tc.ast, location=top.location); }
| 'if' '(' cond::Expr_c ')' tc::SearchStmt_c 'else' ec::SearchStmt_c 
  { top.ast = ifThenElseSearchStmt(cond.ast, tc.ast, ec.ast, location=top.location); }
| 'choose' e::Expr_c ';'
  {
    local ast::Expr = e.ast;
    ast.env = emptyEnv();
    ast.returnType = nothing();
    top.ast =
      case ast of
        callExpr(declRefExpr(f), args) ->
          chooseSearchStmt(f, args, location=top.location)
      | eqExpr(lhs, callExpr(declRefExpr(f), args)) ->
          chooseAssignSearchStmt(lhs, f, args, location=top.location)
      | _ -> warnSearchStmt([err(e.location, "Invalid choose expression")], location=top.location)
      end;
  }
| 'choose' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' ')' ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast = chooseDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), nilExpr(), location=top.location);
  }
| 'choose' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' args::ArgumentExprList_c ')' ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast = chooseDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), foldExpr(args.ast), location=top.location);
  }
| 'choose' 'succeed' f::Identifier_t '(' ')' ';'
  { top.ast = chooseSucceedSearchStmt(fromId(f), nilExpr(), location=top.location); }
| 'choose' 'succeed' f::Identifier_t '(' args::ArgumentExprList_c ')' ';'
  { top.ast = chooseSucceedSearchStmt(fromId(f), foldExpr(args.ast), location=top.location); }
| 'require' c::Expr_c ';'
  { top.ast = requireSearchStmt(c.ast, location=top.location); }

nonterminal ChoiceSearchStmt_c with ast<SearchStmt>, location;
concrete productions top::ChoiceSearchStmt_c
| 'choice' '{' ss::SearchStmts_c '}'
  { top.ast = compoundSearchStmt(foldChoiceSearchStmt(ss.ast), location=top.location); }
| 'choice' 'for' '(' init::ExprStmt_c cond::ExprStmt_c iter::Expr_c ')' '{' body::SearchStmts_c '}'
  {
    top.ast =
      choiceForSearchStmt(
        init.asMaybeExpr, cond.asMaybeExpr, justExpr(iter.ast),
        compoundSearchStmt(foldChoiceSearchStmt(body.ast), location=top.location),
        location=top.location);
  }
| 'choice' 'for' '(' init::ExprStmt_c cond::ExprStmt_c ')' '{' body::SearchStmts_c '}'
  {
    top.ast =
      choiceForSearchStmt(
        init.asMaybeExpr, cond.asMaybeExpr, nothingExpr(),
        compoundSearchStmt(foldChoiceSearchStmt(body.ast), location=top.location),
        location=top.location);
  }
| 'choice' 'for' '(' init::Declaration_c cond::ExprStmt_c iter::Expr_c ')' '{' body::SearchStmts_c '}'
  {
    top.ast =
      choiceForDeclSearchStmt(
        init.ast, cond.asMaybeExpr, justExpr(iter.ast),
        compoundSearchStmt(foldChoiceSearchStmt(body.ast), location=top.location),
        location=top.location);
  }
| 'choice' 'for' '(' init::Declaration_c cond::ExprStmt_c ')' '{' body::SearchStmts_c '}'
  {
    top.ast =
      choiceForDeclSearchStmt(
        init.ast, cond.asMaybeExpr, nothingExpr(),
        compoundSearchStmt(foldChoiceSearchStmt(body.ast), location=top.location),
        location=top.location);
  }

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
