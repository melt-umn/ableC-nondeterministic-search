grammar edu:umn:cs:melt:exts:ableC:search:concretesyntax;

-- These aren't marking terminals, but make them Global anyway for easier disambiguation from
-- other extension Scoped marking terminals
terminal Succeed_t 'succeed' lexer classes {Keyword, Global};
terminal Fail_t    'fail'    lexer classes {Keyword, Global};
terminal Spawn_t   'spawn'   lexer classes {Keyword, Global};
terminal Choice_t  'choice'  lexer classes {Keyword, Global};
terminal Choose_t  'choose'  lexer classes {Keyword, Global};
terminal Pick_t    'pick'    lexer classes {Keyword, Global};
terminal Finally_t 'finally' lexer classes {Keyword, Global}, precedence = 3, association = left;
terminal Require_t 'require' lexer classes {Keyword, Global};

-- For 'dangling-finally' resolution
terminal Prec1RCurly_t /}/ precedence = 1, action { context = tail(context); };

{-
 - For double-brace-enclosed host statments
 - We need to use two RCurly_t terminals from ableC to close the block to avoid MDA follow spillage
 - The lexer hack parser action for '}' closes a scope, so we need to open two scopes here to
 - balance two '}' terminals
 -}
terminal DoubleLBrace_t '{{'
  action { context = head(context) :: head(context) :: context; },
  lexer classes {Keyword};

tracked nonterminal SearchStmts_c with ast<[SearchStmt]>;
concrete productions top::SearchStmts_c
| h::SearchStmt_c t::SearchStmts_c
  { top.ast = h.ast :: t.ast; }
| 
  { top.ast = []; }

closed tracked nonterminal SearchStmt_c with ast<SearchStmt>;
concrete productions top::SearchStmt_c
| ';'
  { top.ast = nullSearchStmt(); }
| d::SearchDeclaration_c
  { top.ast = stmtSearchStmt(declStmt(d.ast)); }
| e::Expr_c ';'
  { top.ast = stmtSearchStmt(exprStmt(e.ast)); }
| 'while' '(' cond::Expr_c ')' '{' b::BlockItemList_c '}'
    { top.ast = stmtSearchStmt(whileStmt(cond.ast, compoundStmt(foldStmt(b.ast)))); }
| 'do' body::Stmt_c 'while' '(' cond::Expr_c ')'
    { top.ast = stmtSearchStmt(doStmt(body.ast, cond.ast)); }
-- Slightly odd syntactic construction: ExprStmt is "Expr;" or ";"
| 'for' '(' init::ExprStmt_c cond::ExprStmt_c iter::Expr_c ')' '{' b::BlockItemList_c '}'
    { top.ast = stmtSearchStmt(forStmt(init.asMaybeExpr, cond.asMaybeExpr, justExpr(iter.ast), compoundStmt(foldStmt(b.ast)))); }
| 'for' '(' init::ExprStmt_c cond::ExprStmt_c ')' '{' b::BlockItemList_c '}'
    { top.ast = stmtSearchStmt(forStmt(init.asMaybeExpr, cond.asMaybeExpr, nothingExpr(), compoundStmt(foldStmt(b.ast)))); }
-- Note that Declaration ends with ;
| 'for' '(' init::Declaration_c cond::ExprStmt_c iter::Expr_c ')' '{' b::BlockItemList_c '}'
    { top.ast = stmtSearchStmt(forDeclStmt(init.ast, cond.asMaybeExpr, justExpr(iter.ast), compoundStmt(foldStmt(b.ast)))); }
| 'for' '(' init::Declaration_c cond::ExprStmt_c ')' '{' b::BlockItemList_c '}'
    { top.ast = stmtSearchStmt(forDeclStmt(init.ast, cond.asMaybeExpr, nothingExpr(), compoundStmt(foldStmt(b.ast)))); }
| '{{' b::BlockItemList_c '}' '}'
  { top.ast = stmtSearchStmt(compoundStmt(foldStmt(b.ast))); }
| 'succeed' e::Expr_c ';'
  { top.ast = succeedSearchStmt(justExpr(e.ast)); }
| 'succeed' '(' e::Expr_c ')' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(succeedSearchStmt(justExpr(e.ast)), foldStmt(b.ast)); }
| 'succeed' ';'
  { top.ast = succeedSearchStmt(nothingExpr()); }
| 'succeed' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(succeedSearchStmt(nothingExpr()), foldStmt(b.ast)); }
| 'fail' ';'
  { top.ast = failSearchStmt(); }
| 'spawn' s::SearchStmt_c
  { top.ast = spawnSearchStmt(s.ast); }
| '{' ss::SearchStmts_c '}'
  { top.ast = compoundSearchStmt(foldSeqSearchStmt(ss.ast)); }
-- Optional 'finally' clause can't be factored out for these, unfourtunately, due to use of precedence
| 'choice' '{' ss::SearchStmts_c Prec1RCurly_t
  { top.ast = compoundSearchStmt(foldChoiceSearchStmt(ss.ast)); }
| 'choice' '{' ss::SearchStmts_c Prec1RCurly_t 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(compoundSearchStmt(foldChoiceSearchStmt(ss.ast)), foldStmt(b.ast)); }
| 'choice' 'for' '(' init::ExprStmt_c cond::ExprStmt_c iter::Expr_c ')' body::SearchStmt_c
  { top.ast = choiceForSearchStmt(init.asMaybeExpr, cond.asMaybeExpr, justExpr(iter.ast), body.ast); }
| 'choice' 'for' '(' init::ExprStmt_c cond::ExprStmt_c iter::Expr_c ')' body::SearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(choiceForSearchStmt(init.asMaybeExpr, cond.asMaybeExpr, justExpr(iter.ast), body.ast), foldStmt(b.ast)); }
| 'choice' 'for' '(' init::ExprStmt_c cond::ExprStmt_c ')' body::SearchStmt_c
  { top.ast = choiceForSearchStmt(init.asMaybeExpr, cond.asMaybeExpr, nothingExpr(), body.ast); }
| 'choice' 'for' '(' init::ExprStmt_c cond::ExprStmt_c ')' body::SearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(choiceForSearchStmt(init.asMaybeExpr, cond.asMaybeExpr, nothingExpr(), body.ast), foldStmt(b.ast)); }
| 'choice' 'for' '(' init::Declaration_c cond::ExprStmt_c iter::Expr_c ')' body::SearchStmt_c
  { top.ast = choiceForDeclSearchStmt(init.ast, cond.asMaybeExpr, justExpr(iter.ast), body.ast); }
| 'choice' 'for' '(' init::Declaration_c cond::ExprStmt_c iter::Expr_c ')' body::SearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(choiceForDeclSearchStmt(init.ast, cond.asMaybeExpr, justExpr(iter.ast), body.ast), foldStmt(b.ast)); }
| 'choice' 'for' '(' init::Declaration_c cond::ExprStmt_c ')' body::SearchStmt_c
  { top.ast = choiceForDeclSearchStmt(init.ast, cond.asMaybeExpr, nothingExpr(), body.ast); }
| 'choice' 'for' '(' init::Declaration_c cond::ExprStmt_c ')' body::SearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(choiceForDeclSearchStmt(init.ast, cond.asMaybeExpr, nothingExpr(), body.ast), foldStmt(b.ast)); }
| 'if' '(' cond::Expr_c ')' tc::SearchStmt_c
  { top.ast = ifThenSearchStmt(cond.ast, tc.ast); }
| 'if' '(' cond::Expr_c ')' tc::SearchStmt_c 'else' ec::SearchStmt_c 
  { top.ast = ifThenElseSearchStmt(cond.ast, tc.ast, ec.ast); }
| 'if' '(' cond::Expr_c ')' tc::SearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(ifThenSearchStmt(cond.ast, tc.ast), foldStmt(b.ast)); }
| 'if' '(' cond::Expr_c ')' tc::SearchStmt_c 'else' ec::SearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(ifThenElseSearchStmt(cond.ast, tc.ast, ec.ast), foldStmt(b.ast)); }
| 'choose' e::Expr_c ';'
  {
    local ast::Expr = e.ast;
    ast.env = emptyEnv();
    ast.controlStmtContext = initialControlStmtContext;
    top.ast =
      case ast of
        callExpr(declRefExpr(f), args) ->
          chooseSearchStmt(f, args)
      | eqExpr(lhs, callExpr(declRefExpr(f), args)) ->
          chooseAssignSearchStmt(lhs, f, args)
      | _ -> warnSearchStmt([errFromOrigin(e, "Invalid choose expression")])
      end;
  }
| 'choose' '(' e::Expr_c ')' 'finally' '{' b::BlockItemList_c '}'
  {
    local ast::Expr = e.ast;
    ast.env = emptyEnv();
    ast.controlStmtContext = initialControlStmtContext;
    top.ast =
      case ast of
        callExpr(declRefExpr(f), args) ->
          finallySearchStmt(chooseSearchStmt(f, args), foldStmt(b.ast))
      | eqExpr(lhs, callExpr(declRefExpr(f), args)) ->
          finallySearchStmt(chooseAssignSearchStmt(lhs, f, args), foldStmt(b.ast))
      | _ -> warnSearchStmt([errFromOrigin(e, "Invalid choose expression")])
      end;
  }
| 'choose' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' ')' ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast = chooseDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), nilExpr());
  }
| 'choose' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' args::ArgumentExprList_c ')' ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast = chooseDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), foldExpr(args.ast));
  }
| 'choose' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' ')' 'finally' '{' b::BlockItemList_c '}'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast =
      finallySearchStmt(chooseDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), nilExpr()), foldStmt(b.ast));
  }
| 'choose' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' args::ArgumentExprList_c ')' 'finally' '{' b::BlockItemList_c '}'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast =
      finallySearchStmt(chooseDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), foldExpr(args.ast)), foldStmt(b.ast));
  }
| 'choose' 'succeed' f::Identifier_t '(' ')' ';'
  { top.ast = chooseSucceedSearchStmt(fromId(f), nilExpr()); }
| 'choose' 'succeed' f::Identifier_t '(' args::ArgumentExprList_c ')' ';'
  { top.ast = chooseSucceedSearchStmt(fromId(f), foldExpr(args.ast)); }
| 'choose' 'succeed' f::Identifier_t '(' ')' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(chooseSucceedSearchStmt(fromId(f), nilExpr()), foldStmt(b.ast)); }
| 'choose' 'succeed' f::Identifier_t '(' args::ArgumentExprList_c ')' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(chooseSucceedSearchStmt(fromId(f), foldExpr(args.ast)), foldStmt(b.ast)); }
| 'pick' e::Expr_c ';'
  {
    local ast::Expr = e.ast;
    ast.env = emptyEnv();
    ast.controlStmtContext = initialControlStmtContext;
    top.ast =
      case ast of
        callExpr(declRefExpr(f), args) ->
          pickSearchStmt(f, args)
      | eqExpr(lhs, callExpr(declRefExpr(f), args)) ->
          pickAssignSearchStmt(lhs, f, args)
      | _ -> warnSearchStmt([errFromOrigin(e, "Invalid pick expression")])
      end;
  }
| 'pick' '(' e::Expr_c ')' 'finally' '{' b::BlockItemList_c '}'
  {
    local ast::Expr = e.ast;
    ast.env = emptyEnv();
    ast.controlStmtContext = initialControlStmtContext;
    top.ast =
      case ast of
        callExpr(declRefExpr(f), args) ->
          finallySearchStmt(pickSearchStmt(f, args), foldStmt(b.ast))
      | eqExpr(lhs, callExpr(declRefExpr(f), args)) ->
          finallySearchStmt(pickAssignSearchStmt(lhs, f, args), foldStmt(b.ast))
      | _ -> warnSearchStmt([errFromOrigin(e, "Invalid pick expression")])
      end;
  }
| 'pick' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' ')' ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast = pickDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), nilExpr());
  }
| 'pick' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' args::ArgumentExprList_c ')' ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast = pickDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), foldExpr(args.ast));
  }
| 'pick' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' ')' 'finally' '{' b::BlockItemList_c '}'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast =
      finallySearchStmt(pickDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), nilExpr()), foldStmt(b.ast));
  }
| 'pick' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' args::ArgumentExprList_c ')' 'finally' '{' b::BlockItemList_c '}'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast =
      finallySearchStmt(pickDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), foldExpr(args.ast)), foldStmt(b.ast));
  }
| 'pick' 'succeed' f::Identifier_t '(' ')' ';'
  { top.ast = pickSucceedSearchStmt(fromId(f), nilExpr()); }
| 'pick' 'succeed' f::Identifier_t '(' args::ArgumentExprList_c ')' ';'
  { top.ast = pickSucceedSearchStmt(fromId(f), foldExpr(args.ast)); }
| 'pick' 'succeed' f::Identifier_t '(' ')' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(pickSucceedSearchStmt(fromId(f), nilExpr()), foldStmt(b.ast)); }
| 'pick' 'succeed' f::Identifier_t '(' args::ArgumentExprList_c ')' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(pickSucceedSearchStmt(fromId(f), foldExpr(args.ast)), foldStmt(b.ast)); }
| 'require' c::Expr_c ';'
  { top.ast = requireSearchStmt(c.ast); }
| 'require' '(' c::Expr_c ')' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(requireSearchStmt(c.ast), foldStmt(b.ast)); }

-- Mirrors Declaration_c, needed to avoid failing MDA by spilling follow set
closed tracked nonterminal SearchDeclaration_c with ast<Decl>;
concrete productions top::SearchDeclaration_c
| ds::DeclarationSpecifiers_c  idcl::InitDeclaratorList_c  ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    local dcls :: Declarators =
      foldDeclarator(idcl.ast);
    
    top.ast = 
      if ds.isTypedef then
        if !null(ds.storageClass) then
          typedefDecls(ds.attributes, 
            warnTypeExpr(
              [errFromOrigin(ds, "Typedef declaration also claims another storage class")],
              bt),
            dcls)
        else
          typedefDecls(ds.attributes, bt, dcls)
      else
        variableDecls(foldStorageClass(ds.storageClass), ds.attributes, bt, dcls);
  }
  action {
    context =
      if ds.isTypedef
      then addIdentsToScope(idcl.declaredIdents, TypeName_t, context)
      else addIdentsToScope(idcl.declaredIdents, Identifier_t, context);
  }
| ds::DeclarationSpecifiers_c  ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    top.ast =
      typeExprDecl(
        ds.attributes,
        figureOutTypeFromSpecifiers(ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers));
  }
