grammar edu:umn:cs:melt:exts:ableC:search:concretesyntax;

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
| 'while' '(' cond::Expr_c ')' '{' b::BlockItemList_c '}'
    { top.ast = stmtSearchStmt(whileStmt(cond.ast, compoundStmt(foldStmt(b.ast))), location=top.location); }
| 'do' body::Stmt_c 'while' '(' cond::Expr_c ')'
    { top.ast = stmtSearchStmt(doStmt(body.ast, cond.ast), location=top.location); }
-- Slightly odd syntactic construction: ExprStmt is "Expr;" or ";"
| 'for' '(' init::ExprStmt_c cond::ExprStmt_c iter::Expr_c ')' '{' b::BlockItemList_c '}'
    { top.ast = stmtSearchStmt(forStmt(init.asMaybeExpr, cond.asMaybeExpr, justExpr(iter.ast), compoundStmt(foldStmt(b.ast))), location=top.location); }
| 'for' '(' init::ExprStmt_c cond::ExprStmt_c ')' '{' b::BlockItemList_c '}'
    { top.ast = stmtSearchStmt(forStmt(init.asMaybeExpr, cond.asMaybeExpr, nothingExpr(), compoundStmt(foldStmt(b.ast))), location=top.location); }
-- Note that Declaration ends with ;
| 'for' '(' init::Declaration_c cond::ExprStmt_c iter::Expr_c ')' '{' b::BlockItemList_c '}'
    { top.ast = stmtSearchStmt(forDeclStmt(init.ast, cond.asMaybeExpr, justExpr(iter.ast), compoundStmt(foldStmt(b.ast))), location=top.location); }
| 'for' '(' init::Declaration_c cond::ExprStmt_c ')' '{' b::BlockItemList_c '}'
    { top.ast = stmtSearchStmt(forDeclStmt(init.ast, cond.asMaybeExpr, nothingExpr(), compoundStmt(foldStmt(b.ast))), location=top.location); }
| '{{' b::BlockItemList_c '}' '}'
  { top.ast = stmtSearchStmt(compoundStmt(foldStmt(b.ast)), location=top.location); }
| 'succeed' e::Expr_c ';'
  { top.ast = succeedSearchStmt(justExpr(e.ast), location=top.location); }
| 'succeed' '(' e::Expr_c ')' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(succeedSearchStmt(justExpr(e.ast), location=top.location), foldStmt(b.ast), location=top.location); }
| 'succeed' ';'
  { top.ast = succeedSearchStmt(nothingExpr(), location=top.location); }
| 'succeed' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(succeedSearchStmt(nothingExpr(), location=top.location), foldStmt(b.ast), location=top.location); }
| 'fail' ';'
  { top.ast = failSearchStmt(location=top.location); }
| 'spawn' s::SearchStmt_c
  { top.ast = spawnSearchStmt(s.ast, location=top.location); }
| '{' ss::SearchStmts_c '}'
  { top.ast = compoundSearchStmt(foldSeqSearchStmt(ss.ast), location=top.location); }
-- Optional 'finally' clause can't be factored out for these, unfourtunately, due to use of precedence
| 'choice' '{' ss::SearchStmts_c Prec1RCurly_t
  { top.ast = compoundSearchStmt(foldChoiceSearchStmt(ss.ast), location=top.location); }
| 'choice' '{' ss::SearchStmts_c Prec1RCurly_t 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(compoundSearchStmt(foldChoiceSearchStmt(ss.ast), location=top.location), foldStmt(b.ast), location=top.location); }
| 'choice' 'for' '(' init::ExprStmt_c cond::ExprStmt_c iter::Expr_c ')' body::SearchStmt_c
  { top.ast = choiceForSearchStmt(init.asMaybeExpr, cond.asMaybeExpr, justExpr(iter.ast), body.ast, location=top.location); }
| 'choice' 'for' '(' init::ExprStmt_c cond::ExprStmt_c iter::Expr_c ')' body::SearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(choiceForSearchStmt(init.asMaybeExpr, cond.asMaybeExpr, justExpr(iter.ast), body.ast, location=top.location), foldStmt(b.ast), location=top.location); }
| 'choice' 'for' '(' init::ExprStmt_c cond::ExprStmt_c ')' body::SearchStmt_c
  { top.ast = choiceForSearchStmt(init.asMaybeExpr, cond.asMaybeExpr, nothingExpr(), body.ast, location=top.location); }
| 'choice' 'for' '(' init::ExprStmt_c cond::ExprStmt_c ')' body::SearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(choiceForSearchStmt(init.asMaybeExpr, cond.asMaybeExpr, nothingExpr(), body.ast, location=top.location), foldStmt(b.ast), location=top.location); }
| 'choice' 'for' '(' init::Declaration_c cond::ExprStmt_c iter::Expr_c ')' body::SearchStmt_c
  { top.ast = choiceForDeclSearchStmt(init.ast, cond.asMaybeExpr, justExpr(iter.ast), body.ast, location=top.location); }
| 'choice' 'for' '(' init::Declaration_c cond::ExprStmt_c iter::Expr_c ')' body::SearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(choiceForDeclSearchStmt(init.ast, cond.asMaybeExpr, justExpr(iter.ast), body.ast, location=top.location), foldStmt(b.ast), location=top.location); }
| 'choice' 'for' '(' init::Declaration_c cond::ExprStmt_c ')' body::SearchStmt_c
  { top.ast = choiceForDeclSearchStmt(init.ast, cond.asMaybeExpr, nothingExpr(), body.ast, location=top.location); }
| 'choice' 'for' '(' init::Declaration_c cond::ExprStmt_c ')' body::SearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(choiceForDeclSearchStmt(init.ast, cond.asMaybeExpr, nothingExpr(), body.ast, location=top.location), foldStmt(b.ast), location=top.location); }
| 'if' '(' cond::Expr_c ')' tc::SearchStmt_c
  { top.ast = ifThenSearchStmt(cond.ast, tc.ast, location=top.location); }
| 'if' '(' cond::Expr_c ')' tc::SearchStmt_c 'else' ec::SearchStmt_c 
  { top.ast = ifThenElseSearchStmt(cond.ast, tc.ast, ec.ast, location=top.location); }
| 'if' '(' cond::Expr_c ')' tc::SearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(ifThenSearchStmt(cond.ast, tc.ast, location=top.location), foldStmt(b.ast), location=top.location); }
| 'if' '(' cond::Expr_c ')' tc::SearchStmt_c 'else' ec::SearchStmt_c 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(ifThenElseSearchStmt(cond.ast, tc.ast, ec.ast, location=top.location), foldStmt(b.ast), location=top.location); }
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
| 'choose' '(' e::Expr_c ')' 'finally' '{' b::BlockItemList_c '}'
  {
    local ast::Expr = e.ast;
    ast.env = emptyEnv();
    ast.returnType = nothing();
    top.ast =
      case ast of
        callExpr(declRefExpr(f), args) ->
          finallySearchStmt(chooseSearchStmt(f, args, location=top.location), foldStmt(b.ast), location=top.location)
      | eqExpr(lhs, callExpr(declRefExpr(f), args)) ->
          finallySearchStmt(chooseAssignSearchStmt(lhs, f, args, location=top.location), foldStmt(b.ast), location=top.location)
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
| 'choose' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' ')' 'finally' '{' b::BlockItemList_c '}'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast =
      finallySearchStmt(chooseDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), nilExpr(), location=top.location), foldStmt(b.ast), location=top.location);
  }
| 'choose' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' args::ArgumentExprList_c ')' 'finally' '{' b::BlockItemList_c '}'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast =
      finallySearchStmt(chooseDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), foldExpr(args.ast), location=top.location), foldStmt(b.ast), location=top.location);
  }
| 'choose' 'succeed' f::Identifier_t '(' ')' ';'
  { top.ast = chooseSucceedSearchStmt(fromId(f), nilExpr(), location=top.location); }
| 'choose' 'succeed' f::Identifier_t '(' args::ArgumentExprList_c ')' ';'
  { top.ast = chooseSucceedSearchStmt(fromId(f), foldExpr(args.ast), location=top.location); }
| 'choose' 'succeed' f::Identifier_t '(' ')' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(chooseSucceedSearchStmt(fromId(f), nilExpr(), location=top.location), foldStmt(b.ast), location=top.location); }
| 'choose' 'succeed' f::Identifier_t '(' args::ArgumentExprList_c ')' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(chooseSucceedSearchStmt(fromId(f), foldExpr(args.ast), location=top.location), foldStmt(b.ast), location=top.location); }
| 'pick' e::Expr_c ';'
  {
    local ast::Expr = e.ast;
    ast.env = emptyEnv();
    ast.returnType = nothing();
    top.ast =
      case ast of
        callExpr(declRefExpr(f), args) ->
          pickSearchStmt(f, args, location=top.location)
      | eqExpr(lhs, callExpr(declRefExpr(f), args)) ->
          pickAssignSearchStmt(lhs, f, args, location=top.location)
      | _ -> warnSearchStmt([err(e.location, "Invalid pick expression")], location=top.location)
      end;
  }
| 'pick' '(' e::Expr_c ')' 'finally' '{' b::BlockItemList_c '}'
  {
    local ast::Expr = e.ast;
    ast.env = emptyEnv();
    ast.returnType = nothing();
    top.ast =
      case ast of
        callExpr(declRefExpr(f), args) ->
          finallySearchStmt(pickSearchStmt(f, args, location=top.location), foldStmt(b.ast), location=top.location)
      | eqExpr(lhs, callExpr(declRefExpr(f), args)) ->
          finallySearchStmt(pickAssignSearchStmt(lhs, f, args, location=top.location), foldStmt(b.ast), location=top.location)
      | _ -> warnSearchStmt([err(e.location, "Invalid pick expression")], location=top.location)
      end;
  }
| 'pick' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' ')' ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast = pickDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), nilExpr(), location=top.location);
  }
| 'pick' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' args::ArgumentExprList_c ')' ';'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast = pickDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), foldExpr(args.ast), location=top.location);
  }
| 'pick' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' ')' 'finally' '{' b::BlockItemList_c '}'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast =
      finallySearchStmt(pickDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), nilExpr(), location=top.location), foldStmt(b.ast), location=top.location);
  }
| 'pick' ds::DeclarationSpecifiers_c d::Declarator_c '=' f::Identifier_t '(' args::ArgumentExprList_c ')' 'finally' '{' b::BlockItemList_c '}'
  {
    ds.givenQualifiers = ds.typeQualifiers;
    local bt :: BaseTypeExpr =
      figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers);
    
    d.givenType = baseTypeExpr();
    
    top.ast =
      finallySearchStmt(pickDeclSearchStmt(bt, d.ast, d.declaredIdent, fromId(f), foldExpr(args.ast), location=top.location), foldStmt(b.ast), location=top.location);
  }
| 'pick' 'succeed' f::Identifier_t '(' ')' ';'
  { top.ast = pickSucceedSearchStmt(fromId(f), nilExpr(), location=top.location); }
| 'pick' 'succeed' f::Identifier_t '(' args::ArgumentExprList_c ')' ';'
  { top.ast = pickSucceedSearchStmt(fromId(f), foldExpr(args.ast), location=top.location); }
| 'pick' 'succeed' f::Identifier_t '(' ')' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(pickSucceedSearchStmt(fromId(f), nilExpr(), location=top.location), foldStmt(b.ast), location=top.location); }
| 'pick' 'succeed' f::Identifier_t '(' args::ArgumentExprList_c ')' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(pickSucceedSearchStmt(fromId(f), foldExpr(args.ast), location=top.location), foldStmt(b.ast), location=top.location); }
| 'require' c::Expr_c ';'
  { top.ast = requireSearchStmt(c.ast, location=top.location); }
| 'require' '(' c::Expr_c ')' 'finally' '{' b::BlockItemList_c '}'
  { top.ast = finallySearchStmt(requireSearchStmt(c.ast, location=top.location), foldStmt(b.ast), location=top.location); }

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
        figureOutTypeFromSpecifiers(ds.location, ds.typeQualifiers, ds.preTypeSpecifiers, ds.realTypeSpecifiers, ds.mutateTypeSpecifiers));
  }
