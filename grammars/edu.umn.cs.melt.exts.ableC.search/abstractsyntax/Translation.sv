grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

synthesized attribute asClosure::Expr;
synthesized attribute asStmt::Stmt;
synthesized attribute asStmtLazy::Stmt;
synthesized attribute asClosureRef::Pair<Stmt Name>;

nonterminal Translation with asClosure, asStmt, asStmtLazy, asClosureRef;

abstract production stmtTranslation_i
top::Translation ::= s::Stmt
{
  top.asStmt = s;
  
  forwards to
    closureTranslation_i(
      substExpr(
        [stmtSubstitution("__body__", s)],
        parseExpr(s"({proto_typedef task_buffer_t; lambda (task_buffer_t *const _schedule) -> (void) { __body__; };})")));
}

abstract production closureRefTranslation_i
top::Translation ::= n::Name
{
  top.asClosureRef = pair(nullStmt(), n);

  forwards to closureTranslation_i(declRefExpr(n, location=builtin));
}

abstract production closureTranslation_i
top::Translation ::= e::Expr
{
  top.asClosure = e;
  top.asStmt =
    exprStmt(
      applyExpr(
        e,
        foldExpr([declRefExpr(name("_schedule", location=builtin), location=builtin)]),
        location=builtin));
  top.asStmtLazy =
    exprStmt(
      directCallExpr(
        name("put_task", location=builtin),
        foldExpr([declRefExpr(name("_schedule", location=builtin), location=builtin), e]),
        location=builtin));
  local tmpId::Name = name("_task_" ++ toString(genInt()), location=builtin);
  top.asClosureRef =
    pair(
      declStmt(
        variableDecls(
          [], nilAttribute(),
          typedefTypeExpr(nilQualifier(), name("task_t", location=builtin)),
          consDeclarator(
            declarator(
              tmpId,
              baseTypeExpr(),
              nilAttribute(),
              justInitializer(exprInitializer(e))),
            nilDeclarator()))),
    tmpId);
}

global stmtTranslation::(Decorated Translation ::= Stmt) = \ s::Stmt -> decorate stmtTranslation_i(s) with {};
global closureRefTranslation::(Decorated Translation ::= Name) = \ n::Name -> decorate closureRefTranslation_i(n) with {};
global closureTranslation::(Decorated Translation ::= Expr) = \ e::Expr -> decorate closureTranslation_i(e) with {};
