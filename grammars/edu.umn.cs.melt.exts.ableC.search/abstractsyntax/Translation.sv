grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

synthesized attribute asStmt::Stmt;
synthesized attribute asStmtLazy::Stmt;
synthesized attribute asClosure::Expr;

synthesized attribute closureRef::Decorated Translation;
synthesized attribute closureRefInit::Stmt;

nonterminal Translation with asStmt, asStmtLazy, asClosure, closureRef, closureRefInit;

abstract production stmtTranslation_i
top::Translation ::= s::Stmt
{
  top.asStmt = s;
  
  forwards to
    closureTranslation_i(
      lambdaStmtExpr(
        nothingExpr(),
        nothingCaptureList(),
        consParameters(
          parameterDecl(
            [],
            typedefTypeExpr(nilQualifier(), name("task_buffer_t", location=builtin)),
            baseTypeExpr(),
            justName(name("_schedule", location=builtin)),
            nilAttribute()),
          nilParameters()),
        typeName(
          builtinTypeExpr(nilQualifier(), voidType()),
          baseTypeExpr()),
        s,
        location=builtin));
}

abstract production closureRefTranslation_i
top::Translation ::= n::Name
{
  top.closureRef = top;
  top.closureRefInit = nullStmt();

  forwards to closureTranslation_i(declRefExpr(n, location=builtin));
}

abstract production closureTranslation_i
top::Translation ::= e::Expr
{
  top.asStmt =
    exprStmt(
      applyExpr(
        e,
        consExpr(declRefExpr(name("_schedule", location=builtin), location=builtin), nilExpr()),
        location=builtin));
  top.asStmtLazy =
    exprStmt(
      directCallExpr(
        name("put_task", location=builtin),
        consExpr(
          declRefExpr(name("_schedule", location=builtin), location=builtin),
          consExpr(e, nilExpr())),
        location=builtin));
  top.asClosure = e;
  
  local tmpId::Name = name("_task_" ++ toString(genInt()), location=builtin);
  top.closureRef = closureRefTranslation(tmpId);
  top.closureRefInit =
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
          nilDeclarator())));
}

global stmtTranslation::(Decorated Translation ::= Stmt) = \ s::Stmt -> decorate stmtTranslation_i(s) with {};
global closureRefTranslation::(Decorated Translation ::= Name) = \ n::Name -> decorate closureRefTranslation_i(n) with {};
global closureTranslation::(Decorated Translation ::= Expr) = \ e::Expr -> decorate closureTranslation_i(e) with {};
