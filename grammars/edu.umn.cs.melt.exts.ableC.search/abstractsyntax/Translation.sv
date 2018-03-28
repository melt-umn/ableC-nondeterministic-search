grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

synthesized attribute asClosure::Expr;
synthesized attribute asStmt::Stmt;
synthesized attribute asStmtLazy::Stmt;
synthesized attribute asClosureRef::Pair<Pair<Stmt Stmt> Name>;

nonterminal Translation with asClosure, asStmt, asStmtLazy, asClosureRef;

abstract production stmtTranslation_i
top::Translation ::= s::Stmt
{
  top.asStmt = s;
  
  forwards to
    closureTranslation_i(
      substExpr(
        [stmtSubstitution("__body__", s)],
        parseExpr(s"""
({proto_typedef task_buffer_t;
  lambda (task_buffer_t *const _schedule) -> (void) {
    if (_cancelled == 0 || !*_cancelled)
      __body__;
    };})""")));
}

abstract production closureRefTranslation_i
top::Translation ::= n::Name
{
  top.asClosure = declRefExpr(n, location=builtin);
  top.asStmt = parseStmt(s"${n.name}(_schedule);");
  top.asStmtLazy = parseStmt(s"${n.name}.add_ref(); put_task(_schedule, ${n.name});");
  top.asClosureRef = pair(pair(nullStmt(), nullStmt()), n);
}

abstract production closureTranslation_i
top::Translation ::= e::Expr
{
  top.asClosure = e;
  local tmpId::String = "_task_" ++ toString(genInt());
  top.asStmt =
    substStmt(
      [declRefSubstitution("__e__", e)],
      parseStmt(s"""
proto_typedef task_t;
task_t ${tmpId} = __e__;
${tmpId}(_schedule);
${tmpId}.remove_ref();"""));
  top.asStmtLazy =
    substStmt([declRefSubstitution("__e__", e)], parseStmt(s"put_task(_schedule, __e__);"));
  top.asClosureRef =
    pair(
      pair(
        substStmt(
          [declRefSubstitution("__e__", e)],
          parseStmt(s"proto_typedef task_t; task_t ${tmpId} = __e__;")),
        parseStmt(s"${tmpId}.remove_ref();")),
      name(tmpId, location=builtin));
}

global stmtTranslation::(Decorated Translation ::= Stmt) = \ s::Stmt -> decorate stmtTranslation_i(s) with {};
global closureRefTranslation::(Decorated Translation ::= Name) = \ n::Name -> decorate closureRefTranslation_i(n) with {};
global closureTranslation::(Decorated Translation ::= Expr) = \ e::Expr -> decorate closureTranslation_i(e) with {};
