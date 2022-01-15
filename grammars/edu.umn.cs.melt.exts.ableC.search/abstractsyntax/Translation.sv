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
      ableC_Expr {
        proto_typedef task_buffer_t;
        refcount::lambda (task_buffer_t *const _schedule) -> (void) {
          if (_cancelled == 0 || !*_cancelled) {
            $Stmt{s}
          }
        }
      });
}

abstract production closureRefTranslation_i
top::Translation ::= n::Name
{
  top.asClosure = declRefExpr(n, location=builtin);
  top.asStmt = ableC_Stmt { $Name{n}(_schedule); };
  top.asStmtLazy = ableC_Stmt { $Name{n}.add_ref(); put_task(_schedule, $Name{n}); };
  top.asClosureRef = pair(pair(nullStmt(), nullStmt()), n);
}

abstract production closureTranslation_i
top::Translation ::= e::Expr
{
  top.asClosure = e;
  local tmpId::String = "_task_" ++ toString(genIntT());
  top.asStmt =
    ableC_Stmt {
      proto_typedef task_t;
      task_t $name{tmpId} = $Expr{e};
      $name{tmpId}(_schedule);
      $name{tmpId}.remove_ref();
    };
  top.asStmtLazy = ableC_Stmt { put_task(_schedule, $Expr{e}); };
  top.asClosureRef =
    pair(
      pair(
        ableC_Stmt { proto_typedef task_t; task_t $name{tmpId} = $Expr{e}; },
        ableC_Stmt { $name{tmpId}.remove_ref(); }),
      name(tmpId, location=builtin));
}

global stmtTranslation::(Decorated Translation ::= Stmt) = \ s::Stmt -> decorate stmtTranslation_i(s) with {};
global closureRefTranslation::(Decorated Translation ::= Name) = \ n::Name -> decorate closureRefTranslation_i(n) with {};
global closureTranslation::(Decorated Translation ::= Expr) = \ e::Expr -> decorate closureTranslation_i(e) with {};
