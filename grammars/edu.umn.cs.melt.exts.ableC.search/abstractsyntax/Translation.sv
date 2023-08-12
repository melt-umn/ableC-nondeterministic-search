grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

synthesized attribute asClosure::Expr;
synthesized attribute asStmt::Stmt;
synthesized attribute asStmtLazy::Stmt;
synthesized attribute asClosureRef::(Stmt, Stmt, Name);

data nonterminal Translation with asClosure, asStmt, asStmtLazy, asClosureRef;

abstract production stmtTranslation
top::Translation ::= s::Stmt
{
  attachNote extensionGenerated("ableC-nondeterministic-search");
  top.asStmt = s;
  top.asClosure =
    ableC_Expr {
      proto_typedef task_buffer_t;
      refcount::lambda (task_buffer_t *const _schedule) -> (void) {
        if (_cancelled == 0 || !*_cancelled) {
          $Stmt{s}
        }
      }
    };
  
  local closureTrans::Translation = closureTranslation(top.asClosure);
  top.asStmtLazy = closureTrans.asStmtLazy;
  top.asClosureRef = closureTrans.asClosureRef;
}

abstract production closureRefTranslation
top::Translation ::= n::Name
{
  attachNote extensionGenerated("ableC-nondeterministic-search");
  top.asClosure = declRefExpr(n);
  top.asStmt = ableC_Stmt { $Name{n}(_schedule); };
  top.asStmtLazy = ableC_Stmt { $Name{n}.add_ref(); put_task(_schedule, $Name{n}); };
  top.asClosureRef = (nullStmt(), nullStmt(), n);
}

abstract production closureTranslation
top::Translation ::= e::Expr
{
  attachNote extensionGenerated("ableC-nondeterministic-search");
  top.asClosure = e;
  local tmpId::String = "_task_" ++ toString(genInt());
  top.asStmt =
    ableC_Stmt {
      proto_typedef task_t;
      task_t $name{tmpId} = $Expr{e};
      $name{tmpId}(_schedule);
      $name{tmpId}.remove_ref();
    };
  top.asStmtLazy = ableC_Stmt { put_task(_schedule, $Expr{e}); };
  top.asClosureRef =
    (ableC_Stmt { proto_typedef task_t; task_t $name{tmpId} = $Expr{e}; },
     ableC_Stmt { $name{tmpId}.remove_ref(); },
     name(tmpId));
}
