grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

-- Construction helpers
function foldSeqSearchStmt
SearchStmt ::= l::[SearchStmt]
{
  return if null(l) then nullSearchStmt(location=builtin) else foldr1(seqSearchStmt(_, _, location=builtin), l);
}

function foldChoiceSearchStmt
SearchStmt ::= l::[SearchStmt]
{
  return if null(l) then nullSearchStmt(location=builtin) else foldr1(choiceSearchStmt(_, _, location=builtin), l);
}
