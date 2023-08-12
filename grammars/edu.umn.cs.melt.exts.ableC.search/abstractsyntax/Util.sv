grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

-- Construction helpers
function foldSeqSearchStmt
SearchStmt ::= l::[SearchStmt]
{
  return if null(l) then nullSearchStmt() else foldr1(seqSearchStmt(_, _), l);
}

function foldChoiceSearchStmt
SearchStmt ::= l::[SearchStmt]
{
  return if null(l) then nullSearchStmt() else foldr1(choiceSearchStmt(_, _), l);
}
