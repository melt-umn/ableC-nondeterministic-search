grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

autocopy attribute expectedResultType::Type;

synthesized attribute seqs::[Decorated SearchStmt];
synthesized attribute choices::[Decorated SearchStmt];

inherited attribute nextTranslation::Decorated Translation;
synthesized attribute translation::Decorated Translation;

nonterminal SearchStmt with env, expectedResultType, nextTranslation, pp, seqs, choices, errors, defs, translation;

aspect default production
top::SearchStmt ::=
{
  top.seqs = [top];
  top.choices = [top];
}

abstract production nullSearchStmt
top::SearchStmt ::= 
{
  top.pp = notext();
  top.errors := [];
  top.defs := [];
  top.translation = top.nextTranslation;
}

abstract production compoundSearchStmt
top::SearchStmt ::= s::SearchStmt
{
  top.pp = notext();
  top.errors := [];
  top.defs := [];
  
  top.translation =
    stmtTranslation(
      seqStmt(
        top.nextTranslation.closureRefInit,
        compoundStmt(s.translation.asStmt)));
  s.nextTranslation = top.nextTranslation.closureRef;
}

abstract production stmtSearchStmt
top::SearchStmt ::= s::Stmt
{
  top.pp = s.pp;
  top.errors := s.errors;
  top.defs := s.defs;
  top.translation = stmtTranslation(seqStmt(s, top.nextTranslation.asStmt));
  
  s.returnType = nothing();
}

abstract production seqSearchStmt
top::SearchStmt ::= h::SearchStmt t::SearchStmt
{
  top.pp = pp"seq {${nestlines(2, ppImplode(line(), map((.pp), top.seqs)))}}";
  top.seqs = h.seqs ++ t.seqs;
  top.errors := h.errors ++ t.errors;
  top.defs := h.defs ++ t.defs;
  
  top.translation = h.translation;
  h.nextTranslation = t.translation;
  t.nextTranslation = top.nextTranslation;
  
  h.expectedResultType = builtinType(nilQualifier(), voidType());
}

abstract production choiceSearchStmt
top::SearchStmt ::= h::SearchStmt t::SearchStmt
{
  top.pp = pp"choice {${nestlines(2, ppImplode(line(), map((.pp), top.choices)))}}";
  top.choices = h.choices ++ t.choices;
  top.errors := h.errors ++ t.errors;
  top.defs := [];
  
  top.translation =
    stmtTranslation(
      foldStmt(
        top.nextTranslation.closureRefInit ::
        map((.asStmtLazy), map((.translation), top.choices))));
  h.nextTranslation = top.nextTranslation.closureRef;
  t.nextTranslation = h.nextTranslation;
}

abstract production ambVarSearchStmt
top::SearchStmt ::= bty::BaseTypeExpr mty::TypeModifierExpr n::Name f::Name a::Exprs
{
  top.pp = pp"amb ${bty.pp} ${mty.lpp}${n.pp}${mty.rpp} = ${f.pp}(${ppImplode(pp", ", a.pps)});";
  top.errors := bty.errors ++ mty.errors ++ a.errors;
  -- n.searchFunctionRedeclarationCheck ++ f.searchFunctionLookupCheck ++ 
  
  production d::Declarator = declarator(n, mty, nilAttribute(), nothingInitializer());
  d.env = top.env;
  d.baseType = bty.typerep;
  d.typeModifiersIn = bty.typeModifiers;
  d.isTopLevel = false;
  d.isTypedef = false;
  d.givenAttributes = nilAttribute();
  d.returnType = nothing();
  
  top.defs := [];
  
  top.translation =
    stmtTranslation(
      exprStmt(
        directCallExpr(
          n,
          consExpr(
            case d.typerep of
              builtinType(_, voidType()) -> top.nextTranslation.asClosure
            | _ -> error("Not implemented")
            end, a),
          location=builtin)));
}
