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
        top.nextTranslation.asClosureRef.fst,
        compoundStmt(s.translation.asStmt)));
  s.nextTranslation = closureRefTranslation(top.nextTranslation.asClosureRef.snd);
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
        top.nextTranslation.asClosureRef.fst ::
        map((.asStmtLazy), map((.translation), top.choices))));
  h.nextTranslation = closureRefTranslation(top.nextTranslation.asClosureRef.snd);
  t.nextTranslation = h.nextTranslation;
}

abstract production ambSearchStmt
top::SearchStmt ::= f::Name a::Exprs
{
  top.pp = pp"amb ${f.pp}(${ppImplode(pp", ", a.pps)});";
  forwards to
    ambVarSearchStmt(
      directTypeExpr(f.searchFunctionItem.resultType),
      baseTypeExpr(),
      name(s"_result_${toString(genInt())}", location=builtin),
      f, a);
}

abstract production ambVarSearchStmt
top::SearchStmt ::= bty::BaseTypeExpr mty::TypeModifierExpr id::Name f::Name a::Exprs
{
  top.pp = pp"amb ${bty.pp} ${mty.lpp}${id.pp}${mty.rpp} = ${f.pp}(${ppImplode(pp", ", a.pps)});";
  top.errors := bty.errors ++ mty.errors ++ a.errors;
  top.errors <- id.valueRedeclarationCheckNoCompatible;
  top.errors <- f.searchFunctionLookupCheck;
  top.errors <-
    if !compatibleTypes(bty.typerep, f.searchFunctionItem.resultType, true, false)
    then [err(f.location, s"Incompatible type in amb var declaration (expected ${showType(bty.typerep)}, got ${showType(f.searchFunctionItem.resultType)}")]
    else [];
  top.errors <- a.argumentErrors;
  
  top.defs := [];
  
  top.translation =
    stmtTranslation(
      substStmt(
        [typedefSubstitution("__result_type__", typeModifierTypeExpr(bty, mty)),
         stmtSubstitution("__body__", top.nextTranslation.asStmt),
         exprsSubstitution("__args__", a)],
        parseStmt(s"""
_search_function${f.name}(
  _schedule,
  lambda (${case bty.typerep of
              builtinType(_, voidType()) -> ""
            | _ -> s", __result_type__ ${id.name}"
            end}) -> void { __body__; },
  __args__);""")));
  
  bty.givenRefId = nothing();
  mty.baseType = bty.typerep;
  mty.typeModifiersIn = bty.typeModifiers;
}
