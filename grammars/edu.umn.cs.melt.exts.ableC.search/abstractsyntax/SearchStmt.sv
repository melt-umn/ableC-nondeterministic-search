grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

autocopy attribute expectedResultType::Type;

synthesized attribute seqs::[Decorated SearchStmt];
synthesized attribute choices::[Decorated SearchStmt];

inherited attribute nextTranslation::Decorated Translation;
synthesized attribute translation::Decorated Translation;

synthesized attribute hasContinuation::Boolean; -- true if translation wrapps nextTranslation in a closure

nonterminal SearchStmt with env, expectedResultType, nextTranslation, pp, seqs, choices, errors, defs, translation, hasContinuation;

aspect default production
top::SearchStmt ::=
{
  top.seqs = [top];
  top.choices = [top];
}

abstract production warnSearchStmt
top::SearchStmt ::= msg::[Message]
{
  top.pp = pp"/*err*/";
  top.errors := msg;
  top.defs := [];
  top.translation = stmtTranslation(warnStmt(msg));
  top.hasContinuation = false;
}

abstract production nullSearchStmt
top::SearchStmt ::= 
{
  top.pp = notext();
  top.errors := [];
  top.defs := [];
  top.translation = top.nextTranslation;
  top.hasContinuation = false;
}

abstract production compoundSearchStmt
top::SearchStmt ::= s::SearchStmt
{
  top.pp = notext();
  top.errors := [];
  top.defs := [];
  
  local directTranslation::Stmt = compoundStmt(s.translation.asStmt);
  top.translation =
    stmtTranslation(
      if top.hasContinuation
      then seqStmt(top.nextTranslation.asClosureRef.fst, directTranslation)
      else seqStmt(directTranslation, top.nextTranslation.asStmt));
  s.nextTranslation =
    if top.hasContinuation
    then closureRefTranslation(top.nextTranslation.asClosureRef.snd)
    else stmtTranslation(nullStmt());
  
  top.hasContinuation = s.hasContinuation;
}

abstract production stmtSearchStmt
top::SearchStmt ::= s::Stmt
{
  top.pp = s.pp;
  top.errors := s.errors;
  top.defs := s.defs;
  top.translation = stmtTranslation(seqStmt(s, top.nextTranslation.asStmt));
  top.hasContinuation = false;
  
  s.returnType = nothing();
}

abstract production succeedSearchStmt
top::SearchStmt ::= me::MaybeExpr
{
  top.pp = pp"succeed ${me.pp};";
  top.errors := me.errors;
  local actualTyperep::Type = fromMaybe(builtinType(nilQualifier(), voidType()), me.maybeTyperep);
  top.errors <-
    if !compatibleTypes(top.expectedResultType, actualTyperep, true, false)
    then [err(case me of justExpr(e) -> e.location | _ -> txtLoc("TODO location") end, s"Invalid result type (expected ${showType(top.expectedResultType)}, got ${showType(actualTyperep)})")]
    else [];
  
  top.defs := [];
  top.translation =
    stmtTranslation(
      substStmt(
        [declRefSubstitution("__result__", me.justTheExpr.fromJust)],
        parseStmt(s"_continuation(${if me.isJust then "__result__" else ""});")));
  top.hasContinuation = false;
}

abstract production failSearchStmt
top::SearchStmt ::= 
{
  top.pp = pp"fail;";
  top.errors := [];
  top.defs := [];
  top.translation = stmtTranslation(nullStmt());
  top.hasContinuation = false;
}

abstract production seqSearchStmt
top::SearchStmt ::= h::SearchStmt t::SearchStmt
{
  top.pp = pp"seq {${nestlines(2, ppImplode(line(), map((.pp), top.seqs)))}}";
  top.seqs = h.seqs ++ t.seqs;
  top.errors := h.errors ++ t.errors;
  top.defs :=
    (if t.hasContinuation then foldr(consDefs, nilDefs(), h.defs).asCaptured else h.defs) ++
    t.defs;
  
  top.translation = h.translation;
  h.nextTranslation = t.translation;
  t.nextTranslation = top.nextTranslation;
  
  top.hasContinuation = h.hasContinuation || t.hasContinuation;
  
  h.expectedResultType = builtinType(nilQualifier(), voidType());
  
  t.env = addEnv(h.defs, if h.hasContinuation then h.env.asCaptured else h.env);
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
  
  top.hasContinuation = true;
}

abstract production ifThenSearchStmt
top::SearchStmt ::= c::Expr t::SearchStmt
{
  top.pp = pp"if (${c.pp} {${cat(line(), nestlines(2, t.pp))}}";
  forwards to ifThenElseSearchStmt(c, t, succeedSearchStmt(nothingExpr()));
}

abstract production ifThenElseSearchStmt
top::SearchStmt ::= c::Expr t::SearchStmt e::SearchStmt
{
  top.pp = pp"if (${c.pp} {${cat(line(), nestlines(2, t.pp))}} else {${cat(line(), nestlines(2, t.pp))}}";
  top.errors := c.errors ++ t.errors ++ e.errors;
  top.errors <-
    if c.typerep.defaultFunctionArrayLvalueConversion.isScalarType then []
    else [err(c.location, "If condition must be scalar type, instead it is " ++ showType(c.typerep))];
  top.defs := [];
  
  local directTranslation::Stmt = ifStmt(c, t.translation.asStmt, e.translation.asStmt);
  top.translation =
    stmtTranslation(
      if top.hasContinuation
      then seqStmt(top.nextTranslation.asClosureRef.fst, directTranslation)
      else seqStmt(directTranslation, top.nextTranslation.asStmt));
  t.nextTranslation =
    if top.hasContinuation
    then closureRefTranslation(top.nextTranslation.asClosureRef.snd)
    else stmtTranslation(nullStmt());
  e.nextTranslation = t.nextTranslation;
  
  top.hasContinuation = t.hasContinuation || e.hasContinuation;
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
    then [err(f.location, s"Incompatible type in amb var declaration (expected ${showType(bty.typerep)}, got ${showType(f.searchFunctionItem.resultType)})")]
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
  top.hasContinuation = true;
  
  bty.givenRefId = nothing();
  mty.baseType = bty.typerep;
  mty.typeModifiersIn = bty.typeModifiers;
}
