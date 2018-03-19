grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

autocopy attribute expectedResultType::Type;

synthesized attribute seqPPs::[Document];
synthesized attribute choicePPs::[Document];
synthesized attribute seqs::[Decorated SearchStmt];
synthesized attribute choices::[Decorated SearchStmt];

inherited attribute nextTranslation::Decorated Translation;
synthesized attribute translation::Decorated Translation;

-- true if flow of control is passed elsewhere by translation - i.e. nextTranslation is NOT simply
-- appended with seqStmt
synthesized attribute hasContinuation::Boolean;

nonterminal SearchStmt with env, expectedResultType, nextTranslation, substitutions, pp, seqPPs, choicePPs, substituted<SearchStmt>, seqs, choices, errors, defs, translation, hasContinuation, location;
flowtype SearchStmt = decorate {env, expectedResultType, nextTranslation}, pp {}, seqPPs {}, choicePPs {}, substituted {substitutions}, seqs {decorate}, choices {decorate}, errors {decorate}, defs {decorate}, translation {decorate}, hasContinuation {decorate};

aspect default production
top::SearchStmt ::=
{
  top.seqs = [top];
  top.choices = [top];
  top.seqPPs = [top.pp];
  top.choicePPs = [top.pp];
}

abstract production warnSearchStmt
top::SearchStmt ::= msg::[Message]
{
  propagate substituted;
  top.pp = pp"/*err*/";
  top.errors := msg;
  top.defs := [];
  top.translation = stmtTranslation(warnStmt(msg));
  top.hasContinuation = false;
}

abstract production nullSearchStmt
top::SearchStmt ::= 
{
  propagate substituted;
  top.pp = notext();
  top.errors := [];
  top.defs := [];
  top.translation = top.nextTranslation;
  top.hasContinuation = false;
}

abstract production compoundSearchStmt
top::SearchStmt ::= s::SearchStmt
{
  propagate substituted;
  top.pp = braces(nestlines(2, ppImplode(line(), top.seqPPs)));
  top.errors := s.errors;
  top.defs := [];
  
  local directTranslation::Stmt = compoundStmt(s.translation.asStmt);
  top.translation =
    stmtTranslation(
      if s.hasContinuation
      then
        foldStmt([
          top.nextTranslation.asClosureRef.fst.fst,
          directTranslation,
          top.nextTranslation.asClosureRef.fst.snd])
      else seqStmt(directTranslation, top.nextTranslation.asStmt));
  s.nextTranslation =
    if s.hasContinuation
    then closureRefTranslation(top.nextTranslation.asClosureRef.snd)
    else stmtTranslation(nullStmt());
  top.hasContinuation = s.hasContinuation;
}

abstract production stmtSearchStmt
top::SearchStmt ::= s::Stmt
{
  propagate substituted;
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
  propagate substituted;
  top.pp = pp"succeed ${me.pp};";
  top.errors := me.errors;
  top.errors <-
    case top.expectedResultType, fromMaybe(builtinType(nilQualifier(), voidType()), me.maybeTyperep) of
      builtinType(nilQualifier(), voidType()), builtinType(nilQualifier(), voidType()) -> []
    | expectedType, actualType ->
      if !typeAssignableTo(expectedType, actualType)
      then [err(top.location, s"Incompatible result type (expected ${showType(expectedType)}, got ${showType(actualType)})")]
      else []
    end;
  
  top.defs := [];
  top.translation =
    stmtTranslation(
      substStmt(
        [declRefSubstitution("__result__", me.justTheExpr.fromJust)],
        parseStmt(s"_continuation(${if me.isJust then "__result__" else ""});")));
  top.hasContinuation = true;
  
  me.returnType = nothing();
}

abstract production failSearchStmt
top::SearchStmt ::= 
{
  propagate substituted;
  top.pp = pp"fail;";
  top.errors := [];
  top.defs := [];
  top.translation = stmtTranslation(nullStmt());
  top.hasContinuation = true;
}

abstract production seqSearchStmt
top::SearchStmt ::= h::SearchStmt t::SearchStmt
{
  propagate substituted;
  top.pp = braces(nestlines(2, ppImplode(line(), top.seqPPs)));
  top.seqPPs = h.seqPPs ++ t.seqPPs;
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
  propagate substituted;
  top.pp = pp"choice {${nestlines(2, ppImplode(line(), top.choicePPs))}}";
  top.choicePPs = h.choicePPs ++ t.choicePPs;
  top.choices = h.choices ++ t.choices;
  top.errors := h.errors ++ t.errors;
  top.defs := [];
  
  top.translation =
    stmtTranslation(
      foldStmt(
        top.nextTranslation.asClosureRef.fst.fst ::
        openFrameStmt ::
        map((.asStmtLazy), map((.translation), top.choices)) ++
        [top.nextTranslation.asClosureRef.fst.snd]));
  h.nextTranslation = closureRefTranslation(top.nextTranslation.asClosureRef.snd);
  t.nextTranslation = h.nextTranslation;
  
  top.hasContinuation = true;
}

abstract production choiceForSearchStmt
top::SearchStmt ::= i::MaybeExpr  c::MaybeExpr  s::MaybeExpr  b::SearchStmt
{
  propagate substituted;
  top.pp = 
    ppConcat([text("choice for"), parens(ppConcat([i.pp, semi(), space(), c.pp, semi(), space(), s.pp])), line(),
      braces(nestlines(2, b.pp)) ]);
  top.errors := i.errors ++ c.errors ++ s.errors ++ b.errors;
  top.defs := [];
  
  top.translation =
    stmtTranslation(
      foldStmt(
        [top.nextTranslation.asClosureRef.fst.fst,
         openFrameStmt,
         forStmt(i, c, s, b.translation.asStmt),
         top.nextTranslation.asClosureRef.fst.snd]));
  b.nextTranslation = closureRefTranslation(top.nextTranslation.asClosureRef.snd);
  
  top.hasContinuation = true;
  
  i.env = openScopeEnv(top.env);
  c.env = addEnv(i.defs, i.env);
  s.env = addEnv(c.defs, c.env);
  b.env = addEnv(s.defs, s.env);
  i.returnType = nothing();
  c.returnType = nothing();
  s.returnType = nothing();
}

abstract production choiceForDeclSearchStmt
top::SearchStmt ::= i::Decl  c::MaybeExpr  s::MaybeExpr  b::SearchStmt
{
  propagate substituted;
  top.pp = 
    ppConcat([text("choice for"), parens(ppConcat([i.pp, semi(), space(), c.pp, semi(), space(), s.pp])), line(),
      braces(nestlines(2, b.pp)) ]);
  top.errors := i.errors ++ c.errors ++ s.errors ++ b.errors;
  top.defs := [];
  
  top.translation =
    stmtTranslation(
      foldStmt(
        [top.nextTranslation.asClosureRef.fst.fst,
         openFrameStmt,
         forDeclStmt(i, c, s, b.translation.asStmt),
         top.nextTranslation.asClosureRef.fst.snd]));
  b.nextTranslation = closureRefTranslation(top.nextTranslation.asClosureRef.snd);
  
  top.hasContinuation = true;
  
  i.env = openScopeEnv(top.env);
  c.env = addEnv(i.defs, i.env);
  s.env = addEnv(c.defs, c.env);
  b.env = addEnv(s.defs, s.env);
  i.isTopLevel = false;
  i.returnType = nothing();
  c.returnType = nothing();
  s.returnType = nothing();
}

abstract production finallySearchStmt
top::SearchStmt ::= s::SearchStmt f::Stmt
{
  propagate substituted;
  top.pp = pp"${s.pp} finally ${f.pp}";
  top.errors := s.errors ++ f.errors;
  top.defs := s.defs ++ f.defs;
  
  top.translation = stmtTranslation(seqStmt(s.translation.asStmt, f));
  s.nextTranslation = top.nextTranslation;
  
  top.hasContinuation = s.hasContinuation;
  
  f.env = addEnv(s.defs, s.env);
  f.returnType = nothing();
}

abstract production ifThenSearchStmt
top::SearchStmt ::= c::Expr t::SearchStmt
{
  propagate substituted;
  top.pp = pp"if (${c.pp} {${cat(line(), nestlines(2, t.pp))}}";
  forwards to ifThenElseSearchStmt(c, t, nullSearchStmt(location=top.location), location=top.location);
}

abstract production ifThenElseSearchStmt
top::SearchStmt ::= c::Expr t::SearchStmt e::SearchStmt
{
  propagate substituted;
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
      then
        foldStmt([
          top.nextTranslation.asClosureRef.fst.fst,
          directTranslation,
          top.nextTranslation.asClosureRef.fst.snd])
      else seqStmt(directTranslation, top.nextTranslation.asStmt));
  t.nextTranslation =
    if top.hasContinuation
    then closureRefTranslation(top.nextTranslation.asClosureRef.snd)
    else stmtTranslation(nullStmt());
  e.nextTranslation = t.nextTranslation;
  top.hasContinuation = t.hasContinuation || e.hasContinuation;
  
  c.returnType = nothing();
}

abstract production chooseSearchStmt
top::SearchStmt ::= f::Name a::Exprs
{
  propagate substituted;
  top.pp = pp"choose ${f.pp}(${ppImplode(pp", ", a.pps)});";
  top.seqPPs = [top.pp];
  top.choicePPs = [top.pp];
  forwards to
    chooseDeclSearchStmt(
      directTypeExpr(f.searchFunctionItem.resultType),
      baseTypeExpr(),
      name(s"_result_${toString(genInt())}", location=builtin),
      f, a,
      location=top.location);
}

abstract production chooseAssignSearchStmt
top::SearchStmt ::= lhs::Expr f::Name a::Exprs
{
  propagate substituted;
  top.pp = pp"choose ${lhs.pp} = ${f.pp}(${ppImplode(pp", ", a.pps)});";
  top.seqPPs = [top.pp];
  top.choicePPs = [top.pp];
  
  local resultName::Name = name(s"_result_${toString(genInt())}", location=builtin);
  forwards to
    seqSearchStmt(
      chooseDeclSearchStmt(
        directTypeExpr(f.searchFunctionItem.resultType),
        baseTypeExpr(),
        resultName,
        f, a,
        location=top.location),
      stmtSearchStmt(
        exprStmt(eqExpr(lhs, declRefExpr(resultName, location=builtin), location=top.location)),
        location=top.location),
      location=top.location);
}

abstract production chooseDeclSearchStmt
top::SearchStmt ::= bty::BaseTypeExpr mty::TypeModifierExpr id::Name f::Name a::Exprs
{
  propagate substituted;
  top.pp = pp"choose ${bty.pp} ${mty.lpp}${id.pp}${mty.rpp} = ${f.pp}(${ppImplode(pp", ", a.pps)});";
  top.errors := bty.errors ++ mty.errors ++ a.errors;
  top.errors <- id.valueRedeclarationCheckNoCompatible;
  top.errors <- f.searchFunctionLookupCheck;
  top.errors <-
    case d.typerep, f.searchFunctionItem.resultType of
      builtinType(nilQualifier(), voidType()), builtinType(nilQualifier(), voidType()) -> []
    | expectedType, actualType ->
      if !typeAssignableTo(expectedType, actualType)
      then [err(f.location, s"Incompatible type in choose declaration (expected ${showType(expectedType)}, got ${showType(actualType)})")]
      else []
    end;
  top.errors <- if null(f.searchFunctionLookupCheck) then a.argumentErrors else [];
  
  top.defs := d.defs;
  
  top.translation =
    stmtTranslation(
      substStmt(
        [typedefSubstitution("__result_type__", typeModifierTypeExpr(bty, mty)),
         stmtSubstitution("__body__", top.nextTranslation.asStmt),
         exprsSubstitution("__args__", a)],
        parseStmt(s"""
_search_function_${f.name}(
  _schedule,
  lambda (${case d.typerep of
              builtinType(_, voidType()) -> "void"
            | _ -> s"__result_type__ ${id.name}"
            end}) -> (void) { __body__; },
  __args__);""")));
  top.hasContinuation = true;
  
  production d::Declarator = declarator(id, mty, nilAttribute(), nothingInitializer());
  d.env = top.env;
  d.baseType = bty.typerep;
  d.typeModifiersIn = bty.typeModifiers;
  d.isTopLevel = false;
  d.isTypedef = false;
  d.givenAttributes = nilAttribute();
  d.returnType = nothing();
  
  bty.givenRefId = nothing();
  bty.returnType = nothing();
  mty.baseType = bty.typerep;
  mty.typeModifiersIn = bty.typeModifiers;
  mty.returnType = nothing();
  a.returnType = nothing();
  a.expectedTypes = f.searchFunctionItem.parameterTypes;
  a.argumentPosition = 1;
  a.callExpr = decorate declRefExpr(f, location=f.location) with {env = top.env; returnType = nothing();};
  a.callVariadic = false;
}

abstract production chooseSucceedSearchStmt
top::SearchStmt ::= f::Name a::Exprs
{
  propagate substituted;
  top.pp = pp"choose succeed ${f.pp}(${ppImplode(pp", ", a.pps)});";
  top.seqPPs = [top.pp];
  top.choicePPs = [top.pp];
  top.errors := a.errors;
  top.errors <- f.searchFunctionLookupCheck;
  top.errors <-
    case top.expectedResultType, f.searchFunctionItem.resultType of
      builtinType(nilQualifier(), voidType()), builtinType(nilQualifier(), voidType()) -> []
    | expectedType, actualType ->
      if !typeAssignableTo(expectedType, actualType)
      then [err(top.location, s"Incompatible result type (expected ${showType(expectedType)}, got ${showType(actualType)})")]
      else []
    end;
  
  top.defs := [];
  
  top.translation =
    stmtTranslation(
      substStmt(
        [exprsSubstitution("__args__", a)],
        parseStmt(s"""
_continuation.add_ref();
_search_function_${f.name}(_schedule, _continuation, __args__);
""")));
  top.hasContinuation = true;
  
  a.returnType = nothing();
  a.expectedTypes = f.searchFunctionItem.parameterTypes;
  a.argumentPosition = 1;
  a.callExpr = decorate declRefExpr(f, location=f.location) with {env = top.env; returnType = nothing();};
  a.callVariadic = false;
}

abstract production requireSearchStmt
top::SearchStmt ::= c::Expr
{
  propagate substituted;
  top.pp = pp"require ${c.pp};";
  top.errors := c.errors;
  top.errors <-
    if c.typerep.defaultFunctionArrayLvalueConversion.isScalarType then []
    else [err(c.location, "require condition must be scalar type, instead it is " ++ showType(c.typerep))];
  top.defs := c.defs;
  top.translation = stmtTranslation(ifStmtNoElse(c, top.nextTranslation.asStmt));
  top.hasContinuation = false;
  
  c.returnType = nothing();
}

global openFrameStmt::Stmt = parseStmt("open_frame(_schedule);");
