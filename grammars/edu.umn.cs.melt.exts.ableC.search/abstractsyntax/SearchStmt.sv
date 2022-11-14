grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

inherited attribute expectedResultType::Type;

synthesized attribute seqPPs::[Document];
synthesized attribute choicePPs::[Document];
synthesized attribute seqs::[Decorated SearchStmt];
synthesized attribute choices::[Decorated SearchStmt];

inherited attribute nextTranslation::Decorated Translation;
synthesized attribute translation::Decorated Translation;

-- true if flow of control is passed elsewhere by translation - i.e. nextTranslation is NOT simply
-- appended with seqStmt
synthesized attribute hasContinuation::Boolean;

nonterminal SearchStmt with env, expectedResultType, nextTranslation, pp, seqPPs, choicePPs, seqs, choices, errors, defs, translation, hasContinuation, location;
flowtype SearchStmt = decorate {env, expectedResultType, nextTranslation}, pp {}, seqPPs {}, choicePPs {}, seqs {decorate}, choices {decorate}, errors {decorate}, defs {decorate}, translation {decorate}, hasContinuation {decorate};

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
  propagate defs;
  top.pp = pp"/*err*/";
  top.errors := msg;
  top.translation = stmtTranslation(warnStmt(msg));
  top.hasContinuation = false;
}

abstract production nullSearchStmt
top::SearchStmt ::= 
{
  propagate defs, errors;
  top.pp = notext();
  top.translation = top.nextTranslation;
  top.hasContinuation = false;
}

abstract production compoundSearchStmt
top::SearchStmt ::= s::SearchStmt
{
  propagate env, expectedResultType, errors;
  top.pp = braces(nestlines(2, ppImplode(line(), top.seqPPs)));
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
  propagate defs, errors;
  top.pp = s.pp;
  top.translation = stmtTranslation(seqStmt(s, top.nextTranslation.asStmt));
  top.hasContinuation = false;
  
  s.env = addEnv(s.functionDefs, top.env);
  s.controlStmtContext = controlStmtContext(nothing(), false, false, tm:add(s.labelDefs, tm:empty()));
}

abstract production succeedSearchStmt
top::SearchStmt ::= me::MaybeExpr
{
  propagate env, errors;
  top.pp = pp"succeed ${me.pp};";
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
    if me.isJust
    then stmtTranslation(ableC_Stmt { _continuation(_schedule, $Expr{me.justTheExpr.fromJust}); })
    else closureRefTranslation(name("_continuation", location=builtin));
  top.hasContinuation = true;
  
  me.controlStmtContext = initialControlStmtContext;
}

abstract production failSearchStmt
top::SearchStmt ::= 
{
  propagate defs, errors;
  top.pp = pp"fail;";
  top.translation = stmtTranslation(nullStmt());
  top.hasContinuation = true;
}

abstract production spawnSearchStmt
top::SearchStmt ::= s::SearchStmt
{
  propagate env, expectedResultType, errors;
  top.pp = ppConcat([pp"spawn", line(), nestlines(2, ppImplode(line(), top.seqPPs))]);
  top.defs := [];
  
  top.translation = stmtTranslation(s.translation.asStmtLazy);
  s.nextTranslation = top.nextTranslation;
  top.hasContinuation = true;
}

abstract production seqSearchStmt
top::SearchStmt ::= h::SearchStmt t::SearchStmt
{
  propagate errors;
  top.pp = braces(nestlines(2, ppImplode(line(), top.seqPPs)));
  top.seqPPs = h.seqPPs ++ t.seqPPs;
  top.seqs = h.seqs ++ t.seqs;
  top.defs :=
    (if t.hasContinuation then foldr(consDefs, nilDefs(), h.defs).asCaptured else h.defs) ++
    t.defs;
  
  top.translation = h.translation;
  h.nextTranslation = t.translation;
  t.nextTranslation = top.nextTranslation;
  
  top.hasContinuation = h.hasContinuation || t.hasContinuation;
  
  h.env = top.env;
  h.expectedResultType = builtinType(nilQualifier(), voidType());
  
  t.env = addEnv(h.defs, if h.hasContinuation then h.env.asCaptured else h.env);
  t.expectedResultType = top.expectedResultType;
}

abstract production choiceSearchStmt
top::SearchStmt ::= h::SearchStmt t::SearchStmt
{
  propagate env, expectedResultType, errors;
  top.pp = pp"choice {${nestlines(2, ppImplode(line(), top.choicePPs))}}";
  top.choicePPs = h.choicePPs ++ t.choicePPs;
  top.choices = h.choices ++ t.choices;
  top.defs := [];
  
  top.translation =
    stmtTranslation(
      foldStmt(
        top.nextTranslation.asClosureRef.fst.fst ::
        map((.asStmt), map((.translation), top.choices)) ++
        [top.nextTranslation.asClosureRef.fst.snd]));
  h.nextTranslation = closureRefTranslation(top.nextTranslation.asClosureRef.snd);
  t.nextTranslation = h.nextTranslation;
  
  top.hasContinuation = true;
}

abstract production choiceForSearchStmt
top::SearchStmt ::= i::MaybeExpr  c::MaybeExpr  s::MaybeExpr  b::SearchStmt
{
  propagate expectedResultType, errors;
  top.pp = 
    ppConcat([text("choice for"), parens(ppConcat([i.pp, semi(), space(), c.pp, semi(), space(), s.pp])), line(),
      braces(nestlines(2, b.pp)) ]);
  top.defs := [];
  
  top.translation =
    stmtTranslation(
      foldStmt(
        [top.nextTranslation.asClosureRef.fst.fst,
         forStmt(i, c, s, b.translation.asStmt),
         top.nextTranslation.asClosureRef.fst.snd]));
  b.nextTranslation = closureRefTranslation(top.nextTranslation.asClosureRef.snd);
  
  top.hasContinuation = true;
  
  i.env = openScopeEnv(top.env);
  c.env = addEnv(i.defs, i.env);
  s.env = addEnv(c.defs, c.env);
  b.env = addEnv(s.defs, s.env);
  
  i.controlStmtContext = initialControlStmtContext;
  c.controlStmtContext = initialControlStmtContext;
  s.controlStmtContext = initialControlStmtContext;
}

abstract production choiceForDeclSearchStmt
top::SearchStmt ::= i::Decl  c::MaybeExpr  s::MaybeExpr  b::SearchStmt
{
  propagate expectedResultType, errors;
  top.pp = 
    ppConcat([text("choice for"), parens(ppConcat([i.pp, semi(), space(), c.pp, semi(), space(), s.pp])), line(),
      braces(nestlines(2, b.pp)) ]);
  top.defs := [];
  
  top.translation =
    stmtTranslation(
      foldStmt(
        [top.nextTranslation.asClosureRef.fst.fst,
         forDeclStmt(i, c, s, b.translation.asStmt),
         top.nextTranslation.asClosureRef.fst.snd]));
  b.nextTranslation = closureRefTranslation(top.nextTranslation.asClosureRef.snd);
  
  top.hasContinuation = true;
  
  i.env = openScopeEnv(top.env);
  c.env = addEnv(i.defs, i.env);
  s.env = addEnv(c.defs, c.env);
  b.env = addEnv(s.defs, s.env);
  i.isTopLevel = false;
  
  i.controlStmtContext = initialControlStmtContext;
  c.controlStmtContext = initialControlStmtContext;
  s.controlStmtContext = initialControlStmtContext;
}

abstract production finallySearchStmt
top::SearchStmt ::= s::SearchStmt f::Stmt
{
  propagate expectedResultType, defs, errors;
  top.pp = pp"${s.pp} finally ${f.pp}";
  
  top.translation = stmtTranslation(seqStmt(s.translation.asStmt, f));
  s.nextTranslation = top.nextTranslation;
  
  top.hasContinuation = s.hasContinuation;
  
  s.env = top.env;
  f.env = addEnv(s.defs, s.env);
  f.controlStmtContext = initialControlStmtContext;
}

abstract production ifThenSearchStmt
top::SearchStmt ::= c::Expr t::SearchStmt
{
  top.pp = pp"if (${c.pp} {${cat(line(), nestlines(2, t.pp))}}";
  forwards to ifThenElseSearchStmt(c, t, nullSearchStmt(location=top.location), location=top.location);
}

abstract production ifThenElseSearchStmt
top::SearchStmt ::= c::Expr t::SearchStmt e::SearchStmt
{
  propagate env, expectedResultType, errors;
  top.pp = pp"if (${c.pp} {${cat(line(), nestlines(2, t.pp))}} else {${cat(line(), nestlines(2, t.pp))}}";
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
  
  c.controlStmtContext = initialControlStmtContext;
}

abstract production chooseSearchStmt
top::SearchStmt ::= f::Name a::Exprs
{
  propagate env;
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
  propagate env;
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
  propagate env, errors;
  top.pp = pp"choose ${bty.pp} ${mty.lpp}${id.pp}${mty.rpp} = ${f.pp}(${ppImplode(pp", ", a.pps)});";
  top.errors <- id.valueRedeclarationCheckNoCompatible;
  top.errors <- f.searchFunctionLookupCheck;
  top.errors <-
    case d.typerep, f.searchFunctionItem.resultType of
      builtinType(nilQualifier(), voidType()), builtinType(nilQualifier(), voidType()) -> []
    | expectedType, actualType ->
      if !compatibleTypes(expectedType, actualType, false, false)
      then [err(f.location, s"Incompatible type in choose declaration (expected ${showType(expectedType)}, got ${showType(actualType)})")]
      else []
    end;
  top.errors <- if null(f.searchFunctionLookupCheck) then a.argumentErrors else [];
  
  top.defs := d.defs;
  
  top.translation =
    stmtTranslation(
      ableC_Stmt {
        proto_typedef task_buffer_t;
        $name{"_search_function_" ++ f.name}(
          _schedule,
          refcount::lambda (
            task_buffer_t *const _schedule,
            $Parameters{
              case d.typerep of
                builtinType(_, voidType()) -> nilParameters()
              | _ -> ableC_Parameters { $BaseTypeExpr{typeModifierTypeExpr(bty, mty)} $Name{id} }
              end}) -> (void) {
            if (_cancelled == 0 || !*_cancelled) {
              $Stmt{top.nextTranslation.asStmt}
            }
          },
          _cancelled,
          $Exprs{a});
      });
  top.hasContinuation = true;
  
  production d::Declarator = declarator(id, mty, nilAttribute(), nothingInitializer());
  d.env = top.env;
  d.baseType = bty.typerep;
  d.typeModifierIn = bty.typeModifier;
  d.isTopLevel = false;
  d.isTypedef = false;
  d.givenStorageClasses = nilStorageClass();
  d.givenAttributes = nilAttribute();
  d.controlStmtContext = initialControlStmtContext;
  
  bty.givenRefId = nothing();
  bty.controlStmtContext = initialControlStmtContext;
  mty.baseType = bty.typerep;
  mty.typeModifierIn = bty.typeModifier;
  mty.controlStmtContext = initialControlStmtContext;

  a.controlStmtContext = initialControlStmtContext;
  a.expectedTypes = f.searchFunctionItem.parameterTypes;
  a.argumentPosition = 1;
  a.callExpr = decorate declRefExpr(f, location=f.location) with {env = top.env; controlStmtContext = initialControlStmtContext; };
  a.callVariadic = false;
}

abstract production chooseSucceedSearchStmt
top::SearchStmt ::= f::Name a::Exprs
{
  propagate env, errors;
  top.pp = pp"choose succeed ${f.pp}(${ppImplode(pp", ", a.pps)});";
  top.seqPPs = [top.pp];
  top.choicePPs = [top.pp];
  top.errors <- f.searchFunctionLookupCheck;
  top.errors <-
    case top.expectedResultType, f.searchFunctionItem.resultType of
      builtinType(nilQualifier(), voidType()), builtinType(nilQualifier(), voidType()) -> []
    | expectedType, actualType ->
      if !compatibleTypes(expectedType, actualType, false, false)
      then [err(top.location, s"Incompatible result type (expected ${showType(expectedType)}, got ${showType(actualType)})")]
      else []
    end;
  
  top.defs := [];
  
  top.translation =
    stmtTranslation(
      ableC_Stmt {
        _continuation.add_ref();
        $name{"_search_function_" ++ f.name}(_schedule, _continuation, _cancelled, $Exprs{a});
      });
  top.hasContinuation = true;
  
  a.controlStmtContext = initialControlStmtContext;
  a.expectedTypes = f.searchFunctionItem.parameterTypes;
  a.argumentPosition = 1;
  a.callExpr = decorate declRefExpr(f, location=f.location) with {env = top.env; controlStmtContext = initialControlStmtContext; };
  a.callVariadic = false;
}

abstract production pickSearchStmt
top::SearchStmt ::= f::Name a::Exprs
{
  propagate env;
  top.pp = pp"pick ${f.pp}(${ppImplode(pp", ", a.pps)});";
  top.seqPPs = [top.pp];
  top.choicePPs = [top.pp];
  forwards to
    pickDeclSearchStmt(
      directTypeExpr(f.searchFunctionItem.resultType),
      baseTypeExpr(),
      name(s"_result_${toString(genInt())}", location=builtin),
      f, a,
      location=top.location);
}

abstract production pickSucceedSearchStmt
top::SearchStmt ::= f::Name a::Exprs
{
  propagate env;
  top.pp = pp"pick succeed ${f.pp}(${ppImplode(pp", ", a.pps)});";
  top.seqPPs = [top.pp];
  top.choicePPs = [top.pp];
  
  local resultName::Name = name(s"_result_${toString(genInt())}", location=builtin);
  forwards to
    seqSearchStmt(
      pickDeclSearchStmt(
        directTypeExpr(f.searchFunctionItem.resultType),
        baseTypeExpr(),
        resultName,
        f, a,
        location=top.location),
      succeedSearchStmt(justExpr(declRefExpr(resultName, location=builtin)), location=top.location),
      location=top.location);
}

abstract production pickAssignSearchStmt
top::SearchStmt ::= lhs::Expr f::Name a::Exprs
{
  propagate env;
  top.pp = pp"pick ${lhs.pp} = ${f.pp}(${ppImplode(pp", ", a.pps)});";
  top.seqPPs = [top.pp];
  top.choicePPs = [top.pp];
  
  local resultName::Name = name(s"_result_${toString(genInt())}", location=builtin);
  forwards to
    seqSearchStmt(
      pickDeclSearchStmt(
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

abstract production pickDeclSearchStmt
top::SearchStmt ::= bty::BaseTypeExpr mty::TypeModifierExpr id::Name f::Name a::Exprs
{
  propagate env, errors;
  top.pp = pp"pick ${bty.pp} ${mty.lpp}${id.pp}${mty.rpp} = ${f.pp}(${ppImplode(pp", ", a.pps)});";
  top.errors <- id.valueRedeclarationCheckNoCompatible;
  top.errors <- f.searchFunctionLookupCheck;
  top.errors <-
    case d.typerep, f.searchFunctionItem.resultType of
      builtinType(nilQualifier(), voidType()), builtinType(nilQualifier(), voidType()) -> []
    | expectedType, actualType ->
      if !compatibleTypes(expectedType, actualType, false, false)
      then [err(f.location, s"Incompatible type in pick declaration (expected ${showType(expectedType)}, got ${showType(actualType)})")]
      else []
    end;
  top.errors <- if null(f.searchFunctionLookupCheck) then a.argumentErrors else [];
  
  top.defs := d.defs;
  
  local pickId::String = s"_pick_${toString(genInt())}";
  top.translation =
    stmtTranslation(
      ableC_Stmt {
        proto_typedef refcount_tag_t, pick_status_t, task_buffer_t;
        
        pick_status_t $name{pickId};
        //fprintf(stderr, "Allocating ${pickId}\n");
        refcount_tag_t $name{"_rt" ++ pickId} = pick_status_init(&$name{pickId});
        //_rt${pickId}->name = "${pickId}";
        
        $name{"_search_function_" ++ f.name}(
          _schedule,
          refcount::lambda [$name{"_rt" ++ pickId}, ...](
            task_buffer_t *const _schedule,
                    $Parameters{
                      case d.typerep of
                        builtinType(_, voidType()) -> nilParameters()
                      | _ -> ableC_Parameters { $BaseTypeExpr{typeModifierTypeExpr(bty, mty)} $Name{id} }
                      end}) -> (void) {
            if (try_pick($name{pickId}) && (_cancelled == 0 || !*_cancelled)) {
              $Stmt{top.nextTranslation.asStmt}
            }
          },
          &($name{pickId}->cancelled),
          $Exprs{a});
        
        remove_ref($name{"_rt" ++ pickId});
      });
  top.hasContinuation = true;
  
  production d::Declarator = declarator(id, mty, nilAttribute(), nothingInitializer());
  d.env = top.env;
  d.baseType = bty.typerep;
  d.typeModifierIn = bty.typeModifier;
  d.isTopLevel = false;
  d.isTypedef = false;
  d.givenStorageClasses = nilStorageClass();
  d.givenAttributes = nilAttribute();
  d.controlStmtContext = initialControlStmtContext;
  
  bty.givenRefId = nothing();
  bty.controlStmtContext = initialControlStmtContext;
  mty.baseType = bty.typerep;
  mty.typeModifierIn = bty.typeModifier;
  mty.controlStmtContext = initialControlStmtContext;
  a.controlStmtContext = initialControlStmtContext;
  a.expectedTypes = f.searchFunctionItem.parameterTypes;
  a.argumentPosition = 1;
  a.callExpr = decorate declRefExpr(f, location=f.location) with {env = top.env; controlStmtContext = initialControlStmtContext; };
  a.callVariadic = false;
}

abstract production requireSearchStmt
top::SearchStmt ::= c::Expr
{
  propagate env, errors, defs;
  top.pp = pp"require ${c.pp};";
  top.errors <-
    if c.typerep.defaultFunctionArrayLvalueConversion.isScalarType then []
    else [err(c.location, "require condition must be scalar type, instead it is " ++ showType(c.typerep))];
  top.translation = stmtTranslation(ifStmtNoElse(c, top.nextTranslation.asStmt));
  top.hasContinuation = false;
  
  c.controlStmtContext = initialControlStmtContext;
}
