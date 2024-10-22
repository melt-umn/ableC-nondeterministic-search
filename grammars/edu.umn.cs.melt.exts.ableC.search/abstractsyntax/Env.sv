grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

nonterminal SearchFunctionItem with parameterTypes, resultType;

abstract production searchFunctionItem
top::SearchFunctionItem ::= f::Decorated SearchFunctionDecl
{
  top.parameterTypes = f.parameterTypes;
  top.resultType = f.resultType;
}

abstract production errorSearchFunctionItem
top::SearchFunctionItem ::=
{
  top.parameterTypes = [];
  top.resultType = errorType();
}

-- Global search function env
synthesized attribute searchFunctions::Scopes<SearchFunctionItem> occurs on Env;
synthesized attribute searchFunctionContribs::Contribs<SearchFunctionItem> occurs on Defs, Def;

aspect production emptyEnv_i
top::Env ::=
{
  top.searchFunctions = emptyScope();
}
aspect production addEnv_i
top::Env ::= d::Defs  e::Decorated Env
{
  top.searchFunctions = addGlobalScope(gd.searchFunctionContribs, addScope(d.searchFunctionContribs, e.searchFunctions));
}
aspect production openScopeEnv_i
top::Env ::= e::Decorated Env
{
  top.searchFunctions = openScope(e.searchFunctions);
}
aspect production globalEnv_i
top::Env ::= e::Decorated Env
{
  top.searchFunctions = globalScope(e.searchFunctions);
}
aspect production nonGlobalEnv_i
top::Env ::= e::Decorated Env
{
  top.searchFunctions = nonGlobalScope(e.searchFunctions);
}
aspect production functionEnv_i
top::Env ::= e::Decorated Env
{
  top.searchFunctions = functionScope(e.searchFunctions);
}

aspect production nilDefs
top::Defs ::=
{
  top.searchFunctionContribs = [];
}
aspect production consDefs
top::Defs ::= h::Def  t::Defs
{
  top.searchFunctionContribs = h.searchFunctionContribs ++ t.searchFunctionContribs;
}

aspect default production
top::Def ::=
{
  top.searchFunctionContribs = [];
}

abstract production searchFunctionDef
top::Def ::= s::String  t::SearchFunctionItem
{
  top.searchFunctionContribs = [(s, t)];
}

-- General convinence stuff with Name
synthesized attribute searchFunctionRedeclarationCheck::([Message] ::= Type [Type]) occurs on Name;
synthesized attribute searchFunctionRedeclarationCheckNoCompatible::[Message] occurs on Name;
synthesized attribute searchFunctionLookupCheck::[Message] occurs on Name;
synthesized attribute searchFunctionItem::Decorated SearchFunctionItem occurs on Name;

aspect production name
top::Name ::= n::String
{
  top.searchFunctionRedeclarationCheck =
    \ resultType::Type parameterTypes::[Type] ->
      case lookupInLocalScope(n, top.env.searchFunctions) of
      | [] -> []
      | v :: _ -> 
          if compatibleTypes(resultType.withoutExtensionQualifiers, v.resultType.withoutExtensionQualifiers, false, false) &&
            length(parameterTypes) == length(v.parameterTypes) &&
            foldr(
              \ b1::Boolean b2::Boolean -> b1 && b2,
              true,
              zipWith(
                \ t1::Type t2::Type ->
                  compatibleTypes(t1.withoutExtensionQualifiers, t2.withoutExtensionQualifiers, false, false),
                parameterTypes,
                v.parameterTypes))
          then []
          else 
            let originalPP :: String = showType(v.resultType) ++ "(" ++ implode(", ", map(showType, v.parameterTypes)) ++ ")",
                herePP :: String = showType(resultType) ++ "(" ++ implode(", ", map(showType, parameterTypes)) ++ ")"
             in
                [errFromOrigin(top, 
                  "Redeclaration of " ++ n ++ " with incompatible types. Original (from " ++
                  getParsedOriginLocationOrFallback(v).unparse ++ ") " ++ originalPP ++ 
                  " but here it is " ++ herePP)]
            end
      end;
  top.searchFunctionRedeclarationCheckNoCompatible =
    case lookupInLocalScope(n, top.env.searchFunctions) of
    | [] -> []
    | v :: _ -> 
        [errFromOrigin(top, 
          "Redeclaration of " ++ n ++ ". Original (from " ++
          getParsedOriginLocationOrFallback(v).unparse ++ ")")]
    end;
  
  local searchFunctions::[SearchFunctionItem] = lookupScope(n, top.env.searchFunctions);
  top.searchFunctionLookupCheck =
    case searchFunctions of
    | [] -> [errFromOrigin(top, "Undeclared search function " ++ n)]
    | _ :: _ -> []
    end;
  
  local searchFunction::SearchFunctionItem =
    if null(searchFunctions) then errorSearchFunctionItem() else head(searchFunctions);
  top.searchFunctionItem = searchFunction;
}

-- Make all non-global value items in the env const
functor attribute asCaptured;

attribute asCaptured<Decorated Env> occurs on Env;

aspect production emptyEnv_i
top::Env ::=
{
  top.asCaptured = emptyEnv();
}
aspect production addEnv_i
top::Env ::= d::Defs  e::Decorated Env
{
  top.asCaptured =
    case e of
    | emptyEnv_i() -> top -- This is the global scope, everything here stays non-const
    | _ -> addEnv(d.asCaptured, e.asCaptured)
    end;
}
aspect production openScopeEnv_i
top::Env ::= e::Decorated Env
{
  top.asCaptured = openScopeEnv(e.asCaptured);
}
aspect production globalEnv_i
top::Env ::= e::Decorated Env
{
  top.asCaptured = globalEnv(e.asCaptured);
}
aspect production nonGlobalEnv_i
top::Env ::= e::Decorated Env
{
  top.asCaptured = nonGlobalEnv(e.asCaptured);
}
aspect production functionEnv_i
top::Env ::= e::Decorated Env
{
  top.asCaptured = functionEnv(e.asCaptured);
}

attribute asCaptured<[Def]> occurs on Defs;

aspect production consDefs
top::Defs ::= h::Def  t::Defs
{
  top.asCaptured = h.asCaptured :: t.asCaptured;
}

aspect production nilDefs
top::Defs ::=
{
  top.asCaptured = [];
}

attribute asCaptured occurs on Def;

aspect default production
top::Def ::=
{
  top.asCaptured = top;
}

aspect production valueDef
top::Def ::= s::String v::ValueItem
{
  propagate asCaptured;
}

attribute asCaptured occurs on ValueItem;

aspect default production
top::ValueItem ::= 
{
  top.asCaptured = if top.isItemValue then constValueItem(top) else top;
}

-- Wraps another ValueItem and makes the type const
abstract production constValueItem
top::ValueItem ::= ref::Decorated ValueItem
{
  top.pp = pp"const ${ref.pp}";
  top.typerep = addQualifiers([constQualifier()], ref.typerep);
  top.isItemValue = ref.isItemValue;
  top.asCaptured = top; -- Non-interfering optimization
}

