grammar edu:umn:cs:melt:exts:ableC:search:abstractsyntax;

autocopy attribute searchFunctionEnv::Scopes<SearchFunctionItem>;
synthesized attribute searchFunctionDefs::Contribs<SearchFunctionItem>;

nonterminal SearchFunctionItem with parameterTypes, resultType, sourceLocation;

abstract production searchFunctionItem
top::SearchFunctionItem ::= f::Decorated SearchFunctionDecl
{
  top.parameterTypes = f.parameterTypes;
  top.resultType = f.resultType;
  top.sourceLocation = f.sourceLocation;
}

abstract production errorSearchFunctionItem
top::SearchFunctionItem ::=
{
  top.parameterTypes = [];
  top.resultType = errorType();
  top.sourceLocation = loc("nowhere", -1, -1, -1, -1, -1, -1);
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
  top.searchFunctionContribs = [pair(s, t)];
}

-- Make all non-global value items in the env const
function constEnv
Decorated Env ::= env::Decorated Env
{
  return error("Not implemented");
}
