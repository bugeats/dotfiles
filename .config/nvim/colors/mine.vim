" Reset -----------------------------------------------------------------------

set background=dark
highlight clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name = "mine"

" hide hidden chars in nerdtree
autocmd FileType nerdtree setlocal nolist

" hide hidden chars in terminal
au TermOpen * setlocal nolist

" General Syntax Matches -------------------------------------------------------

autocmd BufRead,BufNewFile * syn match generalParens /[(){}]/
autocmd BufRead,BufNewFile * syn match generalBrackets /[\[\]]/
autocmd BufRead,BufNewFile * syn match generalControlChar /[\.:\+!=;\,*<>|]/

" Palette ----------------------------------------------------------------------

let s:p = {}

let s:p.a1 = "#d8af8e"
let s:p.a2 = "#a57b55"
let s:p.a3 = "#645851"
let s:p.a4 = "#413c39"
let s:p.a5 = "#2d2b29"
let s:p.a6 = "#272524"
let s:p.b1 = "#94c390"
let s:p.b2 = "#5d9058"
let s:p.b3 = "#525e51"
let s:p.b4 = "#393e39"
let s:p.b5 = "#2a2c29"
let s:p.b6 = "#242624"
let s:p.d1 = "#b8b8aa"
let s:p.d2 = "#84847c"
let s:p.d3 = "#5b5b56"
let s:p.d4 = "#3d3d3a"
let s:p.d5 = "#2c2c29"
let s:p.d6 = "#262624"
let s:p.g1 = "#bbba81"
let s:p.g2 = "#888744"
let s:p.g3 = "#5c5b4e"
let s:p.g4 = "#3d3d37"
let s:p.g5 = "#2c2c29"
let s:p.g6 = "#262624"
let s:p.k1 = "#83bfda"
let s:p.k2 = "#418da9"
let s:p.k3 = "#4f5d64"
let s:p.k4 = "#383e41"
let s:p.k5 = "#292c2d"
let s:p.k6 = "#242627"
let s:p.t1 = "#e6a4be"
let s:p.t2 = "#b46f8c"
let s:p.t3 = "#67565c"
let s:p.t4 = "#433b3e"
let s:p.t5 = "#2e2a2c"
let s:p.t6 = "#282526"

let s:p.blacknormal   = "#5f5955"
let s:p.blackbright   = "#5f5955"
let s:p.blackdim      = "#403c3a"
let s:p.whitenormal   = "#c0b5ad"
let s:p.whitebright   = "#c0b5ad"
let s:p.whitedim      = "#8a827e"
let s:p.rednormal     = "#d8af8e"
let s:p.redbright     = "#d8af8e"
let s:p.reddim        = "#a57b55"
let s:p.yellownormal  = "#a8bf85"
let s:p.yellowbright  = "#a8bf85"
let s:p.yellowdim     = "#748c4a"
let s:p.greennormal   = "#71c6af"
let s:p.greenbright   = "#71c6af"
let s:p.greendim      = "#22947b"
let s:p.cyannormal    = "#83bfda"
let s:p.cyanbright    = "#83bfda"
let s:p.cyandim       = "#418da9"
let s:p.bluenormal    = "#c6ade1"
let s:p.bluebright    = "#c6ade1"
let s:p.bluedim       = "#9478b0"
let s:p.magentanormal = "#e6a4be"
let s:p.magentabright = "#e6a4be"
let s:p.magentadim    = "#b46f8c"

let g:terminal_color_0  = s:p.blacknormal
let g:terminal_color_1  = s:p.rednormal
let g:terminal_color_2  = s:p.greennormal
let g:terminal_color_3  = s:p.yellownormal
let g:terminal_color_4  = s:p.bluenormal
let g:terminal_color_5  = s:p.magentanormal
let g:terminal_color_6  = s:p.cyannormal
let g:terminal_color_7  = s:p.whitenormal
let g:terminal_color_8  = s:p.blacknormal
let g:terminal_color_9  = s:p.rednormal
let g:terminal_color_10 = s:p.greennormal
let g:terminal_color_11 = s:p.yellownormal
let g:terminal_color_12 = s:p.bluenormal
let g:terminal_color_13 = s:p.magentanormal
let g:terminal_color_14 = s:p.cyannormal
let g:terminal_color_15 = s:p.whitenormal

" Functions --------------------------------------------------------------------

function! s:linkGroup(item, toLink)
    for g in a:toLink
        let linkCmd = 'hi! link ' . g . ' ' . a:item
        execute linkCmd
    endfor
endfunction

function! s:smartHi(group, ...)
    " Arguments: group, guifg, guibg
    let chunks = ['hi!', a:group]

    if strlen(a:1)
        call add(chunks, 'guifg=' . a:1)
    endif

    if strlen(a:2)
        call add(chunks, 'guibg=' . a:2)
    endif

    call add(chunks, 'gui=NONE')
    call add(chunks, 'ctermfg=NONE')
    call add(chunks, 'ctermbg=NONE')
    call add(chunks, 'cterm=NONE')

    let cmd = join(chunks, ' ')

    " echom cmd
    execute cmd
endfunction


" Main Groups ------------------------------------------------------------------

call s:smartHi('Normal',                    s:p.d1, s:p.a6)
call s:smartHi('RedshiftAttention',         s:p.t1, s:p.t4)
call s:smartHi('RedshiftAttentionFg',       s:p.t1, '')
call s:smartHi('RedshiftChromeHighlighted', s:p.a1, s:p.a6)
call s:smartHi('RedshiftChromeMin',         s:p.a3, s:p.a6)
call s:smartHi('RedshiftChromeNormal',      s:p.a2, s:p.a6)
call s:smartHi('RedshiftChromePanel',       s:p.a2, s:p.a4)
call s:smartHi('RedshiftChromePanelActive', s:p.a1, s:p.a4)
call s:smartHi('RedshiftComment',           s:p.d2, '')
call s:smartHi('RedshiftControl',           s:p.a2, '')
call s:smartHi('RedshiftControlActive',     s:p.a1, '')
call s:smartHi('RedshiftGhost',             s:p.a3, '')
call s:smartHi('RedshiftGhostActive',       s:p.a2, s:p.a5)
call s:smartHi('RedshiftHighlighted',       s:p.d1, s:p.t4)
call s:smartHi('RedshiftHighlightedAlt',    s:p.a6, s:p.a1)
call s:smartHi('RedshiftKeyword',           s:p.b1, '')
call s:smartHi('RedshiftLiteral',           s:p.g1, s:p.a6)
call s:smartHi('RedshiftLocated',           '',     s:p.a5)
call s:smartHi('RedshiftNormal',            s:p.d1, s:p.a6)
call s:smartHi('RedshiftSelected',          '',     s:p.k4)

call s:smartHi('RedshiftGreen',  s:p.greennormal, '')
call s:smartHi('RedshiftYellow', s:p.yellownormal, '')
call s:smartHi('RedshiftRed',    s:p.rednormal, '')

" Links ------------------------------------------------------------------------

call s:linkGroup('RedshiftNormal', [
    \"ColorColumn",
    \"Conceal",
    \"Constant",
    \"CursorIM",
    \"DiffAdd",
    \"DiffChange",
    \"DiffDelete",
    \"DiffText",
    \"Directory",
    \"EndOfBuffer",
    \"FoldColumn",
    \"Folded",
    \"Function",
    \"Identifier",
    \"ModeMsg",
    \"MoreMsg",
    \"Normal",
    \"PreProc",
    \"Question",
    \"SignColumn",
    \"Special",
    \"SpecialKey",
    \"SpellBad",
    \"SpellCap",
    \"SpellLocal",
    \"SpellRare",
    \"Statement",
    \"TermCursorNC",
    \"Title",
    \"Type",
    \"Underlined",
    \"VisualNOS",
    \"WarningMsg",
    \"WildMenu",
    \"clojureKeyword",
    \"clojureMacro",
    \"htmlTagName",
    \"javascriptArrayMethod",
    \"javascriptBOMNavigatorProp",
    \"javascriptBlock",
    \"javascriptConsoleMethod",
    \"javascriptDOMDocMethod",
    \"javascriptDOMElemProp",
    \"javascriptDOMEventProp",
    \"javascriptDOMStorageProp",
    \"javascriptES6SetMethod",
    \"javascriptFileReaderProp",
    \"javascriptFuncCallArg",
    \"javascriptFunctionMethod",
    \"javascriptHeadersMethod",
    \"javascriptJSONStaticMethod",
    \"javascriptMethod",
    \"javascriptPromiseMethod",
    \"javascriptPromiseStaticMethod",
    \"javascriptProp",
    \"javascriptReflectMethod",
    \"javascriptRegexpString",
    \"javascriptRequestProp",
    \"javascriptResponseProp",
    \"javascriptStringMethod",
    \"xmlEndTag",
    \"xmlTagName",
\])

call s:linkGroup('RedshiftComment', [
    \"Comment",
    \"javascriptDocComment",
    \"javascriptDocNamedDocParamType",
    \"javascriptDocNamedParamType",
    \"javascriptDocNotation",
    \"javascriptDocParamName",
    \"javascriptDocParamType",
    \"javascriptDocTags",
    \"vimLineComment",
\])

call s:linkGroup('RedshiftControl', [
    \"rubyInterpolationDelimiter",
    \"clojureParen",
    \"cssBraces",
    \"cssNoise",
    \"dotBraceEncl",
    \"dotBrackEncl",
    \"dotKeyChar",
    \"generalBrackets",
    \"generalControlChar",
    \"generalParens",
    \"haskellDelimiter",
    \"haskellOperators",
    \"haskellSeparator",
    \"htmlEndTag",
    \"htmlTag",
    \"javascriptArrowFunc",
    \"javascriptBraces",
    \"javascriptBrackets",
    \"javascriptCaseColon",
    \"javascriptComma",
    \"javascriptDefaultAssign",
    \"javascriptDotNotation",
    \"javascriptEndColons",
    \"javascriptObjectLabelColon",
    \"javascriptOpSymbol",
    \"javascriptOpSymbols",
    \"javascriptParenObjectLiteral",
    \"javascriptParens",
    \"javascriptProperty",
    \"javascriptTemplateSB",
    \"jsFuncBraces",
    \"jsFuncParens",
    \"jsNoise",
    \"jsObjectBraces",
    \"jsOperator",
    \"jsParens",
    \"jsonBraces",
    \"jsonNoise",
    \"jsxAttributeBraces",
    \"jsxCloseString",
    \"jsxCloseTag",
    \"jsxEndTag",
    \"jsxEqual",
    \"jsxTag",
    \"luaParen",
    \"markdownHeadingDelimiter",
    \"markdownListMarker",
    \"markdownRule",
    \"pugAttributesDelimiter",
    \"pugInterpolationDelimiter",
    \"pugPipeChar",
    \"shVarAssign",
    \"stylusProperty",
    \"stylusVariableAssignment",
    \"taskpaperListItem",
    \"typescriptArrowFunc",
    \"typescriptArrowFuncDef",
    \"typescriptAssign",
    \"typescriptBinaryOp",
    \"typescriptBraces",
    \"typescriptDotNotation",
    \"typescriptParens",
    \"typescriptTemplateSB",
    \"vimContinue",
    \"vimParenSep",
    \"xmlEqual",
    \"xmlTag",
    \"yamlKeyValueDelimiter",
\])

call s:linkGroup('RedshiftControlActive', [
    \"MatchParen",
\])

call s:linkGroup('RedshiftKeyword', [
    \"4dglKeyword",
    \"rubyDefine",
    \"Boolean",
    \"Keyword",
    \"cInclude",
    \"cStatement",
    \"cType",
    \"clojureCond",
    \"clojureConstant",
    \"clojureDefine",
    \"clojureFunc",
    \"clojureSpecial",
    \"cssBackgroundProp",
    \"cssBorderProp",
    \"cssBoxProp",
    \"cssColorProp",
    \"cssFontProp",
    \"cssProp",
    \"cssProp",
    \"cssTextProp",
    \"cssVisualProp",
    \"graphqlStructure",
    \"haskellDecl",
    \"haskellDeclKeyword",
    \"haskellDerive",
    \"haskellImportKeywords",
    \"haskellKeyword",
    \"haskellWhere",
    \"javascriptBranch",
    \"javascriptCase",
    \"javascriptConditional",
    \"javascriptConditionalElse",
    \"javascriptExceptions",
    \"javascriptExport",
    \"javascriptIdentifier",
    \"javascriptImport",
    \"javascriptNodeGlobal",
    \"javascriptOperator",
    \"javascriptRepeat",
    \"javascriptReserved",
    \"javascriptReturn",
    \"javascriptSwitch",
    \"javascriptTry",
    \"javascriptVariable",
    \"jsGlobalObjects",
    \"jsStorageClass",
    \"luaCond",
    \"luaFunction",
    \"luaIn",
    \"luaRepeat",
    \"luaStatement",
    \"pugScriptStatement",
    \"pugTag",
    \"pythonConditional",
    \"pythonExecption",
    \"pythonImport",
    \"pythonInclude",
    \"pythonOperator",
    \"pythonRaiseFromStatement",
    \"pythonRepeat",
    \"pythonStatement",
    \"rubyControl",
    \"rubyExceptional",
    \"rubyConditional",
    \"rubyInclude",
    \"rubyMacro",
    \"shEcho",
    \"shFunctionKey",
    \"stylusImport",
    \"taskpaperProject",
    \"typescriptAmbientDeclaration",
    \"typescriptBOMWindowProp",
    \"typescriptCastKeyword",
    \"typescriptConditional",
    \"typescriptDefault",
    \"typescriptEnumKeyword",
    \"typescriptExport",
    \"typescriptIdentifierName",
    \"typescriptImport",
    \"typescriptKeyword",
    \"typescriptKeywordOp",
    \"typescriptModule",
    \"typescriptOperator",
    \"typescriptStatementKeyword",
    \"typescriptVariable",
    \"vimCommand",
    \"vimLet",
\])

call s:linkGroup('RedshiftLiteral', [
    \"String",
    \"javascriptPropertyNameString",
    \"javascriptRegexpString",
    \"javascriptString",
    \"jsxRegion",
    \"markdownCode",
    \"markdownCodeBlock",
    \"pugPipedText",
    \"rubyStringDelimiter",
    \"yamlPlainScalar",
\])

call s:linkGroup('RedshiftGhost', [
    \"Conceal",
    \"EndOfBuffer",
    \"LineNr",
    \"NonText",
    \"SpecialKey",
\])

call s:linkGroup('RedshiftGhostActive', [
    \"CursorLineNr",
\])

call s:linkGroup('RedshiftLocated', [
    \"CursorColumn",
    \"CursorLine",
\])

call s:linkGroup('RedshiftHighlighted', [
    \"Search",
\])

call s:linkGroup('RedshiftHighlightedAlt', [
    \"IncSearch",
\])

call s:linkGroup('RedshiftSelected', [
    \"Cursor",
    \"TermCursor",
    \"Visual",
    \"iCursor",
\])

call s:linkGroup('RedshiftAttention', [
    \"ExtraWhitespace",
    \"JavascriptCommentTodo",
    \"SpellBad",
    \"Todo",
\])

call s:linkGroup('RedshiftAttentionFg', [
    \"Error",
    \"ErrorMsg",
    \"NeomakeErrorMsg",
\])

" ----

call s:linkGroup('RedshiftChromeNormal', [
    \"NERDTreeCWD",
    \"NERDTreeDir",
    \"NERDTreeExecFile",
    \"NERDTreeFile",
    \"NERDTreeLinkFile",
\])

call s:linkGroup('RedshiftChromeHighlighted', [
    \"NERDTreeOpenBuffer",
\])

call s:linkGroup('RedshiftChromeMin', [
    \"NERDTreeClosable",
    \"NERDTreeDirSlash",
    \"NERDTreeFlags",
    \"NERDTreeLinkTarget",
    \"NERDTreeOpenable",
\])

call s:linkGroup('RedshiftChromePanel', [
    \"Pmenu",
    \"PmenuSbar",
    \"PmenuThumb",
    \"StatusLineNC",
    \"TabLine",
    \"TabLineFill",
    \"VertSplit",
\])

call s:linkGroup('RedshiftChromePanelActive', [
    \"PmenuSel",
    \"StatusLine",
    \"TabLineSel",
\])

" ----

call s:linkGroup('RedShiftGreen', [
    \"DiffAdd",
    \"GitGutterAdd",
    \"GitGutterAddDefault",
    \"NERDTreeGitStatusModified",
\])

call s:linkGroup('RedShiftYellow', [
    \"DiffChange",
    \"GitGutterChange",
    \"GitGutterChangeDefault",
    \"GitGutterChangeDelete",
    \"GitGutterChangeDeleteDefault",
    \"NERDTreeGitStatusDirDirty",
\])

call s:linkGroup('RedShiftRed', [
    \"DiffDelete",
    \"GitGutterDelete",
    \"GitGutterDeleteDefault",
\])
