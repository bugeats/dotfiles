" Reset -----------------------------------------------------------------------

set background=dark

" highlight clear
" if exists("syntax_on")
"     syntax reset
" endif

let g:colors_name = "mine"

" hide hidden chars in nerdtree
autocmd FileType nerdtree setlocal nolist

" hide hidden chars in terminal
au TermOpen * setlocal nolist

" General Syntax Matches -------------------------------------------------------


" autocmd Syntax * matchadd('generalControlChar', '[\?\.:\+!=;\,*<>|]')

" autocmd Syntax ruby,javascript,typescript call matchadd('generalControlChar', '[,|;=<>]')
" autocmd Syntax javascript,typescript call matchadd('generalBrackets', '[\[\]{}]')
" autocmd Syntax ruby,javascript call matchadd('generalParens', '[()]')

" autocmd Syntax ruby,javascript,typescript call matchadd('generalControlChar', '[\?\.:\+!=;\,*<>|]')
" autocmd FileType ruby,javascript,typescript call matchadd('generalControlChar', '[\?\.:\+!=;\,*<>|]')
autocmd FileType ruby,javascript,typescript,typescriptreact,reason,elm,purescript,dhall,json syn match generalControlChar /[\?\.:\+!=;\,*<>|~]/ containedin=typescriptBlock,typescriptParenExp,typescriptObjectLiteral,typescriptClassBlock,typescriptConditionalParen,reasonFoldBraces,reasonModPath,sassDefinition,purescriptModuleName,jsonFold contained
autocmd FileType ruby,javascript,typescript,typescriptreact,reason,elm,purescript,dhall,json syn match generalBrackets /[\[\]{}]'/ containedin=typescriptBlock,typescriptProperty,typescriptConditionalParen,reasonFoldBraces,sassDefinition
autocmd FileType purescript syn match generalParens /\(\)/ containedin=purescriptImportParams contained

autocmd Syntax ruby,javascript,typescript,reason,elm,purescript,dhall syn match RedshiftControl /[,|;=<>]/
autocmd Syntax ruby,javascript,reason,elm,purescript,dhall syn match RedshiftControl /[()]/

autocmd Syntax javascript syn match jsReturn /return/
autocmd Syntax dart syn match dartKeyword /new/

highlight RedshiftType gui=bold
highlight RedshiftType cterm=bold

" Palette ----------------------------------------------------------------------

let s:p = {}

" alpha - redish
let s:p.a1 = "#d8af8e"
let s:p.a2 = "#a57b55"
let s:p.a3 = "#645851"
let s:p.a4 = "#413c39"
let s:p.a5 = "#2d2b29"
let s:p.a6 = "#272524"

" beta - green
let s:p.b1 = "#94c390"
let s:p.b2 = "#5d9058"
let s:p.b3 = "#525e51"
let s:p.b4 = "#393e39"
let s:p.b5 = "#2a2c29"
let s:p.b6 = "#242624"

" delta - white/gray
let s:p.d1 = "#b8b8aa"
let s:p.d2 = "#84847c"
let s:p.d3 = "#5b5b56"
let s:p.d4 = "#3d3d3a"
let s:p.d5 = "#2c2c29"
let s:p.d6 = "#262624"

" gamma - yellow
let s:p.g1 = "#bbba81"
let s:p.g2 = "#888744"
let s:p.g3 = "#5c5b4e"
let s:p.g4 = "#3d3d37"
let s:p.g5 = "#2c2c29"
let s:p.g6 = "#262624"

" kappa - blue
let s:p.k1 = "#83bfda"
let s:p.k2 = "#418da9"
let s:p.k3 = "#4f5d64"
let s:p.k4 = "#383e41"
let s:p.k5 = "#292c2d"
let s:p.k6 = "#242627"

" tau - pink
let s:p.t1 = "#e6a4be"
let s:p.t2 = "#b46f8c"
let s:p.t3 = "#67565c"
let s:p.t4 = "#433b3e"
let s:p.t5 = "#2e2a2c"
let s:p.t6 = "#282526"

" lambda - between green and yellow
let s:p.l1 = "#A8BF89"

" epsilon - between green and white
let s:p.e1 = "#a7bac5"

" ----

" let s:p.d1_shade = #b8b8aa
let s:p.d1_shade = "#b2b2a5"

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


" Vim Core Groups --------------------------------------------------------------

" Link the core default highlight groups to redshift groups.
" This is done so foreign syntax plugins behave mostly ok by default.

hi! link Comment RedshiftComment
hi! link Constant RedshiftNormal
hi! link String RedshiftLiteral
hi! link SpecialComment RedshiftNormal
hi! link Character RedshiftNormal
hi! link Number RedshiftNormal
hi! link Boolean RedshiftKeyword
hi! link Float RedshiftNormal
hi! link Identifier RedshiftNormal
hi! link Function RedshiftNormal
hi! link Statement RedshiftNormal
hi! link Conditional RedshiftKeyword
hi! link Repeat RedshiftKeyword
hi! link Label RedshiftKeyword
hi! link Keyword RedshiftKeyword
hi! link Exception RedshiftKeyword
hi! link PreProc RedshiftNormal
hi! link Include RedshiftNormal
hi! link Define RedshiftNormal
hi! link Macro RedshiftNormal
hi! link PreCondit RedshiftNormal
hi! link Type RedshiftNormal
hi! link StorageClass RedshiftNormal
hi! link Structure RedshiftKeyword
hi! link Error RedshiftAttention
hi! link Ignore RedshiftNormal
hi! link Typedef RedshiftNormal
hi! link Special RedshiftNormal
hi! link SpecialChar RedshiftNormal
hi! link Tag RedshiftNormal
hi! link Delimiter RedshiftNormal
hi! link Debug RedshiftNormal
hi! link Underlined RedshiftNormal
hi! link Todo RedshiftAttention
hi! link Operator RedshiftNormal

" Main Groups ------------------------------------------------------------------

call s:smartHi('Normal',                    s:p.d1, s:p.a6)
call s:smartHi('RedshiftAttention',         s:p.t1, s:p.t4)
call s:smartHi('RedshiftAttentionFg',       s:p.t1, '')

call s:smartHi('RedshiftComment',           s:p.d2, '')
call s:smartHi('RedshiftControl',           s:p.a2, '')
call s:smartHi('RedshiftControlActive',     s:p.a1, '')
call s:smartHi('RedshiftControlDim',        s:p.a2, s:p.g5)
call s:smartHi('RedshiftGhost',             s:p.a3, '')
call s:smartHi('RedshiftGhostActive',       s:p.a2, s:p.a5)
call s:smartHi('RedshiftHighlighted',       s:p.d1, s:p.t4)
call s:smartHi('RedshiftHighlightedAlt',    s:p.a6, s:p.a1)
call s:smartHi('RedshiftKeyword',           s:p.b1, '')
call s:smartHi('RedshiftLiteral',           s:p.g1, s:p.a6)
call s:smartHi('RedshiftLiteralDim',        s:p.g2, s:p.a6)
call s:smartHi('RedshiftLocated',           '',     s:p.a5)
call s:smartHi('RedshiftNormal',            s:p.d1, s:p.a6)
call s:smartHi('RedshiftNormalDim',         s:p.d1, s:p.g5)
call s:smartHi('RedshiftSelected',          '',     s:p.k4)
call s:smartHi('RedshiftSymbol',            s:p.l1, s:p.a6)
call s:smartHi('RedshiftType',              s:p.d1_shade, '')

call s:smartHi('RedshiftGreen',  s:p.greennormal, '')
call s:smartHi('RedshiftYellow', s:p.yellownormal, '')
call s:smartHi('RedshiftRed',    s:p.rednormal, '')

call s:smartHi('RedshiftChromeHighlighted', s:p.a1, s:p.a6)
call s:smartHi('RedshiftChromeMin',         s:p.a3, s:p.a6)
call s:smartHi('RedshiftChromeNormal',      s:p.a2, s:p.a6)
call s:smartHi('RedshiftChromePanel',       s:p.a2, s:p.a4)
call s:smartHi('RedshiftChromePanelActive', s:p.a1, s:p.a4)

call s:smartHi('RedshiftChromeWarning',     s:p.t1, s:p.a4)
call s:smartHi('RedshiftChromeError',       s:p.t1, s:p.a4)

highlight RedshiftType gui=bold
highlight RedshiftType cterm=bold

" Links ------------------------------------------------------------------------


call s:linkGroup('RedshiftNormal', [
    \"ColorColumn",
    \"Conceal",
    \"CursorIM",
    \"DiffAdd",
    \"DiffChange",
    \"DiffDelete",
    \"DiffText",
    \"Directory",
    \"EndOfBuffer",
    \"FoldColumn",
    \"Folded",
    \"ModeMsg",
    \"MoreMsg",
    \"Normal",
    \"Question",
    \"SignColumn",
    \"SpecialKey",
    \"SpellBad",
    \"SpellCap",
    \"SpellLocal",
    \"SpellRare",
    \"TermCursorNC",
    \"Title",
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
    \"javascriptRegexpSteraltring",
    \"javascriptRequestProp",
    \"javascriptResponseProp",
    \"javascriptStringMethod",
    \"jsonKeyword",
    \"rubyRegexp",
    \"typescriptObjectLabel",
    \"typescriptPaymentAddressProp",
    \"xmlEndTag",
    \"xmlTagName",
    \"TSTag",
    \"TSLabel",
\])

call s:linkGroup('RedshiftNormalDim', [
    \"jsTemplateExpression",
\])

call s:linkGroup('RedshiftComment', [
    \"cPreProc",
    \"javascriptDocComment",
    \"javascriptDocNamedDocParamType",
    \"javascriptDocNamedParamType",
    \"javascriptDocNotation",
    \"javascriptDocParamName",
    \"javascriptDocParamType",
    \"javascriptDocTags",
    \"pythonRun",
    \"rustCommentLineDoc",
    \"vimLineComment",
\])

call s:linkGroup('RedshiftControl', [
    \"cParen",
    \"clojureParen",
    \"cssBraces",
    \"cssNoise",
    \"dartOperator",
    \"dhallParens",
    \"dhallRecord",
    \"dotBraceEncl",
    \"dotBrackEncl",
    \"dotKeyChar",
    \"elmBraces",
    \"elmOperator",
    \"generalBrackets",
    \"generalControlChar",
    \"generalParens",
    \"goBlock",
    \"goParen",
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
    \"jsArrowFunction",
    \"jsBraces",
    \"jsBrackets",
    \"jsClassBraces",
    \"jsDestructuringBraces",
    \"jsDot",
    \"jsFuncArgCommas",
    \"jsFuncArgOperator",
    \"jsFuncBraces",
    \"jsFuncParens",
    \"jsIfElseBraces",
    \"jsModuleBraces",
    \"jsNoise",
    \"jsObjectBraces",
    \"jsObjectSeparator",
    \"jsOperator",
    \"jsParens",
    \"jsRepeatBraces",
    \"jsRestOperator",
    \"jsSpreadOperator",
    \"jsSwitchBraces",
    \"jsSwitchColon",
    \"jsTernaryIfOperator",
    \"jsTryCatchBraces",
    \"jsonBraces",
    \"jsonNoise",
    \"jsonRepeatBraces",
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
    \"purescriptDelimiter",
    \"purescriptOperator",
    \"purescriptOperatorType",
    \"pythonDot",
    \"reasonFoldBraces",
    \"reasonOperator",
    \"rubyArrayDelimiter",
    \"rubyBooleanOperator",
    \"rubyCurlyBlockDelimiter",
    \"rubyDotOperator",
    \"rubyMethodDeclaration",
    \"rubyOperator",
    \"rubyProcOperator",
    \"rubyScopeOperator",
    \"rubySuperClassOperator",
    \"rubySymbolDelimiter",
    \"rustArrowCharacter",
    \"rustFoldBraces",
    \"rustSigil",
    \"shVarAssign",
    \"stylusProperty",
    \"stylusVariableAssignment",
    \"taskpaperListItem",
    \"tsxCloseString",
    \"tsxEqual",
    \"typescriptArrowFunc",
    \"typescriptArrowFuncDef",
    \"typescriptAssign",
    \"typescriptBinaryOp",
    \"typescriptBraces",
    \"typescriptDotNotation",
    \"typescriptEndColons",
    \"typescriptFuncComma",
    \"typescriptObjectColon",
    \"typescriptParens",
    \"typescriptTemplateSB",
    \"typescriptTernaryOp",
    \"typescriptTypeAnnotation",
    \"typescriptTypeBrackets",
    \"typescriptUnion",
    \"vimContinue",
    \"vimParenSep",
    \"xmlEqual",
    \"xmlTag",
    \"yamlKeyValueDelimiter",
    \"TSOperator",
    \"TSPunctBracket",
    \"TSPunctDelimiter",
    \"TSPunctSpecial",
    \"TSTagDelimiter",
\])

call s:linkGroup('RedshiftControlActive', [
    \"MatchParen",
\])

call s:linkGroup('RedshiftControlDim', [
    \"jsTemplateBraces",
\])

call s:linkGroup('RedshiftType', [
    \"dartUserType",
    \"elmType",
    \"rubyConstant",
    \"purescriptModuleName",
    \"purescriptType",
    \"TSType",
    \"TSConstructor",
    \"TSTypeBuiltin",
\])

call s:linkGroup('RedshiftKeyword', [
    \"4dglKeyword",
    \"cConditional",
    \"cDefine",
    \"cInclude",
    \"cLabel",
    \"cPreCondit",
    \"cPreCondit",
    \"cRepeat",
    \"cStatement",
    \"cStorageClass",
    \"cStructure",
    \"cType",
    \"clojureCond",
    \"clojureConstant",
    \"clojureDefine",
    \"clojureFunc",
    \"clojureSpecial",
    \"cppAccess",
    \"cppModifier",
    \"cppStatement",
    \"cppStructure",
    \"cppType",
    \"cssBackgroundProp",
    \"cssBorderProp",
    \"cssBoxProp",
    \"cssColorProp",
    \"cssFontProp",
    \"cssProp",
    \"cssProp",
    \"cssTextProp",
    \"cssVisualProp",
    \"dartBranch",
    \"dartClassDecl",
    \"dartConditional",
    \"dartConstant",
    \"dartCoreType",
    \"dartExceptions",
    \"dartKeyword",
    \"dartLibrary",
    \"dartRepeat",
    \"dartSdkClass",
    \"dartStatement",
    \"dartStorageClass",
    \"dartTypedef",
    \"elmCaseBlockDefinition",
    \"elmConditional",
    \"elmImport",
    \"elmTypedef",
    \"goDirective",
    \"goImport",
    \"goPackage",
    \"graphqlStructure",
    \"haskellBottom",
    \"haskellConditional",
    \"haskellDecl",
    \"haskellDeclKeyword",
    \"haskellDerive",
    \"haskellImportKeywords",
    \"haskellKeyword",
    \"haskellLet",
    \"haskellWhere",
    \"javaScriptFunction",
    \"javaScriptStatement",
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
    \"jsCatch",
    \"jsClassMethodType",
    \"jsConditional",
    \"jsException",
    \"jsExport",
    \"jsExportDefault",
    \"jsFrom",
    \"jsFunction",
    \"jsGlobalNodeObjects",
    \"jsGlobalObjects",
    \"jsImport",
    \"jsLabel",
    \"jsNull",
    \"jsOperatorKeyword",
    \"jsRepeat",
    \"jsReturn",
    \"jsStatement",
    \"jsStorageClass",
    \"jsThis",
    \"jsTry",
    \"jsUndefined",
    \"jsonNull",
    \"luaCond",
    \"luaFunction",
    \"luaIn",
    \"luaRepeat",
    \"luaStatement",
    \"pugScriptStatement",
    \"pugTag",
    \"purescriptStatement",
    \"pythonConditional",
    \"pythonException",
    \"pythonImport",
    \"pythonInclude",
    \"pythonOperator",
    \"pythonRaiseFromStatement",
    \"pythonRepeat",
    \"pythonStatement",
    \"rubyClass",
    \"rubyControl",
    \"rubyDefine",
    \"rubyInclude",
    \"rubyMacro",
    \"rustConditional",
    \"rustOperator",
    \"rustRepeat",
    \"rustSelf",
    \"rustStorage",
    \"rustType",
    \"rustUnsafeKeyword",
    \"shEcho",
    \"shFunctionKey",
    \"stylusImport",
    \"swiftImports",
    \"taskpaperProject",
    \"typescriptAmbientDeclaration",
    \"typescriptBOMWindowProp",
    \"typescriptCastKeyword",
    \"typescriptConditional",
    \"typescriptDefault",
    \"typescriptEnumKeyword",
    \"typescriptExceptions",
    \"typescriptExport",
    \"typescriptImport",
    \"typescriptKeyword",
    \"typescriptKeywordOp",
    \"typescriptMethodAccessor",
    \"typescriptModule",
    \"typescriptOperator",
    \"typescriptPredefinedType",
    \"typescriptStatementKeyword",
    \"typescriptTry",
    \"typescriptVariable",
    \"vimCommand",
    \"vimLet",
    \"TSInclude",
    \"TSVariableBuiltin",
    \"TSKeywordOperator",
\])

call s:linkGroup('RedshiftLiteral', [
    \"dartString",
    \"javascriptPropertyNameString",
    \"javascriptRegexpString",
    \"javascriptString",
    \"jsxRegion",
    \"markdownCode",
    \"markdownCodeBlock",
    \"pugPipedText",
    \"rubyStringDelimiter",
    \"tsxRegion",
    \"yamlPlainScalar",
    \"jsonStringMatch",
    \"TSURI",
    \"TSString",
\])

call s:linkGroup('RedshiftLiteralDim', [
    \"pythonBytesEscape",
    \"rubyPercentRegexpDelimiter",
    \"rubyInterpolationDelimiter",
    \"typescriptTemplateSB",
\])

call s:linkGroup('RedshiftSymbol', [
    \"rubySymbol",
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
    \"CocErrorHighlight",
    \"CocHintHighlight",
    \"CocInfoHighlight",
    \"CocWarningHighlight",
    \"ExtraWhitespace",
    \"JavascriptCommentTodo",
    \"SpellBad",
\])

call s:linkGroup('RedshiftAttentionFg', [
    \"ErrorMsg",
    \"NeomakeErrorMsg",
    \"WarningMsg",
\])

" ----

call s:linkGroup('RedshiftChromeNormal', [
    \"NERDTreeCWD",
    \"NERDTreeDir",
    \"NERDTreeExecFile",
    \"NERDTreeFile",
    \"NERDTreeLinkFile",
    \"NERDTreeNodeDelimiters",
    \"qfFileName",
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
    \"qfSeparator",
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
    \"CocFloating",
    \"CocInfoFloat",
    \"CocHintFloat",
    \"CocCodeLens",
    \"PmenuSel",
    \"StatusLine",
    \"TabLineSel",
\])

" ----

call s:linkGroup('RedShiftGreen', [
    \"CocHintSign",
    \"CocInfoSign",
    \"DiffAdd",
    \"GitGutterAdd",
    \"GitGutterAddDefault",
    \"NERDTreeGitStatusModified",
\])

call s:linkGroup('RedShiftYellow', [
    \"CocWarningSign",
    \"DiffChange",
    \"GitGutterChange",
    \"GitGutterChangeDefault",
    \"GitGutterChangeDelete",
    \"GitGutterChangeDeleteDefault",
    \"NERDTreeGitStatusDirDirty",
\])

call s:linkGroup('RedShiftRed', [
    \"CocErrorSign",
    \"DiffDelete",
    \"GitGutterDelete",
    \"GitGutterDeleteDefault",
\])

call s:linkGroup('RedshiftChromeWarning', [
    \"CocWarningFloat",
\])

call s:linkGroup('RedshiftChromeError', [
    \"CocErrorFloat",
\])

" coc TODO
" \"CocErrorVirtualText", " The highlight used for error signs.
" \"CocWarningVirtualText", " The highlight used for warning signs.
" \"CocInfoVirtualText", " The highlight used for information signs.
" \"CocHintVirtualText", " The highlight used for hint signs.

" \"CocHighlightText", " The highlight used for document highlight feature. Normally used for
" \"CocHighlightTextRead", " Highlight for `Read` kind of document symbol.
" \"CocHighlightTextWrite", " Highlight for `Write` kind of document symbol.
"
" \"CocCodeLens", " Highlight group of virtual text for codeLens.
" \"CocFloating", " Highlight group of a floating window.
" \"CocInfoFloat", " The highlight used for a floating window with information.
" \"CocHintFloat", " The highlight used for a floating window with hints.

" Other ------------------------------------------------------------------------

" FZF fuzzy finder color scheme
let g:fzf_colors =
\ { 'fg':      ['fg', 'RedshiftChromeMin'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'RedshiftChromeHighlighted'],
  \ 'fg+':     ['fg', 'RedshiftChromeNormal'],
  \ 'bg+':     ['bg', 'Normal'],
  \ 'hl+':     ['fg', 'RedshiftChromeNormal'],
  \ 'info':    ['fg', 'RedshiftChromeNormal'],
  \ 'border':  ['fg', 'RedshiftChromeNormal'],
  \ 'prompt':  ['fg', 'RedshiftChromeNormal'],
  \ 'pointer': ['fg', 'RedshiftChromeHighlighted'],
  \ 'marker':  ['fg', 'RedshiftChromeNormal'],
  \ 'spinner': ['fg', 'RedshiftChromeNormal'],
  \ 'header':  ['fg', 'RedshiftChromeNormal'] }
