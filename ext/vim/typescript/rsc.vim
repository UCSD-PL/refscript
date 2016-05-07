"============================================================================
"File:        nanojs.vim
"Description: nanojs checking plugin for syntastic.vim
"Maintainer:  Ranjit Jhala <jhala at cs dot ucsd dot edu>
"License:     BSD
"============================================================================

if exists('g:loaded_syntastic_typescript_nanojs_checker')
    finish
endif
let g:loaded_syntastic_typescript_nanojs_checker = 1


let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_typescript_nanojs_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': 'liquid',
        \ 'fname'    : syntastic#util#shexpand('%:p')})

    let errorformat =
        \ '%E%f:%l:%v: Error: %m,' .
        \ '%E%f:%l:%c-%*[0-9]: Error: %m,' .
        \ '%E%f:%l:%c-%*[0-9]:%*[0-9]: Error: %m,' .
        \ '%W%f:%l:%v: Warning: %m,' .
        \ '%C%m'

    let retVals = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'vcol': 1},
        \ 'postprocess': ['compressWhitespace'] })

    if exists("g:loaded_vim_annotations")
      call annotations#LoadAnnsDefault()
    endif

    return retVals
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'typescript',
    \ 'name': 'nanojs'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
