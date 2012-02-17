" Tabline.vim {{{

" Original Author:       Eric Arnold ( eric_p_arnold@yahoo.com )
" Last Change: 2012 Feb 17

" Configuration variables section {{{
let g:TabLineSet_min_tab_len = 5        " minimum tab width (space padded)
let g:TabLineSet_max_tab_len = 30
                                        " try some smaller number, i.e. 30
                                        " when using 'full_path' option
let g:TabLineSet_max_cols = &columns    
                                        " you might want to set it higher than
                                        " &columns for GUI with scroll buttons
let g:TabLineSet_verbose_auto = 4       " turn on/off verbose at this tab width
let g:TabLineSet_max_wrap = 1           " maximum rows to wrap tabline into
                                        " Value can be 1+
" Readonly, but could be useful:
if !exists('g:TabLineSet_tab_status')
    let g:TabLineSet_tab_status = {}
endif
" Masterlist:  do not change.
let s:all_verbose_options = 
    \ [ 
    \   'modified', 'windows', 'full_path', 'buffers_list', 'closers', 
    \   'tabnr', 'winnr', 'bufnr', 'filler_func'
    \ ]
" You can config this in place here, or add() to it (see below):
let g:TabLineSet_verbose_sets = 
    \ [
        \ [ 
        \   'modified', 'windows', 'buffers_list', 'closers', 
        \   'tabnr', 'winnr', 'bufnr', 'filler_func'
        \ ],
        \ [ 
        \   'modified', 'windows', 'buffers_list', 'closers', 
        \   'tabnr', 'winnr', 'bufnr'
        \ ],
        \ [ 'modified', 'windows', 'buffers_list', 'closers', 'filler_func' ],
        \ [ 'modified', 'windows', 'closers' ],
        \ [ 'modified', 'windows', 'closers', 'full_path' ],
        \ [ 'buffers_list' ],
        \ [ ]
    \ ]
        " ^^^
        "  |
        "  +-----------    Your option list(s) here:
        "                  or
        "  Here:
        "  |
        "  V
call add( g:TabLineSet_verbose_sets, [ 'closers', 'buffers_list', 'filler_func' ] )
" As promised, there is still a string variable for the options list
" which can be set like:
"
"       let g:TabLineSet_verbose = 'modified,windows,closers'
"
" even though here it is being set from the lists above:
"
let g:TabLineSet_verbose = join( g:TabLineSet_verbose_sets[0], ',' )
"
" You should then probably add it to the option/verbose-level set, so 
" it doesn't get clobbered if you rotate through the sets:
"
"       call add( g:TabLineSet_verbose_sets, [ g:TabLineSel_verbose ] )
"
" This nested list holds substitution triplets that well be applied to each
" buffer name via  substitute()  before it is added to the tab.   Note that
" this script does a pretty brute force method of shrinking the tabs to fit by
" looping a lot.  If you add bunches of filters, it might affect performance.
"
"

let g:TabLineSet_bufname_filters = [ 
        \   [ '\[No Name\]'      , '❮❯'        ] , 
        \   [ '\[fuf\]'          , '❮F❯'       ] , 
        \   [ '__Tagbar__'       , '❮T❯'       ] , 
        \   [ '__Gundo.*__'      , '❮G❯'       ] , 
        \   [ '__Scratch__'      , '❮S❯'       ] , 
        \   [ 'NERD_tree[_0-9]*' , '❮N❯'       ] , 
        \   [ 'ControlP'         , '❮P❯'       ] , 
        \   [ '^--*\(.*\)--*$'   , '\1'          , '' ]
        \ ]
        " The first example filters out all unnamed buffers.
        " The second example filters out --minibufexplorer-- type lines that
        " show up in all tabs if the minibufexplorer is used.
" The following allows you to define substitutions on each tab as a whole.
" This begins to get tricky, since what you see at the top of your Vim is a
" small part of the format string that is sent to the tabline formatter.
" You're on your own as to whether it messes up the tab's formatting, length,
" etc.
let g:TabLineSet_bufname_filters = [ 
        \   [ '\[No Name\]'      , ''        ] , 
        \   [ '\[fuf\]'          , 'F'       ] , 
        \   [ '__Tagbar__'       , 'T'       ] , 
        \   [ '__Gundo.*__'      , 'G'       ] , 
        \   [ '__Scratch__'      , 'S'       ] , 
        \   [ 'NERD_tree[_0-9]*' , 'N'       ] , 
        \   [ 'ControlP'         , 'P'       ] , 
        \   [ '^--*\(.*\)--*$'   , '\1'          , '' ]
        \ ]
"
let g:TabLineSet_tab_filters = [
        \   [ ',',       nr2char('0x2502'),   'g' ]
        \ ]
        "\  [ '%#TabPunct\w*#,%#Tab\w*#',        ';',   'g' ]
        " This example removes the commans and their highlighting, and
        " replaces them with semi-colons.
if 0    "  don't execute, example only


" The folowing example replaces the leading "!" to include the current time
" using the substitute special \= evaluation feature.  
"
" First, clean out any copies of our changes because the tab length
" calculation makes multiple passes, each of which would other wise insert
" another timestamp.
call add( g:TabLineSet_tab_filters,     [ '%X%#DiffChange#\d\d:\d\d:\d\d',       '',    'g' ] )
" Note also that the new current time string would inherit the color of the
" "!" char, if it didn't include the %X%#..#  format string around the "!" .
call add( g:TabLineSet_tab_filters,     [ '!%X%#[^#]*#',         '\=MyFunc()',  '' ] )
function! MyFunc()
    let s = strftime("%H:%M:%S")
    " Since this increases the length of the tab outside of the TabLineSet
    " functions, so incrementing the  g:TabLineSet_out_pos to account for
    " the extra chars will help it, a little, to draw.  
    let g:TabLineSet_out_pos += strlen(s)
    return submatch(0) . '%X%#DiffChange#' . s
endfunction
endif   " end of don't execute
" This performs substitutions at the full tabline level.  The possibility to
" make a mess increases :-)
"
let g:TabLineSet_tabline_filters = [ 
        \ ]
        "\  [ '^',      '(-:  ' ],
        "\  [ '$',       ' :-)' ]
" This holds a copy of the final (huge!) string with all the embedded syntax
" and or highlighting.  You can use it to help decide how you want to make
" filters.
"
let g:TabLineSet_output_pre = ''
let g:TabLineSet_output_post = ''

" Use the filler func to doddle in the ending  space in the tabline:
"
let g:TabLineSetFillerFunc = 'TabLineSetFillerTest'
" let g:TabLineSetFillerFunc = ''
" This is called for each evaluation pass and can set the initial
" value of the tabline string.
"
let g:TabLineSet_preproc_func = ''
"let g:TabLineSet_preproc_func = 'Tst_preproc'
" This is passed the final tabline string.  It's a final hook for whatever.
"
let g:TabLineSet_postproc_func = ''
"let g:TabLineSet_postproc_func = 'Tst_postproc_modified'
"  End config vars  }}}

" TabLineSet_main() {{{

" These are all static script vars so that it will handle the way guitablabel
" re-enters for each tab:
"
let s:tabline_out = ''
let s:tabline_pieces = {}
let g:TabLineSet_tablabels = {}

function! TabLineSet_main( ... )
    set guioptions-=e
    if !exists('s:called_hl_init')
        let s:called_hl_init = 1
        call TabLineSet_hl_init()
    endif
    if synIDattr(synIDtrans(hlID("TabPunct")), "fg") == ''
        call TabLineSet_hl_init()
    endif
    " ------------------------------------------------------------
    " Don't recalc unless something has changed:
    "
    let s:check_bufnrs = []
    let t = {}
    for tabnr in range( 1, tabpagenr('$') )
        for bufnr in  tabpagebuflist( tabnr )
            if index( s:check_bufnrs, bufnr ) < 0
                call add( s:check_bufnrs, bufnr )
            endif
            let t[bufnr] = {}
            let t[bufnr].tabnr = tabnr
            let t[bufnr].winnr = tabpagewinnr( bufwinnr( bufnr ) )
            " Seems to need bufname as well as bufnr in some cases:
            "
            let t[bufnr].bufname = bufname( bufnr )
            let t[bufnr].modified = getbufvar( bufnr, '&modified' )
            let t[bufnr].curr_window = 
                        \( tabpagenr() && winnr() == tabpagewinnr( tabnr ) )
        endfor
    endfor
    let t.curr_tabnr = tabpagenr()
    let t.wrap = g:TabLineSet_max_wrap
    let t.max_cols = g:TabLineSet_max_cols
    let t.max_tab_len = g:TabLineSet_max_tab_len
    let t.verbose = g:TabLineSet_verbose
    let t.columns = &columns
    " for key in keys(t)
    " if t[key] != g:TabLineSet_tab_status[key]
    " echomsg 'diff ' . key . ' = ' . string( t[key] ) . ' != ' . string( g:TabLineSet_tab_status[key] )
    " endif
    " endfor
    if t == g:TabLineSet_tab_status
                \ && g:TabLineSet_output_post != ''
        return g:TabLineSet_output_post
    endif
    let g:TabLineSet_tab_status = deepcopy(t)
    "
    " End recalc calc
    " ------------------------------------------------------------
    return s:Fill_tab_labels()
endfunction

function! s:Fill_tab_labels()
    let s:verbose = g:TabLineSet_verbose
    let s:min_tab_len = g:TabLineSet_min_tab_len
    let g:TabLineSet_tablabels = {}
    " Must check bufnames instead of numbers, since when opening a file name
    " from a no-name buffer, the bufnr doesn't seem to change.
    "
    for bufnr in s:check_bufnrs
        if has_key( s:bufnames, bufnr ) && bufname( bufnr ) == s:bufnames[bufnr]
        else
            call s:Fill_bufnames()
            break
        endif
    endfor
    let usable_columns = max( [ &columns , g:TabLineSet_max_cols ] )
    let usable_columns = usable_columns * g:TabLineSet_max_wrap
    let s:avail = usable_columns
    let s:tabline_out = ''
    let s:overflow = 1
    let loop = 0
    while ( loop < 10 ) && ( s:avail > 1 ) && ( s:overflow > 0 )
        "echomsg 'loop: ' . loop . ', overflow: ' . s:overflow
        let loop += 1
        " g:TabLineSet_out_pos will hold the total number of chars, as they
        " will appear in the tab line (without formatting strings).
        "
        let g:TabLineSet_out_pos = 0
        let g:TabLineSet_row = 0
        let g:TabLineSet_col = 0
        let g:TabLineSet_idxs = ''
        let tabs_overflow = 0
        "       if s:verbose =~ 'buffers_list'
        "           "let s:min_tab_len = g:TabLineSet_min_tab_len * 2
        "           let s:min_tab_len = 15
        "       endif
        " ------------------------------------------------------------
        "
        "  Pre-processing custom regex:
        "
        let tabline_out = ''
        if g:TabLineSet_preproc_func != ''
            let tabline_out = {g:TabLineSet_preproc_func}( )
        endif
        let tabnrs = range( 1, tabpagenr('$') )
        " ------------------------------------------------------------
        "
        " Info gathering tab page loop
        " 
        for tabnr in tabnrs
            let s:tabline_pieces[tabnr] = {}
            let is_selected = ( tabnr == tabpagenr() ) 
            let bufnr_list = tabpagebuflist( tabnr )
            let tab_curr_winnr = tabpagewinnr( tabnr )
            let numwins = tabpagewinnr( tabnr, ("$") )
            let tablabel = ''
            let tablabel_len = 0

            exec "let tabline_unselected = '%#TabLine".tabnr."#'"
            exec "let tabexit_unselected = '%#TabExit".tabnr."#'"
            exec "let tabmod_unselected = '%#TabMod".tabnr."#'"

            let tablabel .= is_selected ? '%#TabLineSel#' : tabline_unselected
            " ------------------------------------------------------------
            " Misc values
            "
            let s:tabline_pieces[tabnr].misc_vals = ''
            let numwins_out = ''
            if is_selected == 0 && s:verbose =~ 'windows'  && len( bufnr_list ) > 1
                let numwins_out = numwins
            endif
            let tabnr_out = ''
            if s:verbose =~ 'tabnr'
                let tabnr_out .= tabnr
            endif
            let winnr_out = ''
            if s:verbose =~ 'winnr' && is_selected
                let winnr_out .= '' . tab_curr_winnr . ''
            endif
            let bufnr_out = ''
            if s:verbose =~ 'bufnr' && is_selected
                let bufnr_out .= 'b' . winbufnr( tab_curr_winnr )
            endif
            if s:tabline_pieces[tabnr].misc_vals != ''
                let r_brac = ''
                let l_brac = ':'
                let tablabel .= 
                            \   r_brac
                            \ . ( is_selected ? '%#TabWinNumSel#' : '%#TabWinNum#' )
                            \ . s:tabline_pieces[tabnr].misc_vals
                            \ . ( is_selected ? '%#TabLineSel#' : tabline_unselected )
                            \ . l_brac
                let tablabel_len += strlen( s:tabline_pieces[tabnr].misc_vals
                            \ . r_brac . l_brac )
            endif
            "
            " End Misc values, i.e. the number of windows in the tab:
            "
            " ------------------------------------------------------------
            " ------------------------------------------------------------
            "
            " Gather bufnames
            "
            "           let winnr_start = 1
            "           let winnr_stop = numwins
            "           if s:verbose !~ 'buffers_list'
            "               let winnr_start = tab_curr_winnr
            "               let winnr_stop = tab_curr_winnr
            "           endif
            let s:tabline_pieces[tabnr].curr_bufnr =
                        \ bufnr_list[ tab_curr_winnr - 1 ]
            if s:verbose =~ 'buffers_list'
                let s:tabline_pieces[tabnr].bufnr_list = bufnr_list
            else
                let s:tabline_pieces[tabnr].bufnr_list = 
                            \ [ s:tabline_pieces[tabnr].curr_bufnr ]
            endif
            " ------------------------------------------------------------
            " Add an indicator that some buffer in the tab is modified:
            "
            let s:tabline_pieces[tabnr].modded_chars = ''
            for bufnr in bufnr_list
                if s:verbose =~ 'modified' && getbufvar( bufnr,  '&modified' ) > 0
                    let s:tabline_pieces[tabnr].modded_chars = '+'
                    let tablabel .= (is_selected ? '%#TabMod#' : tabmod_unselected) 
                    let tablabel .= nr2char("0x2551") . ( is_selected ? '%#TabLineSel#' : tabline_unselected )
                    let tablabel_len += 1
                    break
                endif
            endfor
            let save_tablabel_len = tablabel_len
            let stop = 0
            while !stop
                "
                " Loop is for resetting/growing bufnames into available space
                " due to s:min_tab_len
                let linesep = ','
                let out_bufname_list = []
                let tablabel_len = save_tablabel_len
                for bufnr in s:tabline_pieces[tabnr].bufnr_list
                    let out_bufname = s:bufnames[ bufnr ] 
                    if buflisted(bufnr)
                        let out_bufname = out_bufname
                        " let out_bufname = bufnr . ':' . out_bufname
                    endif
                    let tablabel_len += strlen( out_bufname )
                    if is_selected
                                \ && s:verbose =~ '\(tabnr\|winnr\|bufnr\)' 
                                \ && s:tabline_pieces[tabnr].curr_bufnr == bufnr
                                \ && len( s:tabline_pieces[tabnr].bufnr_list ) > 1
                        let out_bufname = 
                                    \ '%#TabWinSel#'. out_bufname 
                                    \ . '%#TabLineSel#'
                        " \ ','.'%#TabWinSelLeft#'. nr2char('0x2B82')
                        " \ . '%#TabWinSelRight#'.nr2char('0x2B80') 
                        " \ . '%#TabLineSel#' . ','
                        " \ . '>'
                        " \ . '<'
                    endif
                    call add( out_bufname_list, out_bufname )
                endfor
                let sep = ''
                            \ . linesep
                " \ . ( is_selected ? '%#TabWinSelRight#'.nr2char('0x2B80') : linesep )
                " \ . ( is_selected ? '%#TabLineSel#' : '' )
                let tabbufnames = join( out_bufname_list, sep )
                " let tabbufnames = substitute( tabbufnames, sep.sep, ' ','g' )
                let tablabel_len += 1 * len( out_bufname_list )     " add in separators len
                "
                " If there is extra space because of min_tab_len, grow the
                " names if possible:
                if tablabel_len < ( s:min_tab_len - 3 )
                    let bufs = {}
                    for bufnr in  s:tabline_pieces[tabnr].bufnr_list
                        let bufs[ bufnr ] = s:bufnames_orig[ bufnr ]
                    endfor
                    let bufs_len = strlen( join( values( bufs ), ',' ) )
                    "echomsg 'checking ' . string(bufs)
                    "echomsg bufs_len .' >  '. strlen( tabbufnames )
                    if bufs_len > strlen( tabbufnames )
                        let avail = ( s:min_tab_len - save_tablabel_len  -  3 )
                        let shrink = ( bufs_len - avail )
                        if shrink > 0
                            let bufs = s:Shrink_bufnames( bufs, shrink )
                        endif
                        for bufnr in  keys( bufs )
                            let s:bufnames[ bufnr ] = bufs[ bufnr ]
                        endfor
                        "echomsg 'reset ' . string(bufs)
                        " restart the loop to re-format and reset tablabel_len
                        continue
                    endif
                endif
                " end bufnames formatting section
                " ------------------------------------------------------------
                break
            endwhile
            "echomsg 'min len ' . s:min_tab_len . ', label len ' . tablabel_len . ', s: ' . tabbufnames
            let tab_pad = max( [ 0, s:min_tab_len - ( 3 + tablabel_len ) ] )
            if tab_pad
                "echomsg 'padding ' . tab_pad . ' for ' . tabbufnames
                let tab_pad = repeat( ' ', tab_pad )
                let tabbufnames .= ''
                            \ . ( is_selected ? '%#TabLineSel#' : tabline_unselected )
                            \ . tab_pad
            endif
            let tablabel_len += tab_pad
            let tabs_overflow += max( [ 0, ( 3 + tablabel_len ) - g:TabLineSet_max_tab_len ] )
            " ------------------------------------------------------------
            "  Closers
            "
            "
            let tabexit = ''
            if s:verbose =~ 'closers'
                let tabexit .= ( is_selected ? '%#TabExitSel#' : tabexit_unselected )
                let tabexit .= ( is_selected ? '%' . tabnr . 'X✖%X' : " ")
                " let tabexit .= ( is_selected ? '%' . tabnr . 'X✖%X ' : '' )
                " let tabexit .= ( is_selected ? '%#TabLineSel#' : tabline_unselected )
            endif

            let tabsep = ''
            if tabnr == tabpagenr('$')
                exec "let tabsep .= (is_selected ?  '%#TabSepSelLast#' : '%#TabSepLast".tabnr."#' )"
            elseif tabnr == tabpagenr()-1
                exec "let tabsep .= '%#TabSepNextSel".tabnr."#'"
            else
                exec "let tabsep .= (is_selected ?  '%#TabSepSel".tabnr."#' : '%#TabSep".tabnr."#' )"
            endif
            let tabsep .= nr2char(0x2b80)

            " ------------------------------------------------------------
            "  Put the pieces together
            "
            let tablabel_len += 2
            let tablabel = '%' . ( tabnr ) . 'T'
                        \ . tablabel . ' ' . tabbufnames . ' ' . tabexit
                        \ . '%T'
                        \ . tabsep
            " \ . ( is_selected ? '%#TabLineSel#' : tabline_unselected )
            "
            " Note: it's important to have the final %T before
            " the separator char, '|', so it won't be a part
            " of the tab, and therefore won't help the
            " wrapping function to break correctly.
            " ------------------------------------------------------------
            "  Tab label custom regex:
            "
            for elem in g:TabLineSet_tab_filters
                while len( elem ) < 3 | call add( elem, '' ) | endwhile
                let tablabel = substitute( tablabel, 
                            \ elem[0], elem[1], elem[2] )
            endfor
            " ------------------------------------------------------------
            "
            "  Start of length calculation
            "
            "           let total += strlen( s:tabline_pieces[tabnr].modded_chars )
            "           let total += strlen( join( s:tabline_pieces[tabnr].bufnr_list, ',' ) )
            "           let total += s:tabline_pieces[tabnr].curr_bufnr > 0 
            "           let total += strlen( r_brac . s:tabline_pieces[tabnr].misc_vals . l_brac )
            "           let total += s:verbose =~ 'closers' 
            " ------------------------------------------------------------
            "  Handle tab wrapping:
            "
            if g:TabLineSet_max_wrap > 1
                " compensate for trailing line space on wrapped tablines
                " created by the internal wrapping [patch]
                "
                if ( ( g:TabLineSet_out_pos + tablabel_len + 1 ) / &columns )
                            \ > g:TabLineSet_row 
                    let g:TabLineSet_row += 1
                    let g:TabLineSet_out_pos += &columns - g:TabLineSet_col
                    let g:TabLineSet_idxs .= 
                                \ repeat( ' ', &columns - g:TabLineSet_col )
                    let g:TabLineSet_col = 0
                endif
            endif
            let s = substitute( tablabel, '%#[^#]*#', '', 'g' )
            let s = substitute( s, '%\d*[=XT]', '', 'g' )
            let tablabel_len = strlen( s )
            let s:tablabel_len = tablabel_len
            let g:TabLineSet_col += tablabel_len
            let g:TabLineSet_out_pos += tablabel_len - 2
            let g:TabLineSet_idxs .= repeat( tabnr, tablabel_len )
            let g:TabLineSet_tablabels[ tabnr ] = tablabel
        endfor " for tabnr in tabnrs
        "
        " --------------------------------------------------
        for tabnr in tabnrs
            let tabline_out .= g:TabLineSet_tablabels[ tabnr ]
        endfor
        " --------------------------------------------------
        "  Final formatting
        "
        " after the last tab fill with TabLineFill and reset tab page nr
        let last_close = ''
            let tabline_out .= '%#TabLineFillEnd#'
            "let last_close = repeat(' ', &columns - g:TabLineSet_col )
            if tabpagenr('$') > 1 && s:verbose == ''
                let last_close .= '%=%#TabLine#%999X!X%X%##'
            endif
            let g:TabLineSet_out_pos += 1
        if exists('&mousefunc')            
            " tabline called from in mousefunc? && &mousefunc != ''
            if g:TabLineSet_max_wrap > 1
                let last_close .= ' <-'
                let g:TabLineSet_out_pos += 1
            endif
            let s = ' ' . g:TabLineSet_max_wrap . ' -> '
            " let g:TabLineSet_out_pos += strlen( s )
            let last_close .= s
        endif
        let a = ( usable_columns - 0 ) 
                    \ - g:TabLineSet_out_pos 
                    \ - ( last_close == '' ? 2 : 0 )
        " TODO Fix filler functions

        if g:TabLineSetFillerFunc != '' && s:verbose =~ 'filler_func'
        let tabline_out .= '%{' . g:TabLineSetFillerFunc . '(' . a . ')}'
        endif

        let tabline_out .= last_close

        let g:TabLineSet_output_pre = tabline_out
        for elem in g:TabLineSet_tabline_filters 
            while len( elem ) < 3 | call add( elem, '' ) | endwhile
            let tabline_out = substitute( tabline_out, 
                        \ elem[0], elem[1], elem[2] )
        endfor
        let g:TabLineSet_output_post = tabline_out
        if g:TabLineSet_postproc_func != ''
            call call( g:TabLineSet_postproc_func, [ tabline_out ] )
        endif
        let s = substitute( tabline_out, '%#[^#]*#', '', 'g' )
        let s = substitute( s, '%\d*[=XT]', '', 'g' )
        let s:overflow = strlen( s ) - s:avail 
        "echomsg 's:' . s
        "echomsg 'avail:' . s:avail . ', overflow end ' . s:overflow
        "echomsg 's len:' . strlen(s)
        "let g:TabLineSet_out_pos += tablabel_len
        if loop == 3 && s:overflow > 0
            let verbose = ""
            continue
        endif
        if loop == 5 && s:overflow > 0
            "echomsg 'clearing min tab len ..........'
            let s:min_tab_len = 1
            continue
        endif
        if s:overflow < tabs_overflow
            let s:overflow = tabs_overflow
        endif
        if s:overflow > 0
            let s:bufnames = s:Shrink_bufnames( s:bufnames, s:overflow + 1 )
        endif
        "echomsg 'tabs o '.tabs_overflow
        "echomsg 'shrunk ' . string( s:bufnames )
        if s:longest_bufname < g:TabLineSet_verbose_auto 
            let s:save_verbose = s:verbose
            let s:verbose = '' 
            "echomsg 'verbose off'
            call s:Fill_bufnames()
            "       elseif s:verbose == ''
            "       \ && s:longest_bufname >= g:TabLineSet_verbose_auto 
            "           let s:verbose = s:save_verbose
            "           echomsg 'verbose on'
            "           call s:Fill_bufnames()
        endif
    endwhile " big loop
    if v:lnum > 0
        return g:TabLineSet_tablabels[ v:lnum ]
    endif
    return tabline_out
endfunction

" }}}

" ------------------------------------------------------------
" Fill bufnames{} 
"
let s:bufnames = {}
let s:bufnames_orig = {}

function! s:Fill_bufnames()
    let s:bufnames = {}
    for tabnr in range( 1, tabpagenr('$') )
        "let winnr_start = 1
        "let winnr_stop = tabpagewinnr( tabnr, "$")
        "?for winnr1 in range( winnr_start, winnr_stop )
        for bufnr in tabpagebuflist( tabnr )
            "let bufnr = winbufnr( winnr1 )
            let bufname = bufname( bufnr )
            if s:verbose =~ 'full_path'
                let bufname = fnamemodify( bufname, ':p' )
            else
                let bufname = fnamemodify( bufname, ':t' )
            endif
            if bufname == ''
                let bufname = '[No Name]'
            endif
"           while strlen( bufname ) < g:TabLineSet_min 
"               let bufname .= ' '
"           endwhile
            " Custom regex:
            "
            for elem in g:TabLineSet_bufname_filters
                while len( elem ) < 3 | call add( elem, '' ) | endwhile
                let bufname = substitute( bufname, 
                            \ elem[0], elem[1], elem[2] )
            endfor
            let s:bufnames[ bufnr ] = bufname
        endfor
    endfor
    let s:bufnames_orig = deepcopy( s:bufnames )
endfunction
"
" End Fill bufnames{} 
"
" ------------------------------------------------------------

function! s:abs( i )
    return max( [ a:i, -1 * a:i ] )
endfunction

" ------------------------------------------------------------
"
" Shrink the names to fit available columns
"
let s:longest_bufname = 0
"let s:shortest_bufname = 0

function! s:Shrink_bufnames( bufnames, shrink )
    let bufnames = a:bufnames
    function! Dict_by_strlen(i1, i2)
        let len1 = strlen( s:sort_dict[ a:i1 ] )
        let len2 = strlen( s:sort_dict[ a:i2 ] )
        return len1 == len2 ? 0 : len1 > len2 ? 1 : -1
    endfunction
    let s:sort_dict = bufnames
    let bufnames_keys_by_len = reverse( sort( keys( s:sort_dict ), "Dict_by_strlen") )
    let bufnames_count = min( [ 5, len( bufnames_keys_by_len ) ] )
    if bufnames_count < 1 | return [] | endif
    let s:longest_bufname = strlen( bufnames[ bufnames_keys_by_len[0] ] )
"   let s:shortest_bufname = s:longest_bufname
    let bufnames_joined_len = strlen( join( values( bufnames ), '' ) )
    let shrink = s:abs( a:shrink )
    let reduced_total = 0
    let loop_counter = 0
    "echomsg string( bufnames_keys_by_len )
    "echomsg string( bufnames )
    "echomsg 'longest ..... ' . s:longest_bufname
    "while bufnames_joined_len >= a:avail
    while reduced_total < shrink
        let reduced = 0
        " Too slow to use increment of 1, so:
        "let incr = s:longest_bufname * ( ( shrink - reduced_total ) / bufnames_count )
        let incr = ( shrink - reduced_total ) / bufnames_count
        let incr = min( [ incr, ( s:longest_bufname / 2 ) ] )
        if incr < 1 | let incr = 1 | endif
        "echomsg ' shrink:' . shrink . ', total:'.reduced_total . '=' . ( shrink -reduced_total) . ', incr:' . incr . ', bufn len:' . bufnames_count
        " Preserves sorted order for keys:
        for bufnr in bufnames_keys_by_len
            if strlen( bufnames[ bufnr ] ) >= s:longest_bufname
    let loop_counter += 1
    "echomsg 'cutting ' . bufnames[bufnr] . ', len=' . strlen( bufnames[ bufnr ] ) . ', longest='  . s:longest_bufname . ', incr=' . incr
"               if strlen( bufnames[ bufnr ]  ) == 0
"                   "continue
"               else
"                   let s:shortest_bufname = 
"                       \ min( [ s:shortest_bufname, strlen( bufnames[ bufnr ]  ) - incr ] )
"               endif
                if bufnames[ bufnr ] =~ '[/\\]'
                    let bufnames[ bufnr ]  = bufnames[ bufnr ][ incr :] 
                else
                    let bufnames[ bufnr ]  = bufnames[ bufnr ][0:0 - incr - 1]
                endif
                let reduced += incr
                let reduced_total += incr
                let bufnames_joined_len -= incr
                "if bufnames_len < a:avail
                if reduced_total < shrink
                    break
                endif
            else
            endif
        endfor
        if !reduced 
            let s:longest_bufname -= incr
        endif
    endwhile
    "echomsg 'shrink loop count ' .  loop_counter 
    return bufnames
endfunction
"
" End Shrink the names to fit available columns
"
" ------------------------------------------------------------

function! Tst_preproc( tabline )
    return '[test]' . a:tabline
endfunction

let s:modified_table = {}
function! Tst_postproc_modified( tabline )
    let updated = []
    for bufnr in range( 1, bufnr("$") )
        if !bufexists( bufnr ) | continue | endif
        let modded = getbufvar( bufnr, '&modified' )
        if !has_key( s:modified_table, bufnr )
            let s:modified_table[ bufnr ] = modded
        endif
        if s:modified_table[ bufnr ] != modded
            let s:modified_table[ bufnr ] = modded
            call add( updated, [ bufnr, modded ] )
        endif
    endfor

    for elem in updated
        let bufnr = elem[0]
        let modded = elem[1]
        echomsg 'bufnr#' . bufnr . ', ' . bufname( bufnr ) . ' is now ' . ( modded ? '' : 'no' ) . 'modified'
    endfor

    " call somefunction( updated )

endfunction
        

"                          Misc functions                              {{{
" -------------------------------------------------------------------------
"
" 

function! TabLineSetFillerNull( avail )
    return ''
endfunction

let s:test_count = 0
function! TabLineSetFillerTest( avail )
    " let out = strftime( '%H:%M:%S' ) . '#' . s:test_count
    let bufs = {}
    for i in range(bufnr('$'))
        if bufwinnr(i+1) < 0 && buflisted(i+1)
            let bufs[ i+1 ] = bufname(i+1)
        endif
    endfor

    let bufbefore = []
    let bufafter = []
    let bufname_full = ''
    let curbuf = bufnr('%')
    let altbuf = bufnr('#')
    if len(bufs) > 0
        for bufnr in  keys( bufs )
            let bufname = bufs[ bufnr]
            let bufname = (bufname=='') ? '[]' : bufname
            let bufno = (bufnr==altbuf) ? '#' : bufnr
            let bufname = bufno . ':' . fnamemodify( bufname, ':t' )
            if bufnr > curbuf
                call add(bufafter, bufname)
            else
                call add(bufbefore, bufname)
            endif
        endfor
        let buf_ring = bufafter + bufbefore
        let bufname_full = join(buf_ring,'|') . '  '
    endif

    if has("win32") || has("win64")
        let comp_name = tolower(expand('$COMPUTERNAME'))
    else
        let comp_name = tolower(expand('$HOSTNAME'))
    endif

    let comp_name = '(' . comp_name . ')'
    let out = bufname_full 
    ". strftime( '%H:%M' ) . ' B:' . bufnr('$') . ' T:' . tabpagenr('$')
    if strlen( out ) > a:avail
        let out = ''
    else
        while strlen( out ) <= a:avail
            let out = ' '. out
        endwhile
    endif
    return out
endfunction

let s:TabLineSet_verbose_save = ''

function! TabLineSet_verbose_toggle()
    call TabLineSet_hl_init()
    call TabLineSet_verbose_toggle0()
    call s:Force_tabline_update()
endfunction

" Have it split up to use this internally, when a "1 new" will fail in the
" sandbox.
function! TabLineSet_verbose_toggle0()
    if s:TabLineSet_verbose_save == ''
        let s:TabLineSet_verbose_save = g:TabLineSet_verbose
        let g:TabLineSet_verbose = ''
    else
        let g:TabLineSet_verbose = s:TabLineSet_verbose_save
        let s:TabLineSet_verbose_save = ''
    endif

endfunction

function! s:Force_tabline_update()
    " Make it update:
    1new
    quit
endfunction

let s:all_verbose_sets_idx = 0

function! TabLineSet_verbose_rotate()
    call TabLineSet_hl_init()
    let s:all_verbose_sets_idx = s:all_verbose_sets_idx + 1
    if s:all_verbose_sets_idx > len( g:TabLineSet_verbose_sets ) - 1
        let s:all_verbose_sets_idx = 0
    endif

    let g:TabLineSet_verbose = join( 
                \g:TabLineSet_verbose_sets[ s:all_verbose_sets_idx ], ',' )
    "silent! normal! gtgT
    echomsg 'Tabline options: ' . g:TabLineSet_verbose
    call s:Fill_tab_labels()
    1new
    quit
endfunction

" End Misc functions  }}}

"                          Highlighting (configurable)                  {{{
" -------------------------------------------------------------------------
"
" 

set tabline=%!TabLineSet_main()

if &showtabline < 1
    set showtabline=1   " 2=always
endif

function! TabLineSet_hl_init()
    "                           *cterm-colors*
    "       NR-16   NR-8    COLOR NAME ~
    "       0       0       Black
    "       1       4       DarkBlue
    "       2       2       DarkGreen
    "       3       6       DarkCyan
    "       4       1       DarkRed
    "       5       5       DarkMagenta
    "       6       3       Brown, DarkYellow
    "       7       7       LightGray, LightGrey, Gray, Grey
    "       8       0*      DarkGray, DarkGrey
    "       9       4*      Blue, LightBlue
    "       10      2*      Green, LightGreen
    "       11      6*      Cyan, LightCyan
    "       12      1*      Red, LightRed
    "       13      5*      Magenta, LightMagenta
    "       14      3*      Yellow, LightYellow
    "       15      7*      White
    "
    "   The number under "NR-16" is used for 16-color terminals ('t_Co'
        " au CmdWinEnter * hi StatusLine ctermfg=244 guifg=#A6E22E guibg=#080808
        " au CmdWinLeave * hi StatusLine ctermfg=130 guifg=#CD5907 guibg=#FFFFFF

        " au InsertEnter * call InsertStatuslineColor(v:insertmode)
        " au InsertEnter * hi StatusLine ctermfg=161 guifg=#D53873 
        " au InsertLeave * hi StatusLine ctermfg=130 guifg=#CD5907

        " au WinEnter * hi WarningMsg guibg=#CD5907 guifg=#FFFFFF
        " au WinLeave * hi WarningMsg guibg=#808080 guifg=#080808
        
    hi! TabWinNum term=bold,None  gui=bold,None
                \  guifg=black  guibg=DarkGrey

    hi! TabWinNumSel term=bold,None  gui=bold,None
                \   guifg=white guibg=#CD5907

    hi! TabPunct term=bold,None  gui=bold,None
                \  guifg=black  guibg=DarkGrey

    hi! TabPunctSel term=None  gui=None
                \   guifg=white guibg=#CD5907

    hi! TabLineFill term=None  gui=None

    hi! TabLineFillEnd term=None  gui=None
                \   guifg=#F8F8F2 guibg=#1B1E1F

    hi! TabLineSel  term=bold,reverse,None 
                \   guifg=#F8F8F2 guibg=#CD5907 gui=None ctermbg=166 ctermfg=255

    hi! TabLine  term=bold,reverse,None 
                \   guifg=black guibg=darkgrey gui=None

    hi! TabWinSel term=None gui=None guifg=white guibg=#F92672

    hi! TabWinSelRight term=None gui=None guifg=#F92672 guibg=#CD5907
    hi! TabWinSelLeft term=None gui=None guifg=#F92672 guibg=#CD5907

    hi! TabBufSel term=None gui=None,bold

    hi! TabMod term=None  gui=None guibg=#CD5907 guifg=white


    hi! TabExitSel gui=None term=None  guifg=white guibg=#CD5907 ctermbg=166


    hi! TabSepSelLast term=None
                \   guifg=#CD5907 guibg=#1B1E1F

    let greys = [
                \'grey90',
                \'grey80',
                \'grey70',
                \'grey60',
                \'grey50',
                \'grey40',
                \'grey30',
                \'grey20',
                \'grey10',
                \'grey10',
                \'grey10',
                \'grey10',
                \'grey10',
                \'grey10']

    let invgreys = [
                \'black',
                \'black',
                \'grey10',
                \'grey10',
                \'grey20',
                \'grey20',
                \'grey50',
                \'grey60',
                \'grey70',
                \'grey80',
                \'grey90',
                \'grey90',
                \'grey90',
                \'grey90',
                \'grey90',
                \'grey90']


    for i in range(1,10)
        exec "hi! TabSepLast".i." term=None  guifg=".greys[i]." guibg=#1B1E1F"

        exec "hi! TabSep".i." term=None guifg=".greys[i]." guibg=".greys[i+1]

        exec "hi! TabSepNextSel".i." term=None guifg=".greys[i]." guibg=#CD5907"

        exec "hi! TabSepSel".i." term=None guifg=#CD5907 guibg=".greys[i+1]

        exec "hi! TabLine".i."  term=bold,reverse,None guifg=".invgreys[i]." guibg=".greys[i]." gui=None"

        exec "hi! TabExit".i." term=None,bold  guifg=".invgreys[i]." guibg=".greys[i]." gui=None"

        exec "hi! TabMod".i." term=None  guifg=red guibg=".greys[i]." gui=None"
    endfor




endfunction

call TabLineSet_hl_init()

augroup au_tablimit
	au!
    au TabEnter * if tabpagenr('$') > 8 | :close | :echoerr "Too many tabs open" | endif 
augroup END

" End highlighting   }}}

"let g:TabLineSet_tab_status = {}
"let g:TabLineSet_min_tab_len = 30

" }}}

