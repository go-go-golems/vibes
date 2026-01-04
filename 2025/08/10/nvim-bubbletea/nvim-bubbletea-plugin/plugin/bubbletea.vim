" Neovim Bubble Tea Plugin
" This plugin allows running Bubble Tea TUI applications within Neovim buffers

if exists('g:loaded_bubbletea')
  finish
endif
let g:loaded_bubbletea = 1

" Define the main command
command! BubbleTeaDemo lua require('bubbletea').start_demo()
command! BubbleTeaStop lua require('bubbletea').stop()

" Optional: Set up keybindings
nnoremap <leader>bt :BubbleTeaDemo<CR>
nnoremap <leader>bs :BubbleTeaStop<CR>

