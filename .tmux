#!/bin/sh

set -e

if tmux has-session -t docvim 2> /dev/null; then
  tmux attach -t docvim
  exit
fi

tmux new-session -d -s docvim -n vim

# 1. Main window: vim.
tmux send-keys -t docvim:vim "vim -c CommandT" Enter

# 2. REPL.
tmux new-window -t docvim -n repl
tmux send-keys -t docvim:repl bin/repl Enter

# 3. Build
tmux new-window -t docvim -n build
tmux send-keys -t docvim:build bin/build Enter

# 4. General shell use.
tmux new-window -t docvim

# 5. vim-docvim plugin
tmux new-window -t docvim -c "$HOME/.vim/pack/bundle/start/vim-docvim" -n vim-docvim
tmux send-keys -t docvim:vim-docvim "vim -c CommandT" Enter
tmux split-window -t docvim:vim-docvim -h -c "$HOME/.vim/pack/bundle/start/vim-docvim"
tmux send-keys -t docvim:vim-docvim.right "git status" Enter

tmux attach -t docvim:vim
