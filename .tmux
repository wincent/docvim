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
tmux send-keys -t docvim:repl "stack repl" Enter

# 3. Build
tmux new-window -t docvim -n build
tmux send-keys -t docvim:build "stack build --file-watch" Enter

# 4. General shell use.
tmux new-window -t docvim

tmux attach -t docvim:vim
