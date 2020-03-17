#!/bin/bash

#set -eux

SESSION=ciseau
EDITOR=vim
LOGS=/tmp/ciseau.log

tmux kill-session -t $SESSION || echo "no previous session"

tmux new-session -P -d -s $SESSION -n 'Editor' $EDITOR .

tmux new-window -P -n 'Build'
tmux send-keys '. Build' C-m

[ - $LOGS ] || touch $LOGS

tmux new-window -P -n 'Log'
tmux send-keys "tail -f " $LOGS C-m

tmux select-window -t $SESSION:1
tmux -2 attach-session -t $SESSION
