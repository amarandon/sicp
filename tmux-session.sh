tmux has-session -t sicp
if [ $? != 0 ]
then
    tmux new-session -s sicp -n vim -d
    tmux send-keys -t sicp 'vim +$ section-2.3.scm' C-m
    tmux split-window -v -p 30 -t sicp
    tmux send-keys -t sicp 'racket -il readline' C-m
    tmux select-pane -t 0
fi
tmux attach -t sicp
