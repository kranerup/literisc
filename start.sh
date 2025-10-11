# let g:swank_port=5556
SWANK_PORT=5556 sbcl \
  --load $HOME/.local/share/nvim/lazy/slimv/slime/start-swank.lisp --no-linedit
