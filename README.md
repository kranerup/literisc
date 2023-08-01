# literisc
The LiteRISC processor

On a fresh Ubuntu install you would need to do the following to install
SBCL Common Lisp and then run the LiteRISC emulator.

sudo apt install sbcl
sudo apt install -y git
sudo snap install curl
sudo apt install ncurses-dev
curl -O https://beta.quicklisp.org/quicklisp.lisp
sudo apt install build-essential
sbcl --load quicklisp.lisp 
git clone git@github.com:kranerup/literisc.git liteRISC
cd liteRISC
sbcl --load run.lisp

