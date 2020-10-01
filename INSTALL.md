
You will need to install [OPAM](http://opam.ocaml.org), for instance via `APT` on Ubuntu or `brew` on macOS:

    apt install -y opam
    brew install opam

If you have set up OPAM in the past, you can probably skip the next step. EBA should work with the 4.07 version of OCaml --- let us know if it doesn't.

If you are installing OPAM for the first time:

    opam init --auto-setup
    opam switch 4.07.0
    eval `opam config env`

Setup customized CIL for EBA:

    git clone https://github.com/IagoAbal/eba-cil.git
    opam pin add -n cil eba-cil/

Build EBA itself:

    git clone https://github.com/IagoAbal/eba.git
    opam pin add -y -n eba eba/
    opam install eba --deps-only
    cd eba
    make

And you will find the EBA binary in `bin/eba`.
