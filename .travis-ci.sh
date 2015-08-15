# Dependencies
APT_DEPENDS="opam g++"
OPAM_DEPENDS="ocamlfind ppx_deriving js_of_ocaml"

# Install OPAM and $APT_DEPENDS
echo "yes" | sudo add-apt-repository ppa:avsm/ocaml42+opam12
sudo apt-get update -qq
sudo apt-get install -qq ${APT_DEPENDS}

# Install OCaml
export OPAMYES=1
export OPAMVERBOSE=1
opam init
eval `opam config env`

# Show OCaml and OPAM versions
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

# Install $OPAM_DEPENDS
opam install ${OPAM_DEPENDS}

# Test
./configure --enable-tests
make
make test
