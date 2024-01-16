mydir=$(mktemp -d "${TMPDIR:-/tmp/}$(basename $0).XXXXXXXXXXXX")

wat2wasm --enable-all --debug-names "./$1.wat" -o "$mydir/tmp.wasm"
wasm-opt -g -O4 "$mydir/tmp.wasm" -o "$mydir/tmp.opt.wasm"
wasm2wat "$mydir/tmp.opt.wasm" -o "./$1.opt.wat"
