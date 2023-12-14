mydir=$(mktemp -d "${TMPDIR:-/tmp/}$(basename $0).XXXXXXXXXXXX")

wat2wasm "./$1.wat" -o "$mydir/tmp.wasm"
wasm-opt -O4 "$mydir/tmp.wasm" -o "$mydir/tmp.opt.wasm"
wasm2wat "$mydir/tmp.opt.wasm" -o "./$1.opt.wasm"
