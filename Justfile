build:
    # TODO: Re-enable the below after fixing the flake/shell for this repo
    # cd grammars/commonlisp
    # tree-sitter build --wasm
    # cd ../../
    # cp grammars/commonlisp/tree-sitter-commonlisp.wasm grammars/commonlisp.wasm
    cargo component build --target wasm32-wasip1
    cp target/wasm32-wasip1/debug/zed_common_lisp.wasm extension.wasm

clean:
    cargo clean
