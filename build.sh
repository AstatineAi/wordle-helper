#!/usr/bin/env bash

wasm32-wasi-cabal build -f web
obj_path=$(find ./dist-newstyle/ -name "*.wasm" -type f)
frontend_obj_path=./frontend/public/wordle-helper.wasm
"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs -i "$obj_path" -o ./frontend/ghc_wasm_jsffi.js
env -i GHCRTS=-H64m "$(type -P wizer)" --allow-wasi --wasm-bulk-memory true --inherit-env true --init-func _initialize -o $frontend_obj_path "$obj_path"
wasm-opt --low-memory-unused --converge -O4 $frontend_obj_path -o $frontend_obj_path
wasm-tools strip $frontend_obj_path -o $frontend_obj_path

cd frontend && pnpm install && pnpm build
