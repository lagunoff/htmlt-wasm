import { WASI, File, OpenFile, PreopenDirectory } from "@bjorn3/browser_wasi_shim";
import * as reactor from './reactor';

let wasi = new WASI([], [], [
  new OpenFile(new File([])), // stdin
  new OpenFile(new File([])), // stdout
  new OpenFile(new File([])), // stderr
]);

let wasm = await WebAssembly.compileStreaming(fetch("./build/wasm32-wasi/ghc-9.9.20230916/htmlt-wasm-0.1.0.0/x/voting/build/voting/voting.wasm"));
let inst = await WebAssembly.instantiate(wasm, {
  "wasi_snapshot_preview1": wasi.wasiImport
});
wasi.inst = inst;

inst.exports.hs_init(0, 0);

reactor.haskellApp(inst);
