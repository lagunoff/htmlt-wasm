import { WASI, File, OpenFile, PreopenDirectory } from "@bjorn3/browser_wasi_shim";
import * as reactor from './reactor';

let args = ["bin", "arg1", "arg2"];
let env = ["FOO=bar"];
let fds = [
  new OpenFile(new File([])), // stdin
  new OpenFile(new File([])), // stdout
  new OpenFile(new File([])), // stderr
  new PreopenDirectory(".", {
    "example.c": new File(new TextEncoder("utf-8").encode(`#include "a"`)),
    "hello.rs": new File(new TextEncoder("utf-8").encode(`fn main() { println!("Hello World!"); }`)),
  }),
];
let wasi = new WASI(args, env, fds);


let wasm = await WebAssembly.compileStreaming(fetch("todomvc/todomvc.wasm"));
let inst = await WebAssembly.instantiate(wasm, {
  "wasi_snapshot_preview1": wasi.wasiImport,
  env : {
    myExportedFunction: function() { alert("From WASM!"); }
  },
});
wasi.inst = inst;

inst.exports.hs_init(0, 0);

reactor.haskellApp(inst);

function haskellApp(buf) {
  const ptrBuf = reactor.storeBuffer(inst, buf);
  return reactor.loadBuffer(inst, inst.exports.app(ptrBuf));
}

function haskellAppString(str) {
  const ptrBuf = reactor.storeBuffer(inst, new TextEncoder("utf8").encode(str));
  return new TextDecoder("utf8").decode(reactor.loadBuffer(inst, inst.exports.app(ptrBuf)));
}
