import { WASI, File, OpenFile } from '@bjorn3/browser_wasi_shim';
import { absurd } from './lib';
import * as p from './protocol';
import { HaskellMessage, JavaScriptMessage, JavaScriptMessageTag, HaskellMessageTag, Bindings, List } from './protocol';

export type HaskellPointer = number;

export type HaskellExports = {
  init_greadymem: () => void;
  init_debug: () => void;
  _initialize: () => void;
  hs_malloc: (size: number) => HaskellPointer;
  hs_free: (ptr: HaskellPointer) => void;
  app: (input: HaskellPointer) => HaskellPointer;
  memory: WebAssembly.Memory;
};

export type HaskellIstance = {
  exports: HaskellExports;
  argsCtxStack: List<IArguments>[];
};

export function loadBuffer(inst: HaskellIstance, ptr: HaskellPointer) {
  const b = new Uint8Array(inst.exports.memory.buffer, ptr);
  const len = b[0] +
    (b[1] << 8) +
    (b[2] << 16) +
    (b[3] << 24) +
    (b[4] << 32) +
    (b[5] << 40) +
    (b[6] << 48) +
    (b[7] << 56);
  const buf = (new Uint8Array(inst.exports.memory.buffer, ptr + 8, len)).slice().buffer;
  inst.exports.hs_free(ptr);
  return new Uint8Array(buf);
}

export function storeBuffer(inst: HaskellIstance, u8array: Uint8Array) {
  const len = u8array.byteLength;
  const ptr = inst.exports.hs_malloc(u8array.length + 8);
  // Write the length of the buffer as 8 bytes before the buffer
  const view = new DataView(inst.exports.memory.buffer);
  view.setUint32(ptr, len, true);

  // Copy the buffer into WebAssembly memory
  const dest = new Uint8Array(inst.exports.memory.buffer, ptr + 8, len);
  dest.set(u8array);
  return ptr;
}

export function haskellApp(inst: HaskellIstance, maybeJsMsg?: JavaScriptMessage, argScope: List<IArguments> = null) {
  const jsMsg = maybeJsMsg ? maybeJsMsg : p.mkStartMessage();
  const haskMsg = interactWithHaskell(inst, jsMsg);
  const jsCallback = (jsMsg: JavaScriptMessage, argScope: List<IArguments>) => {
    haskellApp(inst, jsMsg, argScope);
  };
  switch (haskMsg.tag) {
    case HaskellMessageTag.EvalExpr: {
      const result = p.evalExpr(globalContext, argScope, jsCallback, haskMsg.expr);
      const jvalue = p.unknownToJValue(result);
      return haskellApp(inst, { tag: JavaScriptMessageTag.Return, 0: jvalue }, argScope);
    }
    case HaskellMessageTag.Yield: {
      p.evalExpr(globalContext, argScope, jsCallback, haskMsg.expr);
      return;
    }
    case HaskellMessageTag.HotReload: {
      window.location.reload();
      return;
    }
    case HaskellMessageTag.Done: {
      return;
    }
  }
  absurd(haskMsg);
}

const globalContext: List<Bindings> = [window as any, null]

function interactWithHaskell(inst: HaskellIstance, down: JavaScriptMessage): HaskellMessage {
  const downBuf = p.javascriptMessage.encode(down);
  const downPtr = storeBuffer(inst, downBuf);
  const upBuf = loadBuffer(inst, inst.exports.app(downPtr));
  return p.haskellMessage.decode(upBuf);
}

export type StartReactorOptions = {
  greedyMem?: boolean;
};

export async function startReactor(wasmUri: string, opt: StartReactorOptions = {}): Promise<void> {
  const stdoutLogger = consoleLineBuffering(console.log);
  const stderrLogger = consoleLineBuffering(console.log);
  const wasi = new WASI([], [], [
    new OpenFile(new File([])), // stdin
    new OpenFileDebug(new File([]), stdoutLogger), // stdout
    new OpenFileDebug(new File([]), stderrLogger), // stderr
  ]);

  const wasm = await WebAssembly.compileStreaming(fetch(wasmUri));
  const inst = await WebAssembly.instantiate(wasm, {
    "wasi_snapshot_preview1": wasi.wasiImport
  }) as HaskellIstance;
  wasi.inst = inst;
  inst.exports._initialize();
  if (opt.greedyMem) {
    inst.exports.init_greadymem();
  } else {
    inst.exports.init_debug();
  }

  window.addEventListener("beforeunload", () => haskellApp(inst, { tag: JavaScriptMessageTag.BeforeUnload }));

  haskellApp(inst);
};

export class OpenFileDebug extends OpenFile {
  public printDebug: (s: Uint8Array) => void;

  constructor(file: File, printDebug: (s: Uint8Array) => void) {
    super(file);
    this.printDebug = printDebug;
  }

  fd_write(view8: Uint8Array, iovs: Array<any /*wasi.Ciovec*/>): {
    ret: number;
    nwritten: number;
  } {
    const result = super.fd_write(view8, iovs);
    iovs.forEach(iov => {
      this.printDebug(view8.subarray(iovs[0].buf, iov.buf + iov.buf_len));
    });
    return result;
  }
}

// Split given chunks of memory into lines and pass decoded strings
// into the given logger function, buffer unfinished lines if neccessary
function consoleLineBuffering(logger: (s: string) => void): (u8: Uint8Array) => void {
  let buffer: Uint8Array[] = [];
  return (u8: Uint8Array) => {
    const go = (bytes: Uint8Array) => {
      if (bytes.byteLength == 0) return;
      const newLineIndex = bytes.findIndex(b => b == '\n'.charCodeAt(0));
      if (newLineIndex >= 0) {
        const olderBits = buffer.map(u8s => new TextDecoder('utf8').decode(u8s)).join('');
        buffer = [];
        const lastBits = new TextDecoder('utf8').decode(bytes.subarray(0, newLineIndex));
        logger(olderBits + lastBits);
        go(bytes.subarray(newLineIndex + 1));
      } else {
        buffer.push(bytes.slice());
      }
    };
    go(u8);
  };
}
