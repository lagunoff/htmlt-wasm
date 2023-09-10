type HaskellPointer = number;

type JSValueRef = number;

type JSFunctionName = string;

type HaskellExports = {
  hs_malloc: (size: number) => HaskellPointer;
  hs_free: (ptr: HaskellPointer) => void;
  app: (input: HaskellPointer) => HaskellPointer;
  memory: WebAssembly.Memory;
};

type HaskellIstance = {
  exports: HaskellExports;
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

export class DecoderBase<A> {
  // @ts-ignore
  readonly _A: A;

  encode(value: A): Uint8Array {
    const decoder = this as Decoder<A>;
    const size = computeSize(decoder, value);
    const u8array = new Uint8Array(size);
    runEncoder(decoder, u8array, 0, value);
    return u8array;
  }

  decode(mem: Uint8Array): A {
    const decoder = this as Decoder<A>;
    const [resultVal, _ptr] = runDecoder(decoder, mem, 0);
    return resultVal;
  }
}

export type Decoder<A> =
  | Int8Decoder<A>
  | Int64Decoder<A>
  | Uint8ArrayDecoder<A>
  | StringDecoder<A>
  | ArrayDecoder<A>
  | RecordDecoder<A>
  | OneOfDecoder<A>
  | RecursiveDecoder<A>
;

export class Int8Decoder<A> extends DecoderBase<A> {
}
export class Int64Decoder<A> extends DecoderBase<A> {
}
export class Uint8ArrayDecoder<A> extends DecoderBase<A> {
}
export class StringDecoder<A> extends DecoderBase<A> {
}
export class ArrayDecoder<A> extends DecoderBase<A> {
  constructor(
    readonly _element: Decoder<any>,
  ) { super(); }
}
export class RecordDecoder<A> extends DecoderBase<A> {
  constructor(
    readonly _description: Record<string, Decoder<any>>,
  ) { super(); }
}
export class OneOfDecoder<A> extends DecoderBase<A> {
  constructor(
    readonly _alternatives: Record<number, Decoder<any>>,
  ) { super(); }
}
export class RecursiveDecoder<A> extends DecoderBase<A> {
  constructor(
    public _self: Decoder<any>,
  ) { super(); }
}

export function computeSize<A>(
  decoder: Decoder<A>,
  value: A
): number {
  if (decoder instanceof Int8Decoder) {
    return 1;
  }
  if (decoder instanceof Int64Decoder) {
    return 8;
  }
  if (decoder instanceof StringDecoder) {
    const str = value as any as string;
    const lengthSize = 8; // How many bytes to encode array length
    const u8array = new TextEncoder().encode(str);
    return lengthSize + u8array.length;
  }
  if (decoder instanceof Uint8ArrayDecoder) {
    const u8array = value as any as Uint8Array;
    const lengthSize = 8; // How many bytes to encode array length
    return lengthSize + u8array.length;
  }
  if (decoder instanceof ArrayDecoder) {
    const array = value as any as any[];
    const lengthSize = 8; // How many bytes to encode array length
    return array.reduce((acc, v) => acc + computeSize(decoder._element, v), lengthSize);
  }
  if (decoder instanceof RecordDecoder) {
    const obj = value as any as Record<any,any>;
    return Object.keys(decoder._description).reduce(((acc, k) =>
      acc + computeSize(decoder._description[k], obj[k])
      ), 0
    );
  }
  if (decoder instanceof OneOfDecoder) {
    const obj = value as any as { tag: number };
    const dscrSize = discriminatorSize(Object.keys(decoder._alternatives).length);
    return dscrSize + computeSize(decoder._alternatives[obj.tag], obj);
  }
  if (decoder instanceof RecursiveDecoder) {
    return computeSize(decoder._self, value);
  }
  return absurd(decoder);
}

export function runDecoder<A>(
  decoder: Decoder<A>,
  mem: Uint8Array,
  ptr: HaskellPointer,
): [A, HaskellPointer] {
  if (decoder instanceof Int8Decoder) {
    const result = mem[ptr] as any as A;
    return [result, ptr + 1];
  }
  if (decoder instanceof Int64Decoder) {
    // Data.Binary encodes the integers with Big Endian encoding, what
    // the heck?
    const val = mem[ptr + 7] +
      (mem[ptr + 6] << 8) +
      (mem[ptr + 5] << 16) +
      (mem[ptr + 4] << 24) +
      (mem[ptr + 3] << 32) +
      (mem[ptr + 2] << 40) +
      (mem[ptr + 1] << 48) +
      (mem[ptr] << 56);
    return [val as any as A, ptr + 8];
  }
  if (decoder instanceof StringDecoder) {
    const len = mem[ptr + 7] +
      (mem[ptr + 6] << 8) +
      (mem[ptr + 5] << 16) +
      (mem[ptr + 4] << 24) +
      (mem[ptr + 3] << 32) +
      (mem[ptr + 2] << 40) +
      (mem[ptr + 1] << 48) +
      (mem[ptr] << 56);
    const strView = mem.subarray(ptr + 8, ptr + 8 + len);
    const resultStr = new TextDecoder("utf8").decode(strView) as any as A;
    return [resultStr, ptr + 8 + len];
  }
  if (decoder instanceof Uint8ArrayDecoder) {
    const len = mem[ptr + 7] +
      (mem[ptr + 6] << 8) +
      (mem[ptr + 5] << 16) +
      (mem[ptr + 4] << 24) +
      (mem[ptr + 3] << 32) +
      (mem[ptr + 2] << 40) +
      (mem[ptr + 1] << 48) +
      (mem[ptr] << 56);
    const resultU8Arr = mem.subarray(ptr + 8, ptr + 8 + len) as any as A;
    return [resultU8Arr, ptr + 8 + len];
  }
  if (decoder instanceof ArrayDecoder) {
    const len = mem[ptr + 7] +
      (mem[ptr + 6] << 8) +
      (mem[ptr + 5] << 16) +
      (mem[ptr + 4] << 24) +
      (mem[ptr + 3] << 32) +
      (mem[ptr + 2] << 40) +
      (mem[ptr + 1] << 48) +
      (mem[ptr] << 56);
    const resultArr = [];
    let jx = ptr + 8;
    for (let i = 0; i < len; i++) {
      const [val, newIx] = runDecoder(decoder._element, mem, jx);
      resultArr.push(val);
      jx = newIx;
    }
    return [resultArr as any as A, jx];
  }
  if (decoder instanceof RecordDecoder) {
    let jx = ptr;
    const resultRec = Object.fromEntries(
      Object.entries(decoder._description).map(([k, dec]) => {
        const [val, newIx] = runDecoder(dec, mem, jx);
        jx = newIx;
        return [k, val];
      })
    );
    return [resultRec as any as A, jx];
  }
  if (decoder instanceof OneOfDecoder) {
    const dscrSize = discriminatorSize(Object.keys(decoder._alternatives).length);
    const [tag, ix1] = readDiscriminator(dscrSize, mem, ptr);
    const [oneValue, ix2] = runDecoder(decoder._alternatives[tag], mem, ix1);
    oneValue['tag'] = tag;
    return [oneValue, ix2];
  }
  if (decoder instanceof RecursiveDecoder) {
    return runDecoder(decoder._self, mem, ptr);
  }
  return absurd(decoder);
}

export function runEncoder<A>(
  decoder: Decoder<A>,
  mem: Uint8Array,
  ptr: HaskellPointer,
  value: A,
): HaskellPointer {
  if (decoder instanceof Int8Decoder) {
    mem[ptr] = value as any as number;
    return ptr + 1;
  }
  if (decoder instanceof Int64Decoder) {
    const val = value as any as number;
    mem[ptr + 7] = val & 0xFF;
    mem[ptr + 6] = (val >> 8) & 0xFF;
    mem[ptr + 5] = (val >> 16) & 0xFF;
    mem[ptr + 4] = (val >> 24) & 0xFF;
    // mem[ptr + 3] = (val >> 32) & 0xFF;
    // mem[ptr + 2] = (val >> 40) & 0xFF;
    // mem[ptr + 1] = (val >> 48) & 0xFF;
    // mem[ptr] = (val >> 56) & 0xFF;
    return ptr + 8;
  }
  if (decoder instanceof StringDecoder) {
    const str = value as any as string;
    const encoder = new TextEncoder();
    const strView = encoder.encode(str);
    const len = strView.length;
    mem[ptr + 7] = len & 0xFF;
    mem[ptr + 6] = (len >> 8) & 0xFF;
    mem[ptr + 5] = (len >> 16) & 0xFF;
    mem[ptr + 4] = (len >> 24) & 0xFF;
    // mem[ptr + 3] = (len >> 32) & 0xFF;
    // mem[ptr + 2] = (len >> 40) & 0xFF;
    // mem[ptr + 1] = (len >> 48) & 0xFF;
    // mem[ptr] = (len >> 56) & 0xFF;
    mem.set(strView, ptr + 8);
    return ptr + 8 + len;
  }
  if (decoder instanceof Uint8ArrayDecoder) {
    const u8Array = value as any as Uint8Array;
    const len = u8Array.length;
    mem[ptr + 7] = len & 0xFF;
    mem[ptr + 6] = (len >> 8) & 0xFF;
    mem[ptr + 5] = (len >> 16) & 0xFF;
    mem[ptr + 4] = (len >> 24) & 0xFF;
    // mem[ptr + 3] = (len >> 32) & 0xFF;
    // mem[ptr + 2] = (len >> 40) & 0xFF;
    // mem[ptr + 1] = (len >> 48) & 0xFF;
    // mem[ptr] = (len >> 56) & 0xFF;
    mem.set(u8Array, ptr + 8);
    return ptr + 8 + len;
  }
  if (decoder instanceof ArrayDecoder) {
    const array = value as any as any[];
    const len = array.length;
    mem[ptr + 7] = len & 0xFF;
    mem[ptr + 6] = (len >> 8) & 0xFF;
    mem[ptr + 5] = (len >> 16) & 0xFF;
    mem[ptr + 4] = (len >> 24) & 0xFF;
    // mem[ptr + 3] = (len >> 32) & 0xFF;
    // mem[ptr + 2] = (len >> 40) & 0xFF;
    // mem[ptr + 1] = (len >> 48) & 0xFF;
    // mem[ptr] = (len >> 56) & 0xFF;
    let jx = ptr + 8;
    for (let i = 0; i < len; i++) {
      jx = runEncoder(decoder._element, mem, jx, array[i]);
    }
    return jx;
  }
  if (decoder instanceof RecordDecoder) {
    const obj = value as Record<string, any>;
    let jx = ptr;
    for (const k in decoder._description) {
      if (Object.prototype.hasOwnProperty.call(decoder._description, k)) {
        jx = runEncoder(decoder._description[k], mem, jx, obj[k]);
      }
    }
    return jx;
  }
  if (decoder instanceof OneOfDecoder) {
    const tag = (value as any)['tag'];
    const dscrSize = discriminatorSize(Object.keys(decoder._alternatives).length);
    mem[ptr] = tag;
    return runEncoder(decoder._alternatives[tag], mem, ptr + dscrSize, value);
  }
  if (decoder instanceof RecursiveDecoder) {
    return runEncoder(decoder._self, mem, ptr, value);
  }
  return absurd(decoder);
}

export function evalExpr(exp: Expr): unknown {
  switch(exp.tag) {
    case ExprTag.Num: {
       return exp[0];
    }
    case ExprTag.Str: {
      return exp[0];
    }
    case ExprTag.Dot: {
      const lhs = evalExpr(exp[0]) as any;
      return lhs[exp[1]];
    }
    case ExprTag.Add: {
      const lhs = evalExpr(exp[0]) as number;
      const rhs = evalExpr(exp[1]) as number;
      return lhs + rhs;
    }
    case ExprTag.Subtract: {
      const lhs = evalExpr(exp[0]) as number;
      const rhs = evalExpr(exp[1]) as number;
      return lhs - rhs;
    }
    case ExprTag.Multiply: {
      const lhs = evalExpr(exp[0]) as number;
      const rhs = evalExpr(exp[1]) as number;
      return lhs * rhs;
    }
    case ExprTag.Divide: {
      const lhs = evalExpr(exp[0]) as number;
      const rhs = evalExpr(exp[1]) as number;
      return lhs / rhs;
    }
    case ExprTag.Var: {
      const globalEnv = window as any;
      return globalEnv[exp[0]];
    }
    case ExprTag.Apply: {
      const lhs = evalExpr(exp[0]) as Function;
      return lhs.apply(undefined, exp[1].map(evalExpr));
    }
    case ExprTag.Call: {
      const lhs = evalExpr(exp[0]) as any;
      const fn = lhs[exp[1]];
      return fn.apply(lhs, exp[2].map(evalExpr));
    }
    case ExprTag.Ref: {
      throw new Error('unimplemented');
    }
  }
}

// TODO: complete implementation
function discriminatorSize(_numAlernatives: number): number {
  return 1;
}

// TODO: complete implementation
function readDiscriminator(_dscrSize: number, mem: Uint8Array, ix: HaskellPointer): [number, HaskellPointer] {
  return [mem[ix], ix + 1];
}

/** Helper for totality checking */
export function absurd(_x: never): any {
  throw new Error('absurd: unreachable code');
}

export const int8 = new Int8Decoder<number>();
export const int64 = new Int64Decoder<number>();
export const string = new StringDecoder<string>();
export const u8array = new Uint8ArrayDecoder<Uint8Array>();

export function array<A>(d: Decoder<A>): ArrayDecoder<A[]> {
  return new ArrayDecoder(d);
}
export function record<fields extends { [k: string]: Decoder<any> }>(fields: fields): RecordDecoder<{[k in keyof fields]: fields[k]['_A'] }> {
  return new RecordDecoder(fields);
}

export type DiscriminateOn<TagKey extends string|number, Descriptor extends Record<string|number, Decoder<any>>> = OneOfDecoder<{ [K in keyof Descriptor]: { [K2 in TagKey]: K } & Descriptor[K]['_A']}[keyof Descriptor]>;

export function discriminate<Descriptor extends Record<string|number, Decoder<any>>>(record: Descriptor): DiscriminateOn<'tag', Descriptor> {
  return new OneOfDecoder(record);
}

export function recursive<A>(f: (self: Decoder<any>) => Decoder<A>): Decoder<A> {
  const self = new RecursiveDecoder<A>(undefined as any);
  const result = f(self);
  self._self = result;
  return result;
}

export enum ExprTag {
  Num,
  Str,
  Dot,
  Add,
  Subtract,
  Multiply,
  Divide,
  Var,
  Apply,
  Call,
  Ref,
}

export type Expr =
  | { tag: ExprTag.Num, 0: number }
  | { tag: ExprTag.Str, 0: string }
  | { tag: ExprTag.Dot, 0: Expr, 1: string }
  | { tag: ExprTag.Add, 0: Expr, 1: Expr }
  | { tag: ExprTag.Subtract, 0: Expr, 1: Expr  }
  | { tag: ExprTag.Multiply, 0: Expr, 1: Expr  }
  | { tag: ExprTag.Divide, 0: Expr, 1: Expr  }
  | { tag: ExprTag.Var, 0: string }
  | { tag: ExprTag.Apply, 0: Expr, 1: Expr[] }
  | { tag: ExprTag.Call, 0: Expr, 1: string, 2: Expr[] }
  | { tag: ExprTag.Ref, 0: number }
;

export const expr = recursive<Expr>(self => discriminate({
  [ExprTag.Num]: record({ 0: int64 }),
  [ExprTag.Str]: record({ 0: string }),
  [ExprTag.Dot]: record({ 0: self, 1: string }),
  [ExprTag.Add]: record({ 0: self, 1: self }),
  [ExprTag.Subtract]: record({ 0: self, 1: self }),
  [ExprTag.Multiply]: record({ 0: self, 1: self }),
  [ExprTag.Divide]: record({ 0: self, 1: self }),
  [ExprTag.Var]: record({ 0: string }),
  [ExprTag.Apply]: record({ 0: self, 1: array(self) }),
  [ExprTag.Call]: record({ 0: self, 1: string, 2: array(self) }),
  [ExprTag.Ref]: record({ 0: int64 }),
}));

export enum UpCommandTag {
  Assign,
  Eval,
}

export const upCmd = discriminate({
  [UpCommandTag.Assign]: record({ expr: expr, result: int64 }),
  [UpCommandTag.Eval]: record({ expr: expr }),
});

export enum DownCmdTag {
  Start,
  Completed,
}

export const downCmd = discriminate({
  [DownCmdTag.Start]: record({}),
  [DownCmdTag.Completed]: record({}),
});

export type UpCmd = typeof upCmd['_A'];
export type DownCmd = typeof downCmd['_A'];

export function haskellApp(inst: HaskellIstance, down: DownCmd = { tag: DownCmdTag.Start }) {
  const upCmd = interactWithHaskell(inst, down);
  console.log(upCmd);
  switch (upCmd.tag) {
    case UpCommandTag.Assign: {
      console.log('result', evalExpr(upCmd.expr));
      return;
    }
    case UpCommandTag.Eval: {
      console.log('result', evalExpr(upCmd.expr));
      return;
    }
  }
  return absurd(upCmd);
}

function interactWithHaskell(inst: HaskellIstance, down: DownCmd): UpCmd {
  const downBuf = downCmd.encode(down);
  const downPtr = storeBuffer(inst, downBuf);
  const upBuf = loadBuffer(inst, inst.exports.app(downPtr));
  console.log('received', upBuf);
  return upCmd.decode(upBuf);
}

// const t01: Expr = {
//   tag: ExprTag.Call,
//   0: { tag: ExprTag.Dot, 0: { tag: ExprTag.Var, 0: 'console'}, 1: 'log' },
//   1: [{ tag: ExprTag.Str, 0: 'Fuck, this is really working!'}],
// };

// const t02: UpCmd = {
//   tag: UpCommandTag.Assign,
//   expr: t01,
//   result: 0,
// };

// console.log(upCmd.decode(upCmd.encode(t02)));
// console.log(downCmd.encode({ tag: DownCmdTag.Start }));
// console.log(downCmd.decode(downCmd.encode({ tag: DownCmdTag.Start })));
const t_01: UpCmd = { tag: UpCommandTag.Eval, expr: { tag: ExprTag.Apply, 0: { tag: ExprTag.Var, 0: "log" }, 1: [{ tag: ExprTag.Str, 0: "Fuck, this is really working!"}]} }
const t_01_0 = upCmd.encode(t_01);
console.log(t_01_0);
console.log(upCmd.decode(t_01_0));
