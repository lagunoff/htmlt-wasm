import * as html from './html';

export  type HaskellPointer = number;

export type JSValueRef = [number, number];

export type Scope = unknown[];

export type Context = Map<number, Scope>;

export type JSFunctionName = string;

export type HaskellExports = {
  hs_malloc: (size: number) => HaskellPointer;
  hs_free: (ptr: HaskellPointer) => void;
  app: (input: HaskellPointer) => HaskellPointer;
  memory: WebAssembly.Memory;
};

export type HaskellIstance = {
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
  | TupleDecoder<A>
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
export class TupleDecoder<A> extends DecoderBase<A> {
  constructor(
    readonly _tuple: Decoder<any>[],
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
  if (decoder instanceof TupleDecoder) {
    const tupleVal = value as any as any[];
    return decoder._tuple.reduce((acc, v, i) => acc + computeSize(v, tupleVal[i]), 0);
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
  if (decoder instanceof TupleDecoder) {
    let jx = ptr;
    const resultTup = decoder._tuple.map(dec => {
      const [val, newIx] = runDecoder(dec, mem, jx);
      jx = newIx;
      return val;
    });
    return [resultTup as any as A, jx];
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
  if (decoder instanceof TupleDecoder) {
    const tupleVal = value as any[];
    let jx = ptr;
    decoder._tuple.forEach((dec, i) => {
      jx = runEncoder(dec, mem, jx, tupleVal[i]);
    });
    return jx;
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
    case ExprTag.Arr: {
      return exp[0].map(evalExpr);
    }
    case ExprTag.Obj: {
      return Object.fromEntries(exp[0].map(([k, e]) => [k, evalExpr(e)]));
    }
    case ExprTag.Dot: {
      const lhs = evalExpr(exp[0]) as any;
      return lhs[exp[1]];
    }
    case ExprTag.Assign: {
      const rhs = evalExpr(exp[2]);
      const obj = evalExpr(exp[0]) as any;
      obj[exp[1]] = rhs;
      return rhs;
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
      return context.get(exp[0])![exp[1]];
    }
    case ExprTag.Let: {
      context.get(exp.scopeId)![exp.varId] = evalExpr(exp.rhs);
      return evalExpr(exp.body);
    }
    case ExprTag.Seq: {
      evalExpr(exp.first);
      return evalExpr(exp.second);
    }
    case ExprTag.El: {
      const builderStack = context.get(exp.builderId)! as Element[];
      const attrs = Object.fromEntries(exp.attrs.map(([k, e]) => [k, evalExpr(e)]));
      const stackTip = builderStack[builderStack.length - 1];
      const newElm = html.el(exp.tagName, attrs, {});
      builderStack.push(newElm);
      if (stackTip) stackTip.appendChild(newElm);
      return newElm;
    }
    case ExprTag.Text: {
      const builderStack = context.get(exp.builderId)! as Element[];
      const stackTip = builderStack[builderStack.length - 1];
      const newTextNode = new Text(exp.contents);
      if (stackTip) stackTip.appendChild(newTextNode);
      return newTextNode;
    }
    case ExprTag.PopDomBuilder: {
      const builderStack = context.get(exp.builderId)! as Element[];
      const stackTip = builderStack.pop();
      if (builderStack.length == 0) {
        document.body.appendChild(stackTip as HTMLElement);
      }
      return stackTip;
    }
  }
  absurd(exp);
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

export function tuple<Args extends Decoder<unknown>[]>(...args: Args): TupleDecoder<{[k in keyof Args]: Args[k]['_A'] }>  {
  return new TupleDecoder(args);
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
  Arr,
  Obj,
  Dot,
  Assign,
  Add,
  Subtract,
  Multiply,
  Divide,
  Var,
  Apply,
  Call,
  Ref,
  Let,
  Seq,
  El,
  Text,
  PopDomBuilder,
}

export type Expr =
  | { tag: ExprTag.Num, 0: number }
  | { tag: ExprTag.Str, 0: string }
  | { tag: ExprTag.Arr, 0: Expr[] }
  | { tag: ExprTag.Obj, 0: [string, Expr][] }
  | { tag: ExprTag.Dot, 0: Expr, 1: string }
  | { tag: ExprTag.Assign, 0: Expr, 1: string, 2: Expr }
  | { tag: ExprTag.Add, 0: Expr, 1: Expr }
  | { tag: ExprTag.Subtract, 0: Expr, 1: Expr  }
  | { tag: ExprTag.Multiply, 0: Expr, 1: Expr  }
  | { tag: ExprTag.Divide, 0: Expr, 1: Expr  }
  | { tag: ExprTag.Var, 0: string }
  | { tag: ExprTag.Apply, 0: Expr, 1: Expr[] }
  | { tag: ExprTag.Call, 0: Expr, 1: JSFunctionName, 2: Expr[] }
  | { tag: ExprTag.Ref, 0: number, 1: number }
  | { tag: ExprTag.Let, scopeId: number, varId: number, rhs: Expr, body: Expr }
  | { tag: ExprTag.Seq, first: Expr, second: Expr }
  | { tag: ExprTag.El, builderId: number, tagName: string, attrs: [string, Expr][] }
  | { tag: ExprTag.Text, builderId: number, contents: string }
  | { tag: ExprTag.PopDomBuilder, builderId: number }
;

export const expr = recursive<Expr>(self => discriminate({
  [ExprTag.Num]: record({ 0: int64 }),
  [ExprTag.Str]: record({ 0: string }),
  [ExprTag.Arr]: record({ 0: array(self) }),
  [ExprTag.Obj]: record({ 0: array(tuple(string, self)) }),
  [ExprTag.Dot]: record({ 0: self, 1: string }),
  [ExprTag.Assign]: record({ 0: self, 1: string, 2: self }),
  [ExprTag.Add]: record({ 0: self, 1: self }),
  [ExprTag.Subtract]: record({ 0: self, 1: self }),
  [ExprTag.Multiply]: record({ 0: self, 1: self }),
  [ExprTag.Divide]: record({ 0: self, 1: self }),
  [ExprTag.Var]: record({ 0: string }),
  [ExprTag.Apply]: record({ 0: self, 1: array(self) }),
  [ExprTag.Call]: record({ 0: self, 1: string, 2: array(self) }),
  [ExprTag.Ref]: record({ 0: int64, 1: int64 }),
  [ExprTag.Let]: record({ scopeId: int64, varId: int64, rhs: self, body: self }),
  [ExprTag.Seq]: record({ first: self, second: self }),
  [ExprTag.El]: record({ builderId: int64, tagName: string, attrs: array(tuple(string, self)) }),
  [ExprTag.Text]: record({ builderId: int64, contents: string }),
  [ExprTag.PopDomBuilder]: record({ builderId: int64 }),
}));

export enum UpCommandTag {
  Eval,
  NewScope,
  FreeScope,
  NewDomBuilder,
  FinalizeDomBuilder,
  UncaughtException,
  Exit,
}

export const upCmd = discriminate({
  [UpCommandTag.Eval]: record({ expr: expr }),
  [UpCommandTag.NewScope]: record({}),
  [UpCommandTag.FreeScope]: record({ ref: int64 }),
  [UpCommandTag.NewDomBuilder]: record({}),
  [UpCommandTag.FinalizeDomBuilder]: record({ ref: int64 }),
  [UpCommandTag.UncaughtException]: record({ 0: string }),
  [UpCommandTag.Exit]: record({ }),
});

export enum DownCmdTag {
  Start,
  Return,
}

export const downCmd = discriminate({
  [DownCmdTag.Start]: record({}),
  [DownCmdTag.Return]: record({ 0: expr }),
});

export type UpCmd = typeof upCmd['_A'];
export type DownCmd = typeof downCmd['_A'];

export const context: Context = new Map([[0, []]]);

export function haskellApp(inst: HaskellIstance, down: DownCmd = { tag: DownCmdTag.Start }) {
  // TODO: Replace recursion with a loop
  const upCmd = interactWithHaskell(inst, down);
  // console.log(upCmd);
  switch (upCmd.tag) {
    case UpCommandTag.Eval: {
      const result = evalExpr(upCmd.expr);
      // console.log('result', result);
      return haskellApp(inst, { tag: DownCmdTag.Return, 0: { tag: ExprTag.Num, 0: 0 } });
    }
    case UpCommandTag.NewScope: {
      const scopeId = lookupMininumFreeScope(context);
      context.set(scopeId, []);
      return haskellApp(inst, { tag: DownCmdTag.Return, 0: { tag: ExprTag.Num, 0: scopeId } });
    }
    case UpCommandTag.FreeScope: {
      context.delete(upCmd.ref);
      return haskellApp(inst, { tag: DownCmdTag.Return, 0: { tag: ExprTag.Num, 0: 0 } });
    }
    case UpCommandTag.NewDomBuilder: {
      const scopeId = lookupMininumFreeScope(context);
      context.set(scopeId, []);
      return haskellApp(inst, { tag: DownCmdTag.Return, 0: { tag: ExprTag.Num, 0: scopeId } });
    }
    case UpCommandTag.FinalizeDomBuilder: {
      const builder = context.get(upCmd.ref);
      context.delete(upCmd.ref);
      if (builder) {
        const rootElm = builder[builder.length - 1] as HTMLElement;
        document.body.appendChild(rootElm);
      }
      return haskellApp(inst, { tag: DownCmdTag.Return, 0: { tag: ExprTag.Num, 0: 0 } });
    }
    case UpCommandTag.UncaughtException: {
      console.error(upCmd[0]);
      return;
    }
    case UpCommandTag.Exit: {
      return;
    }
  }
  absurd(upCmd);
}

function interactWithHaskell(inst: HaskellIstance, down: DownCmd): UpCmd {
  const downBuf = downCmd.encode(down);
  const downPtr = storeBuffer(inst, downBuf);
  const upBuf = loadBuffer(inst, inst.exports.app(downPtr));
  return upCmd.decode(upBuf);
}

function lookupMininumFreeScope(c: Context): number {
  for(let i = 0;;i++) {
    if (!c.has(i)) return i;
  }
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
// const t_01: UpCmd = {
//   tag: UpCommandTag.Eval,
//   expr: { tag: ExprTag.Obj, 0: [
//     ["0", { tag: ExprTag.Str, 0: "0"}],
//     ["1", { tag: ExprTag.Str, 0: "1"}],
//   ]}
// };
// const t_01_0 = upCmd.encode(t_01);
// console.log(t_01_0);
// console.log(upCmd.decode(t_01_0));
