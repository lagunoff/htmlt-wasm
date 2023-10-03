import * as b from './binary';
import * as reactor from './reactor';
import { HaskellIstance } from './reactor';
import { absurd } from './lib';

export  type HaskellPointer = number;

export type JSFunctionName = string;

export type Ident = string;

export type Bindings = Record<Ident, unknown>;

export type List<T> = null | Cons<T>;

export type Cons<T> = { 0: T, 1: List<T> };

export function Cons<T>(x: T, xs: List<T>): List<T> {
  return [x, xs];
}

export function car<T>(pair: Cons<T>): T {
  return pair[0];
}

export function cdr<T>(pair: Cons<T>): List<T> {
  return pair[1];
}


export function evalExpr(ctx: List<Bindings>, inst: HaskellIstance, exp: Expr): unknown {
  switch(exp.tag) {
    case ExprTag.Null: {
       return null;
    }
    case ExprTag.Boolean: {
       return exp[0] != 0;
    }
    case ExprTag.Num: {
       return exp[0];
    }
    case ExprTag.Str: {
      return exp[0];
    }
    case ExprTag.Arr: {
      return exp[0].map(evalExpr.bind(undefined, ctx, inst));
    }
    case ExprTag.Obj: {
      return Object.fromEntries(exp[0].map(([k, e]) => [k, evalExpr(ctx, inst, e)]));
    }
    case ExprTag.Dot: {
      const lhs = evalExpr(ctx, inst, exp[0]) as any;
      return lhs[exp[1]];
    }
    case ExprTag.Assign: {
      const rhs = evalExpr(ctx, inst, exp[2]);
      const obj = evalExpr(ctx, inst, exp[0]) as any;
      obj[exp[1]] = rhs;
      return rhs;
    }
    case ExprTag.Add: {
      const lhs = evalExpr(ctx, inst, exp[0]) as number;
      const rhs = evalExpr(ctx, inst, exp[1]) as number;
      return lhs + rhs;
    }
    case ExprTag.Subtract: {
      const lhs = evalExpr(ctx, inst, exp[0]) as number;
      const rhs = evalExpr(ctx, inst, exp[1]) as number;
      return lhs - rhs;
    }
    case ExprTag.Multiply: {
      const lhs = evalExpr(ctx, inst, exp[0]) as number;
      const rhs = evalExpr(ctx, inst, exp[1]) as number;
      return lhs * rhs;
    }
    case ExprTag.Divide: {
      const lhs = evalExpr(ctx, inst, exp[0]) as number;
      const rhs = evalExpr(ctx, inst, exp[1]) as number;
      return lhs / rhs;
    }
    case ExprTag.Var: {
      const ident = exp[0];
      for (let iter = ctx; iter; iter = cdr(iter)) {
        const bindings = car(iter);
        if (ident in bindings) {
          // Found bound value
          return bindings[ident];
        }
      }
      throw new Error('Variable not in scope: ' + exp[0]);
    }
    case ExprTag.Lam: {
      const argsNames = exp.args;
      return (...argValues: any[]) => {
        const bindings = argsNames.reduce<Bindings>((acc, name, idx) => (acc[name] = argValues[idx], acc), {});
        return evalExpr(Cons(bindings, ctx), inst, exp.body);
      };
    }
    case ExprTag.Apply: {
      const lhs = evalExpr(ctx, inst, exp[0]) as Function;
      return lhs.apply(undefined, exp[1].map(evalExpr.bind(undefined, ctx, inst)));
    }
    case ExprTag.Call: {
      const lhs = evalExpr(ctx, inst, exp[0]) as any;
      const fn = lhs[exp[1]];
      return fn.apply(lhs, exp[2].map(evalExpr.bind(undefined, ctx, inst)));
    }
    case ExprTag.Seq: {
      return exp.exprs.reduceRight<unknown>((_, e) => evalExpr(ctx, inst, e), null);
    }
    case ExprTag.ExecCallback: {
      const arg = evalExpr(ctx, inst, exp.arg);
      return reactor.haskellApp(inst, {
        tag: DownCmdTag.ExecCallback,
        arg: unknownToJValue(arg),
        callbackId: exp.callbackId
      });
    }
    case ExprTag.LAssign: {
      const rhs = evalExpr(ctx, inst, exp.rhs);
      return assignLhs(exp.lhs, rhs);
    }
    case ExprTag.FreeVar: {
      return storage.delete(exp.varId);
    }
    case ExprTag.RVar: {
      return storage.get(exp.varId);
    }
    case ExprTag.Ix: {
      const rhs: any = evalExpr(ctx, inst, exp.exp);
      return rhs[exp.ix];
    }
    case ExprTag.ElInitBuilder: {
      const element: HTMLElement = evalExpr(ctx, inst, exp.element) as any;
      const newBuilder = new ElementBuilder(null, element);
      return assignLhs(exp.builder, newBuilder);
    }
    case ExprTag.ElDestroyBuilder: {
      const builder: DomBuilder = evalLhs(exp.builder) as any;
      if (builder instanceof BoundaryBuilder) {
        clearBoundary(builder);
        builder._begin.parentElement!.removeChild(builder._begin);
        builder._end.parentElement!.removeChild(builder._end);
      }
      // storage.delete(exp.builder.varId)
      return null;
    }
    case ExprTag.ElPush: {
      const builder: DomBuilder = evalLhs(exp.builder) as any;
      const newBuilder = insertElement(builder, exp.tagName)
      return assignLhs(exp.builder, newBuilder);
    }
    case ExprTag.ElNoPush: {
      const builder: DomBuilder = evalLhs(exp.builder) as any;
      const newElm = document.createElement(exp.tagName);
      insertIntoBuilder(builder, newElm);
      return null;
    }
    case ExprTag.ElProp: {
      const builder: DomBuilder = evalLhs(exp.builder) as any;
      const propVal = evalExpr(ctx, inst, exp.val);
      applyProperty(builder, exp.prop, propVal);
      return null;
    }
    case ExprTag.ElAttr: {
      const builder: DomBuilder = evalLhs(exp.builder) as any;
      applyAttribute(builder, exp.attr, exp.val);
      return null;
    }
    case ExprTag.ElEvent: {
      const builder: DomBuilder = evalLhs(exp.builder) as any;
      const callback = evalExpr(ctx, inst, exp.callback) as any;
      addEventListener(builder, exp.name, callback);
      return null;
    }
    case ExprTag.ElText: {
      const builder: DomBuilder = evalLhs(exp.builder) as any;
      const textNode = document.createTextNode(exp.content);
      insertIntoBuilder(builder, textNode);
      return textNode;
    }
    case ExprTag.ElAssignTextContent: {
      const textNode = storage.get(exp.varId) as Text;
      textNode.nodeValue = exp.content;
      return null;
    }
    case ExprTag.ElPop: {
      const builder: DomBuilder = evalLhs(exp.builder) as any;
      if (builder instanceof ElementBuilder) {
        return assignLhs(exp.builder, builder._parent);
      } else if (builder instanceof BoundaryBuilder) {
        return assignLhs(exp.builder, builder._parent);
      }
      return absurd(builder);
    }
    case ExprTag.ElInsertBoundary: {
      const builder: DomBuilder = evalLhs(exp.builder) as any;
      const newBuilder = insertBoundary(builder);
      return assignLhs(exp.builder, newBuilder);
    }
    case ExprTag.ElClearBoundary: {
      const builder: DomBuilder = evalLhs(exp.builder) as any;
      if (builder instanceof BoundaryBuilder) {
        clearBoundary(builder);
        return null;
      }
      return null;
    }
    case ExprTag.ElToggleClass: {
      const builder: DomBuilder = evalLhs(exp.builder) as any;
      // TODO: Remove the defensive test and inverstigate the bug.
      // The 'builder' supposed to be a valid reference, but when a
      // 'simpleList' element gets deleted, this gets called with
      // previously freed 'builder'!
      toggleClass(builder, exp.className, exp.enable != 0);
      return null;
    }
    case ExprTag.UncaughtException: {
      throw new Error(exp.message);
    }
    case ExprTag.ReadLhs: {
      return evalLhs(exp.lhs);
    }
  }
  absurd(exp);
}

export function evalLhs(exp: LhsExpr): unknown {
  switch (exp.tag) {
    case LhsExprTag.LVar: {
      return storage.get(exp.varId);
    }
    case LhsExprTag.LIx: {
      const lhs = evalLhs(exp.lhs) as any;
      return lhs[exp.ix];
    }
    case LhsExprTag.LProp: {
      const lhs = evalLhs(exp.lhs) as any;
      return lhs[exp.prop];
    }
  }
  return absurd(exp);
}

export function assignLhs(exp: LhsExpr, value: unknown) {
  switch (exp.tag) {
    case LhsExprTag.LVar: {
      return storage.set(exp.varId, value);
    }
    case LhsExprTag.LIx: {
      switch (exp.lhs.tag) {
        case LhsExprTag.LVar: {
          const lhs = storage.get(exp.lhs.varId) as any;
          lhs[exp.ix] = value;
          return;
        }
      }
      throw new Error("Unimplemented");
    }
    case LhsExprTag.LProp: {
      throw new Error("Unimplemented");
    }
  }
}

export function unknownToJValue(inp: unknown): JValue {
  if (typeof(inp) === 'boolean') {
    return { tag: JValueTag.JBool, 0: inp ? 1 : 0 };
  }
  if (typeof(inp) === 'number') {
    return { tag: JValueTag.JNum, 0: inp };
  }
  if (typeof(inp) === 'string') {
    return { tag: JValueTag.JStr, 0: inp };
  }
  if (Array.isArray(inp)) {
    return { tag: JValueTag.JArr, 0: inp.map(unknownToJValue) };
  }
  if (inp === null || inp === undefined) {
    return { tag: JValueTag.JNull };
  }
  const entries = Object.entries(inp)
    .map(([k, v]) => [k, unknownToJValue(v)] as KV);

  return { tag: JValueTag.JObj, 0: entries }

  type KV = [string, JValue];
}

export enum LhsExprTag {
  LVar,
  LIx,
  LProp,
}

export type LhsExpr =
  | { tag: LhsExprTag.LVar, varId: number }
  | { tag: LhsExprTag.LIx, lhs: LhsExpr, ix: number }
  | { tag: LhsExprTag.LProp, lhs: LhsExpr, prop: string }
;

export const lhsExpr = b.recursive<LhsExpr>(self => b.discriminate({
  [LhsExprTag.LVar]: b.record({ varId: b.int64 }),
  [LhsExprTag.LIx]: b.record({ lhs: self, ix: b.int64 }),
  [LhsExprTag.LProp]: b.record({ lhs: self, prop: b.string }),
}));

export enum JValueTag {
  JNull,
  JBool,
  JNum,
  JStr,
  JArr,
  JObj,
}

export type JValue =
  | { tag: JValueTag.JNull }
  | { tag: JValueTag.JBool, 0: number }
  | { tag: JValueTag.JNum, 0: number }
  | { tag: JValueTag.JStr, 0: string }
  | { tag: JValueTag.JArr, 0: JValue[] }
  | { tag: JValueTag.JObj, 0: [string, JValue][] }
;

export const jvalue = b.recursive<JValue>(self => b.discriminate({
  [JValueTag.JNull]: b.record({ }),
  [JValueTag.JBool]: b.record({ 0: b.int8 }),
  [JValueTag.JNum]: b.record({ 0: b.int64 }),
  [JValueTag.JStr]: b.record({ 0: b.string }),
  [JValueTag.JArr]: b.record({ 0: b.array(self) }),
  [JValueTag.JObj]: b.record({ 0: b.array(b.tuple(b.string, self)) }),
}));


export enum ExprTag {
  Null,
  Boolean,
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
  Lam,
  Apply,
  Call,
  Seq,

  ExecCallback,
  LAssign,
  FreeVar,
  RVar,
  Ix,

  ElInitBuilder,
  ElDestroyBuilder,
  ElPush,
  ElNoPush,
  ElProp,
  ElAttr,
  ElEvent,
  ElText,
  ElAssignTextContent,
  ElPop,
  ElInsertBoundary,
  ElClearBoundary,
  ElToggleClass,

  UncaughtException,

  ReadLhs,
}

export type Expr =
  | { tag: ExprTag.Null }
  | { tag: ExprTag.Boolean, 0: number }
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
  | { tag: ExprTag.Lam, args: string[], body: Expr }
  | { tag: ExprTag.Apply, 0: Expr, 1: Expr[] }
  | { tag: ExprTag.Call, 0: Expr, 1: JSFunctionName, 2: Expr[] }
  | { tag: ExprTag.Seq, exprs: Expr[] }

  | { tag: ExprTag.ExecCallback, callbackId: number, arg: Expr }
  | { tag: ExprTag.LAssign, lhs: LhsExpr, rhs: Expr }
  | { tag: ExprTag.FreeVar, varId: number }
  | { tag: ExprTag.RVar, varId: number }
  | { tag: ExprTag.Ix, exp: Expr, ix: number }

  | { tag: ExprTag.ElInitBuilder, builder: LhsExpr, element: Expr }
  | { tag: ExprTag.ElDestroyBuilder, builder: LhsExpr }
  | { tag: ExprTag.ElPush, builder: LhsExpr, tagName: string }
  | { tag: ExprTag.ElNoPush, builder: LhsExpr, tagName: string }
  | { tag: ExprTag.ElProp, builder: LhsExpr, prop: string, val: Expr }
  | { tag: ExprTag.ElAttr, builder: LhsExpr, attr: string, val: string }
  | { tag: ExprTag.ElEvent, builder: LhsExpr, name: string, callback: Expr }
  | { tag: ExprTag.ElText, builder: LhsExpr, content: string }
  | { tag: ExprTag.ElAssignTextContent, varId: number, content: string }
  | { tag: ExprTag.ElPop, builder: LhsExpr }
  | { tag: ExprTag.ElInsertBoundary, builder: LhsExpr }
  | { tag: ExprTag.ElClearBoundary, builder: LhsExpr }
  | { tag: ExprTag.ElToggleClass, builder: LhsExpr, className: string, enable: number }

  | { tag: ExprTag.UncaughtException, message: string }

  | { tag: ExprTag.ReadLhs, lhs: LhsExpr }
;

export const expr = b.recursive<Expr>(self => b.discriminate({
  [ExprTag.Null]: b.record({}),
  [ExprTag.Boolean]: b.record({ 0: b.int8 }),
  [ExprTag.Num]: b.record({ 0: b.int64 }),
  [ExprTag.Str]: b.record({ 0: b.string }),
  [ExprTag.Arr]: b.record({ 0: b.array(self) }),
  [ExprTag.Obj]: b.record({ 0: b.array(b.tuple(b.string, self)) }),
  [ExprTag.Dot]: b.record({ 0: self, 1: b.string }),
  [ExprTag.Assign]: b.record({ 0: self, 1: b.string, 2: self }),
  [ExprTag.Add]: b.record({ 0: self, 1: self }),
  [ExprTag.Subtract]: b.record({ 0: self, 1: self }),
  [ExprTag.Multiply]: b.record({ 0: self, 1: self }),
  [ExprTag.Divide]: b.record({ 0: self, 1: self }),
  [ExprTag.Var]: b.record({ 0: b.string }),
  [ExprTag.Lam]: b.record({ args: b.array(b.string), body: self }),
  [ExprTag.Apply]: b.record({ 0: self, 1: b.array(self) }),
  [ExprTag.Call]: b.record({ 0: self, 1: b.string, 2: b.array(self) }),
  [ExprTag.Seq]: b.record({ exprs: b.array(self) }),

  [ExprTag.ExecCallback]: b.record({ callbackId: b.int64, arg: self }),
  [ExprTag.LAssign]: b.record({ lhs: lhsExpr, rhs: self }),
  [ExprTag.FreeVar]: b.record({ varId: b.int64 }),
  [ExprTag.RVar]: b.record({ varId: b.int64 }),
  [ExprTag.Ix]: b.record({ exp: self, ix: b.int64 }),

  [ExprTag.ElInitBuilder]: b.record({ builder: lhsExpr, element: self }),
  [ExprTag.ElDestroyBuilder]: b.record({ builder: lhsExpr }),
  [ExprTag.ElPush]: b.record({ builder: lhsExpr, tagName: b.string }),
  [ExprTag.ElNoPush]: b.record({ builder: lhsExpr, tagName: b.string }),
  [ExprTag.ElProp]: b.record({ builder: lhsExpr, prop: b.string, val: self }),
  [ExprTag.ElAttr]: b.record({ builder: lhsExpr, attr: b.string, val: b.string }),
  [ExprTag.ElEvent]: b.record({ builder: lhsExpr, name: b.string, callback: self }),
  [ExprTag.ElText]: b.record({ builder: lhsExpr, content: b.string }),
  [ExprTag.ElAssignTextContent]: b.record({ varId: b.int64, content: b.string }),
  [ExprTag.ElPop]: b.record({ builder: lhsExpr }),
  [ExprTag.ElInsertBoundary]: b.record({ builder: lhsExpr }),
  [ExprTag.ElClearBoundary]: b.record({ builder: lhsExpr }),
  [ExprTag.ElToggleClass]: b.record({ builder: lhsExpr, className: b.string, enable: b.int8 }),

  [ExprTag.UncaughtException]: b.record({ message: b.string }),

  [ExprTag.ReadLhs]: b.record({ lhs: lhsExpr }),
}));

export enum UpCommandTag {
  Eval,
  Exit,
}

export const upCmd = b.discriminate({
  [UpCommandTag.Eval]: b.record({ expr: expr }),
  [UpCommandTag.Exit]: b.record({ }),
});

export enum DownCmdTag {
  Start,
  Return,
  ExecCallback,
}

export const downCmd = b.discriminate({
  [DownCmdTag.Start]: b.record({}),
  [DownCmdTag.Return]: b.record({ 0: jvalue }),
  [DownCmdTag.ExecCallback]: b.record({ arg: jvalue, callbackId: b.int64 }),
});

export type UpCmd = typeof upCmd['_A'];
export type DownCmd = typeof downCmd['_A'];

export const storage = new Map<number, unknown>();

export type DomBuilder = ElementBuilder | BoundaryBuilder;

export class ElementBuilder {
  constructor(
    readonly _parent: DomBuilder|null,
    readonly _element: HTMLElement,
  ) {}
}

export class BoundaryBuilder {
  constructor(
    readonly _parent: DomBuilder,
    readonly _begin: Comment,
    readonly _end: Comment,
  ) {}
}

export function insertElement(builder: DomBuilder, tagName: string): ElementBuilder {
  const newElm = document.createElement(tagName);
  insertIntoBuilder(builder, newElm);
  return new ElementBuilder(builder, newElm);
}

export function insertBoundary(builder: DomBuilder): BoundaryBuilder {
  const begin = document.createComment('ContentBoundary {{');
  const end = document.createComment('}}');
  insertIntoBuilder(builder, begin);
  insertIntoBuilder(builder, end);
  return new BoundaryBuilder(builder, begin, end);
}

export function clearBoundary(boundary: BoundaryBuilder) {
  const [begin, end] = [boundary._begin, boundary._end];
  for (;;){
    if (!end.previousSibling
      || !end.previousSibling.parentNode
      || end.previousSibling === begin
    ) break;
    end.previousSibling.parentNode.removeChild(end.previousSibling);
  }
}

export function insertIntoBuilder(builder: DomBuilder, child: Node) {
  if (builder instanceof ElementBuilder) {
    builder._element.appendChild(child);
    return;
  } else if (builder instanceof BoundaryBuilder) {
    const parent = builder._end.parentElement!;
    parent.insertBefore(child, builder._end);
    return;
  }
  return absurd(builder);
}

export function applyProperty(builder: DomBuilder, propName: string, propVal: unknown) {
  if (builder instanceof ElementBuilder) {
    (builder._element as any)[propName] = propVal;
    return;
  } else if (builder instanceof BoundaryBuilder) {
    (builder._end.parentElement as any)[propName] = propVal;
    return;
  }
  return absurd(builder);
}

export function applyAttribute(builder: DomBuilder, attr: string, value: string) {
  if (builder instanceof ElementBuilder) {
    builder._element.setAttribute(attr, value)
    return;
  } else if (builder instanceof BoundaryBuilder) {
    builder._end.parentElement!.setAttribute(attr, value);
    return;
  }
  return absurd(builder);
}

export function toggleClass(builder: DomBuilder, className: string, enabled: boolean) {
  if (builder instanceof ElementBuilder) {
    if (enabled) {
      builder._element.classList.add(className);
    } else {
      builder._element.classList.remove(className);
    }
    return;
  } else if (builder instanceof BoundaryBuilder) {
    if (enabled) {
      builder._end.parentElement!.classList.add(className);
    } else {
      builder._end.parentElement!.classList.remove(className);
    }
    return;
  }
  return absurd(builder);
}
export function addEventListener(builder: DomBuilder, eventName: string, listener: EventListener) {
  if (builder instanceof ElementBuilder) {
    builder._element.addEventListener(eventName, listener);
    return;
  } else if (builder instanceof BoundaryBuilder) {
    builder._end.parentElement!.addEventListener(eventName, listener);
    return;
  }
  return absurd(builder);
}
