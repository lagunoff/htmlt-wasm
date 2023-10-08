import * as b from './binary';
import * as reactor from './reactor';
import { HaskellIstance } from './reactor';
import { absurd } from './lib';

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

export type HaskellCallback = (down: DownCmd) => void;

export function evalExpr(ctx: List<Bindings>, hscb: HaskellCallback, exp: Expr): unknown {
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
      return exp[0].map(evalExpr.bind(undefined, ctx, hscb));
    }
    case ExprTag.Obj: {
      return Object.fromEntries(exp[0].map(([k, e]) => [k, evalExpr(ctx, hscb, e)]));
    }
    case ExprTag.Dot: {
      const lhs = evalExpr(ctx, hscb, exp[0]) as any;
      return lhs[exp[1]];
    }
    case ExprTag.AssignProp: {
      const rhs = evalExpr(ctx, hscb, exp[2]);
      const obj = evalExpr(ctx, hscb, exp[0]) as any;
      obj[exp[1]] = rhs;
      return rhs;
    }
    case ExprTag.Ix: {
      const rhs: any = evalExpr(ctx, hscb, exp.exp);
      return rhs[exp.ix];
    }
    case ExprTag.Add: {
      const lhs = evalExpr(ctx, hscb, exp[0]) as number;
      const rhs = evalExpr(ctx, hscb, exp[1]) as number;
      return lhs + rhs;
    }
    case ExprTag.Subtract: {
      const lhs = evalExpr(ctx, hscb, exp[0]) as number;
      const rhs = evalExpr(ctx, hscb, exp[1]) as number;
      return lhs - rhs;
    }
    case ExprTag.Multiply: {
      const lhs = evalExpr(ctx, hscb, exp[0]) as number;
      const rhs = evalExpr(ctx, hscb, exp[1]) as number;
      return lhs * rhs;
    }
    case ExprTag.Divide: {
      const lhs = evalExpr(ctx, hscb, exp[0]) as number;
      const rhs = evalExpr(ctx, hscb, exp[1]) as number;
      return lhs / rhs;
    }
    case ExprTag.Id: {
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
        return evalExpr(Cons(bindings, ctx), hscb, exp.body);
      };
    }
    case ExprTag.Apply: {
      const lhs = evalExpr(ctx, hscb, exp[0]) as Function;
      return lhs.apply(undefined, exp[1].map(evalExpr.bind(undefined, ctx, hscb)));
    }
    case ExprTag.Call: {
      const lhs = evalExpr(ctx, hscb, exp[0]) as any;
      const fn = lhs[exp[1]];
      return fn.apply(lhs, exp[2].map(evalExpr.bind(undefined, ctx, hscb)));
    }
    case ExprTag.AssignVar: {
      const rhs = evalExpr(ctx, hscb, exp.rhs);
      varStorage.set(exp.lhs, rhs);
      return rhs;
    }
    case ExprTag.FreeVar: {
      return varStorage.delete(exp.varId);
    }
    case ExprTag.Var: {
      return varStorage.get(exp.varId);
    }
    case ExprTag.ElInitBuilder: {
      const element: HTMLElement = evalExpr(ctx, hscb, exp.element) as any;
      const newBuilder = new ElementBuilder(null, element);
      varStorage.set(exp.varId, newBuilder);
      return newBuilder;
    }
    case ExprTag.ElDestroyBuilder: {
      const builder = varStorage.get(exp.varId) as DomBuilder;
      if (builder instanceof BoundaryBuilder) {
        clearBoundary(builder);
        builder._begin.parentElement!.removeChild(builder._begin);
        builder._end.parentElement!.removeChild(builder._end);
      }
      varStorage.delete(exp.varId)
      return null;
    }
    case ExprTag.ElPush: {
      const builder = varStorage.get(exp.varId) as DomBuilder;
      const newBuilder = insertElement(builder, exp.tagName)
      varStorage.set(exp.varId, newBuilder);
      return newBuilder;
    }
    case ExprTag.ElNoPush: {
      const builder = varStorage.get(exp.varId) as DomBuilder;
      const newElm = document.createElement(exp.tagName);
      insertIntoBuilder(builder, newElm);
      return null;
    }
    case ExprTag.ElProp: {
      const builder = varStorage.get(exp.varId) as DomBuilder;
      const propVal = evalExpr(ctx, hscb, exp.val);
      applyProperty(builder, exp.prop, propVal);
      return null;
    }
    case ExprTag.ElAttr: {
      const builder = varStorage.get(exp.varId) as DomBuilder;
      applyAttribute(builder, exp.attr, exp.val);
      return null;
    }
    case ExprTag.ElEvent: {
      const builder = varStorage.get(exp.varId) as DomBuilder;
      const callback = evalExpr(ctx, hscb, exp.callback) as any;
      addEventListener(builder, exp.name, callback);
      return null;
    }
    case ExprTag.ElText: {
      const builder = varStorage.get(exp.varId) as DomBuilder;
      const textNode = document.createTextNode(exp.content);
      insertIntoBuilder(builder, textNode);
      return textNode;
    }
    case ExprTag.ElAssignTextContent: {
      const textNode = varStorage.get(exp.varId) as Text;
      textNode.nodeValue = exp.content;
      return null;
    }
    case ExprTag.ElPop: {
      const builder = varStorage.get(exp.varId) as DomBuilder;
      if (builder instanceof ElementBuilder) {
        varStorage.set(exp.varId, builder._parent);
        return null;
      } else if (builder instanceof BoundaryBuilder) {
        varStorage.set(exp.varId, builder._parent);
        return null;
      }
      return absurd(builder);
    }
    case ExprTag.ElInsertBoundary: {
      const builder = varStorage.get(exp.varId) as DomBuilder;
      const newBuilder = insertBoundary(builder);
      varStorage.set(exp.varId, newBuilder);
      return newBuilder;
    }
    case ExprTag.ElClearBoundary: {
      const builder = varStorage.get(exp.varId) as DomBuilder;
      if (builder instanceof BoundaryBuilder) {
        clearBoundary(builder);
        return null;
      }
      return null;
    }
    case ExprTag.ElToggleClass: {
      const builder = varStorage.get(exp.varId) as DomBuilder;
      toggleClass(builder, exp.className, exp.enable != 0);
      return null;
    }
    case ExprTag.RevSeq: {
      return exp.exprs.reduceRight<unknown>((_, e) => evalExpr(ctx, hscb, e), null);
    }
    case ExprTag.ExecCallback: {
      const arg = evalExpr(ctx, hscb, exp.arg);
      return hscb({
        tag: DownCmdTag.ExecCallback,
        arg: unknownToJValue(arg),
        callbackId: exp.callbackId
      });
    }
    case ExprTag.UncaughtException: {
      throw new Error(exp.message);
    }
  }
  absurd(exp);
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
  AssignProp,
  Ix,

  Add,
  Subtract,
  Multiply,
  Divide,

  Id,
  Lam,
  Apply,
  Call,

  AssignVar,
  FreeVar,
  Var,

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

  RevSeq,
  ExecCallback,
  UncaughtException,
}

export type Expr =
  | { tag: ExprTag.Null }
  | { tag: ExprTag.Boolean, 0: number }
  | { tag: ExprTag.Num, 0: number }
  | { tag: ExprTag.Str, 0: string }
  | { tag: ExprTag.Arr, 0: Expr[] }
  | { tag: ExprTag.Obj, 0: [string, Expr][] }

  | { tag: ExprTag.Dot, 0: Expr, 1: string }
  | { tag: ExprTag.AssignProp, 0: Expr, 1: string, 2: Expr }
  | { tag: ExprTag.Ix, exp: Expr, ix: number }

  | { tag: ExprTag.Add, 0: Expr, 1: Expr }
  | { tag: ExprTag.Subtract, 0: Expr, 1: Expr  }
  | { tag: ExprTag.Multiply, 0: Expr, 1: Expr  }
  | { tag: ExprTag.Divide, 0: Expr, 1: Expr  }

  | { tag: ExprTag.Id, 0: string }
  | { tag: ExprTag.Lam, args: string[], body: Expr }
  | { tag: ExprTag.Apply, 0: Expr, 1: Expr[] }
  | { tag: ExprTag.Call, 0: Expr, 1: JSFunctionName, 2: Expr[] }

  | { tag: ExprTag.AssignVar, lhs: DomBuilderId, rhs: Expr }
  | { tag: ExprTag.FreeVar, varId: number }
  | { tag: ExprTag.Var, varId: number }

  | { tag: ExprTag.ElInitBuilder, varId: DomBuilderId, element: Expr }
  | { tag: ExprTag.ElDestroyBuilder, varId: DomBuilderId }
  | { tag: ExprTag.ElPush, varId: DomBuilderId, tagName: string }
  | { tag: ExprTag.ElNoPush, varId: DomBuilderId, tagName: string }
  | { tag: ExprTag.ElProp, varId: DomBuilderId, prop: string, val: Expr }
  | { tag: ExprTag.ElAttr, varId: DomBuilderId, attr: string, val: string }
  | { tag: ExprTag.ElEvent, varId: DomBuilderId, name: string, callback: Expr }
  | { tag: ExprTag.ElText, varId: DomBuilderId, content: string }
  | { tag: ExprTag.ElAssignTextContent, varId: number, content: string }
  | { tag: ExprTag.ElPop, varId: DomBuilderId }
  | { tag: ExprTag.ElInsertBoundary, varId: DomBuilderId }
  | { tag: ExprTag.ElClearBoundary, varId: DomBuilderId }
  | { tag: ExprTag.ElToggleClass, varId: DomBuilderId, className: string, enable: number }

  | { tag: ExprTag.RevSeq, exprs: Expr[] }
  | { tag: ExprTag.ExecCallback, callbackId: number, arg: Expr }
  | { tag: ExprTag.UncaughtException, message: string }
;

export const expr = b.recursive<Expr>(self => b.discriminate({
  [ExprTag.Null]: b.record({}),
  [ExprTag.Boolean]: b.record({ 0: b.int8 }),
  [ExprTag.Num]: b.record({ 0: b.int64 }),
  [ExprTag.Str]: b.record({ 0: b.string }),
  [ExprTag.Arr]: b.record({ 0: b.array(self) }),
  [ExprTag.Obj]: b.record({ 0: b.array(b.tuple(b.string, self)) }),

  [ExprTag.Dot]: b.record({ 0: self, 1: b.string }),
  [ExprTag.AssignProp]: b.record({ 0: self, 1: b.string, 2: self }),
  [ExprTag.Ix]: b.record({ exp: self, ix: b.int64 }),

  [ExprTag.Add]: b.record({ 0: self, 1: self }),
  [ExprTag.Subtract]: b.record({ 0: self, 1: self }),
  [ExprTag.Multiply]: b.record({ 0: self, 1: self }),
  [ExprTag.Divide]: b.record({ 0: self, 1: self }),

  [ExprTag.Id]: b.record({ 0: b.string }),
  [ExprTag.Lam]: b.record({ args: b.array(b.string), body: self }),
  [ExprTag.Apply]: b.record({ 0: self, 1: b.array(self) }),
  [ExprTag.Call]: b.record({ 0: self, 1: b.string, 2: b.array(self) }),

  [ExprTag.AssignVar]: b.record({ lhs: b.int64, rhs: self }),
  [ExprTag.FreeVar]: b.record({ varId: b.int64 }),
  [ExprTag.Var]: b.record({ varId: b.int64 }),

  [ExprTag.ElInitBuilder]: b.record({ varId: b.int64, element: self }),
  [ExprTag.ElDestroyBuilder]: b.record({ varId: b.int64 }),
  [ExprTag.ElPush]: b.record({ varId: b.int64, tagName: b.string }),
  [ExprTag.ElNoPush]: b.record({ varId: b.int64, tagName: b.string }),
  [ExprTag.ElProp]: b.record({ varId: b.int64, prop: b.string, val: self }),
  [ExprTag.ElAttr]: b.record({ varId: b.int64, attr: b.string, val: b.string }),
  [ExprTag.ElEvent]: b.record({ varId: b.int64, name: b.string, callback: self }),
  [ExprTag.ElText]: b.record({ varId: b.int64, content: b.string }),
  [ExprTag.ElAssignTextContent]: b.record({ varId: b.int64, content: b.string }),
  [ExprTag.ElPop]: b.record({ varId: b.int64 }),
  [ExprTag.ElInsertBoundary]: b.record({ varId: b.int64 }),
  [ExprTag.ElClearBoundary]: b.record({ varId: b.int64 }),
  [ExprTag.ElToggleClass]: b.record({ varId: b.int64, className: b.string, enable: b.int8 }),

  [ExprTag.RevSeq]: b.record({ exprs: b.array(self) }),
  [ExprTag.ExecCallback]: b.record({ callbackId: b.int64, arg: self }),
  [ExprTag.UncaughtException]: b.record({ message: b.string }),
}));

export enum UpCommandTag {
  Eval,
  HotReload,
  Exit,
}

export const upCmd = b.discriminate({
  [UpCommandTag.Eval]: b.record({ expr: expr }),
  [UpCommandTag.HotReload]: b.record({ }),
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

export const varStorage = new Map<number, unknown>();

export type DomBuilderId = number;

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
