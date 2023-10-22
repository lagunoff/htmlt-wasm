import * as b from './binary';
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

export type HaskellCallback = (jsmsg: JavaScriptMessage) => void;

export function evalExpr(ctx: List<Bindings>, argCtx: List<IArguments>, hscb: HaskellCallback, exp: Expr): unknown {
  switch(exp.tag) {
    case ExprTag.Null: {
       return null;
    }
    case ExprTag.Boolean: {
       return exp[0] != 0;
    }
    case ExprTag.Num: {
      return exp.coefficient * Math.pow(10, exp.base10Exponent);
    }
    case ExprTag.Str: {
      return exp[0];
    }
    case ExprTag.Arr: {
      return exp[0].map(evalExpr.bind(undefined, ctx, argCtx, hscb));
    }
    case ExprTag.Obj: {
      return Object.fromEntries(exp[0].map(([k, e]) => [k, evalExpr(ctx, argCtx, hscb, e)]));
    }
    case ExprTag.Dot: {
      const lhs = evalExpr(ctx, argCtx, hscb, exp[0]) as any;
      return lhs[exp[1]];
    }
    case ExprTag.AssignProp: {
      const rhs = evalExpr(ctx, argCtx, hscb, exp[2]);
      const obj = evalExpr(ctx, argCtx, hscb, exp[0]) as any;
      obj[exp[1]] = rhs;
      return rhs;
    }
    case ExprTag.Ix: {
      const rhs: any = evalExpr(ctx, argCtx, hscb, exp.exp);
      return rhs[exp.ix];
    }
    case ExprTag.Add: {
      const lhs = evalExpr(ctx, argCtx, hscb, exp[0]) as number;
      const rhs = evalExpr(ctx, argCtx, hscb, exp[1]) as number;
      return lhs + rhs;
    }
    case ExprTag.Subtract: {
      const lhs = evalExpr(ctx, argCtx, hscb, exp[0]) as number;
      const rhs = evalExpr(ctx, argCtx, hscb, exp[1]) as number;
      return lhs - rhs;
    }
    case ExprTag.Multiply: {
      const lhs = evalExpr(ctx, argCtx, hscb, exp[0]) as number;
      const rhs = evalExpr(ctx, argCtx, hscb, exp[1]) as number;
      return lhs * rhs;
    }
    case ExprTag.Divide: {
      const lhs = evalExpr(ctx, argCtx, hscb, exp[0]) as number;
      const rhs = evalExpr(ctx, argCtx, hscb, exp[1]) as number;
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
      return function() {
        return evalExpr(ctx, Cons(arguments, argCtx), hscb, exp.body);
      };
    }
    case ExprTag.Arg: {
      let iter = argCtx;
      let j = 0;
      while (iter) {
        if (j == exp.scopeIx) {
          const iarguments = car(iter);
          return iarguments[j];
        }
        iter = cdr(iter);
        j++;
      }
      throw new Error('Argument scope out of a rabge: ' + exp.scopeIx);
    }
    case ExprTag.Apply: {
      const lhs = evalExpr(ctx, argCtx, hscb, exp[0]) as Function;
      return lhs.apply(undefined, exp[1].map(evalExpr.bind(undefined, ctx, argCtx, hscb)));
    }
    case ExprTag.Call: {
      const lhs = evalExpr(ctx, argCtx, hscb, exp[0]) as any;
      const fn = lhs[exp[1]];
      return fn.apply(lhs, exp[2].map(evalExpr.bind(undefined, ctx, argCtx, hscb)));
    }
    case ExprTag.AssignVar: {
      const rhs = evalExpr(ctx, argCtx, hscb, exp.rhs);
      varStorage.set(exp.lhs, rhs);
      return rhs;
    }
    case ExprTag.FreeVar: {
      return varStorage.delete(exp.varId);
    }
    case ExprTag.Var: {
      return varStorage.get(exp.varId);
    }
    case ExprTag.InsertNode: {
      const parent = evalExpr(ctx, argCtx, hscb, exp.parent) as Element|Comment;
      const child = evalExpr(ctx, argCtx, hscb, exp.child) as Node;
      domBuilder.insertIntoBuilder(parent, child);
      return null;
    }
    case ExprTag.WithBuilder: {
      const builder = evalExpr(ctx, argCtx, hscb, exp.builder) as Element|Comment;
      const builderContent = evalExpr(ctx, argCtx, hscb, exp.builderContent) as Function;
      builderContent(builder);
      return builder;
    }
    case ExprTag.CreateElement: {
      return document.createElement(exp.tagName);
    }
    case ExprTag.CreateText: {
      return document.createTextNode(exp.content);
    }
    case ExprTag.ElementProp: {
      const parent = evalExpr(ctx, argCtx, hscb, exp.node) as Element|Comment;
      const propValue = evalExpr(ctx, argCtx, hscb, exp.propValue);
      domBuilder.assignProperty(parent, exp.propName, propValue);
      return null;
    }
    case ExprTag.ElementAttr: {
      const parent = evalExpr(ctx, argCtx, hscb, exp.node) as Element|Comment;
      domBuilder.assignAttribute(parent, exp.attrName, exp.attrValue);
      return null;
    }
    case ExprTag.AddEventListener: {
      const parent = evalExpr(ctx, argCtx, hscb, exp.node) as Element|Comment;
      const listener = evalExpr(ctx, argCtx, hscb, exp.listener) as EventListener;
      domBuilder.addEventListener(parent, exp.eventName, listener);
      return null;
    }
    case ExprTag.ToggleClass: {
      const parent = evalExpr(ctx, argCtx, hscb, exp.node) as Element|Comment;
      domBuilder.toggleClass(parent, exp.className, Boolean(exp.enable));
      return null;
    }
    case ExprTag.AssignText: {
      const node = evalExpr(ctx, argCtx, hscb, exp.node) as Text;
      node.textContent = exp.content;
      return null;
    }
    case ExprTag.InsertBoundary: {
      const parent = evalExpr(ctx, argCtx, hscb, exp.parent) as Element|Comment;
      return domBuilder.insertBoundary(parent);
    }
    case ExprTag.ClearBoundary: {
      const boundary = evalExpr(ctx, argCtx, hscb, exp.boundary) as Comment;
      return domBuilder.clearBoundary(boundary, Boolean(exp.detach));
    }
    case ExprTag.RevSeq: {
      return exp.exprs.reduceRight<unknown>((_, e) => evalExpr(ctx, argCtx, hscb, e), null);
    }
    case ExprTag.Eval: {
      return eval(exp.rawJavaScript);
    }
    case ExprTag.ExecCallback: {
      const arg = evalExpr(ctx, argCtx, hscb, exp.arg);
      return hscb({
        tag: JavaScriptMessageTag.ExecCallback,
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
    const {coefficient, base10Exponent} = toScientific(inp);
    return { tag: JValueTag.JNum, coefficient, base10Exponent };
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

function toScientific(n: number): { coefficient: number, base10Exponent: number } {
  // Check if the number is zero
  if (n === 0) {
    return { coefficient: 0, base10Exponent: 0 };
  }

  // Calculate the base 10 exponent
  let base10Exponent = 0;
  let coefficient = n;

  while (coefficient % 10 !== 0) {
    coefficient *= 10;
    base10Exponent--;
  }

  return { coefficient, base10Exponent };
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
  | { tag: JValueTag.JNum, coefficient: number, base10Exponent: number }
  | { tag: JValueTag.JStr, 0: string }
  | { tag: JValueTag.JArr, 0: JValue[] }
  | { tag: JValueTag.JObj, 0: [string, JValue][] }
;

export const jvalue = b.recursive<JValue>(self => b.discriminate({
  [JValueTag.JNull]: b.record({ }),
  [JValueTag.JBool]: b.record({ 0: b.int8 }),
  [JValueTag.JNum]: b.record({ coefficient: b.int64, base10Exponent: b.int8 }),
  [JValueTag.JStr]: b.record({ 0: b.string }),
  [JValueTag.JArr]: b.record({ 0: b.array(self) }),
  [JValueTag.JObj]: b.record({ 0: b.array(b.tuple(b.string, self)) }),
}));

export type StartFlags = {
  initial_url: string;
};

export const startFlags: b.Decoder<StartFlags> = b.record({
  initial_url: b.string,
});

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
  Arg,
  Apply,
  Call,

  AssignVar,
  FreeVar,
  Var,

  InsertNode,
  WithBuilder,
  CreateElement,
  CreateText,
  ElementProp,
  ElementAttr,
  AddEventListener,
  ToggleClass,
  AssignText,
  InsertBoundary,
  ClearBoundary,

  RevSeq,
  Eval,
  ExecCallback,
  UncaughtException,
}

export type Expr =
  | { tag: ExprTag.Null }
  | { tag: ExprTag.Boolean, 0: number }
  | { tag: ExprTag.Num, coefficient: number, base10Exponent: number }
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
  | { tag: ExprTag.Lam, body: Expr }
  | { tag: ExprTag.Arg, scopeIx: number, argIx: number }
  | { tag: ExprTag.Apply, 0: Expr, 1: Expr[] }
  | { tag: ExprTag.Call, 0: Expr, 1: JSFunctionName, 2: Expr[] }

  | { tag: ExprTag.AssignVar, lhs: number, rhs: Expr }
  | { tag: ExprTag.FreeVar, varId: number }
  | { tag: ExprTag.Var, varId: number }

  | { tag: ExprTag.InsertNode, parent: Expr, child: Expr }
  | { tag: ExprTag.WithBuilder, builder: Expr, builderContent: Expr }
  | { tag: ExprTag.CreateElement, tagName: string }
  | { tag: ExprTag.CreateText, content: string }
  | { tag: ExprTag.ElementProp, node: Expr, propName: string, propValue: Expr }
  | { tag: ExprTag.ElementAttr, node: Expr, attrName: string, attrValue: string }
  | { tag: ExprTag.AddEventListener, node: Expr, eventName: string, listener: Expr }
  | { tag: ExprTag.ToggleClass, node: Expr, className: string, enable: number }
  | { tag: ExprTag.AssignText, node: Expr, content: string }
  | { tag: ExprTag.InsertBoundary, parent: Expr }
  | { tag: ExprTag.ClearBoundary, boundary: Expr, detach: number }

  | { tag: ExprTag.RevSeq, exprs: Expr[] }
  | { tag: ExprTag.Eval, rawJavaScript: string }
  | { tag: ExprTag.ExecCallback, callbackId: number, arg: Expr }
  | { tag: ExprTag.UncaughtException, message: string }
;

export const expr = b.recursive<Expr>(self => b.discriminate({
  [ExprTag.Null]: b.record({}),
  [ExprTag.Boolean]: b.record({ 0: b.int8 }),
  [ExprTag.Num]: b.record({ coefficient: b.int64, base10Exponent: b.int8 }),
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
  [ExprTag.Lam]: b.record({ body: self }),
  [ExprTag.Arg]: b.record({ scopeIx: b.int8, argIx: b.int8 }),
  [ExprTag.Apply]: b.record({ 0: self, 1: b.array(self) }),
  [ExprTag.Call]: b.record({ 0: self, 1: b.string, 2: b.array(self) }),

  [ExprTag.AssignVar]: b.record({ lhs: b.int64, rhs: self }),
  [ExprTag.FreeVar]: b.record({ varId: b.int64 }),
  [ExprTag.Var]: b.record({ varId: b.int64 }),

  [ExprTag.InsertNode]: b.record({ parent: self, child: self }),
  [ExprTag.WithBuilder]: b.record({ builder: self, builderContent: self }),
  [ExprTag.CreateElement]: b.record({ tagName: b.string }),
  [ExprTag.CreateText]: b.record({ content: b.string }),
  [ExprTag.ElementProp]: b.record({ node: self, propName: b.string, propValue: self }),
  [ExprTag.ElementAttr]: b.record({ node: self, attrName: b.string, attrValue: b.string }),
  [ExprTag.AddEventListener]: b.record({ node: self, eventName: b.string, listener: self }),
  [ExprTag.ToggleClass]: b.record({ node: self, className: b.string, enable: b.int8 }),
  [ExprTag.AssignText]: b.record({ node: self, content: b.string }),
  [ExprTag.InsertBoundary]: b.record({ parent: self }),
  [ExprTag.ClearBoundary]: b.record({ boundary: self, detach: b.int8 }),

  [ExprTag.RevSeq]: b.record({ exprs: b.array(self) }),
  [ExprTag.Eval]: b.record({ rawJavaScript: b.string }),
  [ExprTag.ExecCallback]: b.record({ callbackId: b.int64, arg: self }),
  [ExprTag.UncaughtException]: b.record({ message: b.string }),
}));

export enum HaskellMessageTag {
  EvalExpr,
  HotReload,
  Exit,
}

export const haskellMessage = b.discriminate({
  [HaskellMessageTag.EvalExpr]: b.record({ expr: expr }),
  [HaskellMessageTag.HotReload]: b.record({ }),
  [HaskellMessageTag.Exit]: b.record({ }),
});

export enum JavaScriptMessageTag {
  Start,
  Return,
  ExecCallback,
  BeforeUnload,
}

export const javascriptMessage = b.discriminate({
  [JavaScriptMessageTag.Start]: b.record({ startFlags }),
  [JavaScriptMessageTag.Return]: b.record({ 0: jvalue }),
  [JavaScriptMessageTag.ExecCallback]: b.record({ arg: jvalue, callbackId: b.int64 }),
  [JavaScriptMessageTag.BeforeUnload]: b.record({}),
});

export type HaskellMessage = typeof haskellMessage['_A'];
export type JavaScriptMessage = typeof javascriptMessage['_A'];

export const varStorage = new Map<number, unknown>();

export function mkStartMessage(): JavaScriptMessage {
  return { tag: JavaScriptMessageTag.Start, startFlags: { initial_url: location.href } };
}

namespace domBuilder {
  export function insertIntoBuilder(builder: Element|Comment, child: Node): void {
    if (builder instanceof Comment) {
      builder.parentElement!.insertBefore(child, builder);
    } else {
      builder.appendChild(child);
    }
  }

  export function assignProperty(parent: Element|Comment, propName: string, propValue: unknown): void {
    if (parent instanceof Comment) {
      (parent.parentElement as any)[propName] = propValue;
    } else {
      (parent as any)[propName] = propValue;
    }
  }

  export function assignAttribute(builder: Element|Comment, attrName: string, attrValue: string): void {
    const element = domBuilderElement(builder);
    element.setAttribute(attrName, attrValue);
  }

  export function addEventListener(builder: Element|Comment, eventName: string, listener: EventListener): void {
    const element = domBuilderElement(builder);
    element.addEventListener(eventName, listener);
  }

  export function toggleClass(builder: Element|Comment, className: string, enable: boolean): void {
    const element = domBuilderElement(builder);
    if (enable) {
      element.classList.add(className);
    } else {
      element.classList.remove(className);
    }
  }

  export function insertBoundary(builder: Element|Comment): Comment {
    const begin = document.createComment('ContentBoundary {{');
    const end = document.createComment('}}');
    insertIntoBuilder(builder, begin);
    insertIntoBuilder(builder, end);
    return end;
  }

  export function clearBoundary(boundary: Comment, detach: boolean): void {
    const end = boundary;
    let nestedCounter = 0;
    for (;;){
      if (!end.previousSibling ||
        (nestedCounter == 0 && isOpeningBoundary(end.previousSibling))
         ) break;
      if (isClosingBoundary(end.previousSibling)) nestedCounter++;
      else if (isOpeningBoundary(end.previousSibling)) nestedCounter--;
      end.previousSibling!.parentNode!.removeChild(end.previousSibling!);
    }
    if (detach) {
      end.previousSibling!.parentNode!.removeChild(end.previousSibling!);
      end.parentNode!.removeChild(end);
    }
  }

  function domBuilderElement(builder: Element|Comment): Element {
    if (builder instanceof Comment) {
      return builder.parentElement!;
    }
    return builder;
  }

  function isOpeningBoundary(node: Node): boolean {
    if (node instanceof Comment && node.textContent == 'ContentBoundary {{') {
      return true;
    }
    return false;
  }

  function isClosingBoundary(node: Node): boolean {
    if (node instanceof Comment && node.textContent == '}}') {
      return true;
    }
    return false;
  }
};
