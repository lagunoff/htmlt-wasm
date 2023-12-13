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

export type HaskellCallback = (jsMsg: JavaScriptMessage, argScope: List<IArguments>) => void;

export function evalExpr(idenScope: List<Bindings>, argScope: List<IArguments>, hscb: HaskellCallback, exp: Expr): unknown {
  switch(exp.tag) {
    case ExprTag.Null: {
       return null;
    }
    case ExprTag.Boolean: {
       return exp[0] != 0;
    }
    case ExprTag.Num: {
      return Number(exp.decimal);
    }
    case ExprTag.Str: {
      return exp[0];
    }
    case ExprTag.Arr: {
      return exp[0].map(evalExpr.bind(undefined, idenScope, argScope, hscb));
    }
    case ExprTag.Obj: {
      return Object.fromEntries(exp[0].map(([k, e]) => [k, evalExpr(idenScope, argScope, hscb, e)]));
    }
    case ExprTag.Dot: {
      const lhs = evalExpr(idenScope, argScope, hscb, exp[0]) as any;
      return lhs[exp[1]];
    }
    case ExprTag.AssignProp: {
      const rhs = evalExpr(idenScope, argScope, hscb, exp[2]);
      const obj = evalExpr(idenScope, argScope, hscb, exp[0]) as any;
      obj[exp[1]] = rhs;
      return rhs;
    }
    case ExprTag.Ix: {
      const rhs: any = evalExpr(idenScope, argScope, hscb, exp.exp);
      return rhs[exp.ix];
    }
    case ExprTag.Add: {
      const lhs = evalExpr(idenScope, argScope, hscb, exp[0]) as number;
      const rhs = evalExpr(idenScope, argScope, hscb, exp[1]) as number;
      return lhs + rhs;
    }
    case ExprTag.Subtract: {
      const lhs = evalExpr(idenScope, argScope, hscb, exp[0]) as number;
      const rhs = evalExpr(idenScope, argScope, hscb, exp[1]) as number;
      return lhs - rhs;
    }
    case ExprTag.Multiply: {
      const lhs = evalExpr(idenScope, argScope, hscb, exp[0]) as number;
      const rhs = evalExpr(idenScope, argScope, hscb, exp[1]) as number;
      return lhs * rhs;
    }
    case ExprTag.Divide: {
      const lhs = evalExpr(idenScope, argScope, hscb, exp[0]) as number;
      const rhs = evalExpr(idenScope, argScope, hscb, exp[1]) as number;
      return lhs / rhs;
    }
    case ExprTag.Id: {
      const ident = exp[0];
      for (let iter = idenScope; iter; iter = cdr(iter)) {
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
        return evalExpr(idenScope, Cons(arguments, argScope), hscb, exp.body);
      };
    }
    case ExprTag.Arg: {
      let iter = argScope;
      let j = 0;
      while (iter) {
        if (j == exp.scopeIx) {
          const iarguments = car(iter);
          return iarguments[exp.argIx];
        }
        iter = cdr(iter);
        j++;
      }
      throw new Error('Argument scope out of a range: ' + exp.scopeIx);
    }
    case ExprTag.Apply: {
      const lhs = evalExpr(idenScope, argScope, hscb, exp[0]) as Function;
      return lhs.apply(undefined, exp[1].map(evalExpr.bind(undefined, idenScope, argScope, hscb)));
    }
    case ExprTag.Call: {
      const lhs = evalExpr(idenScope, argScope, hscb, exp[0]) as any;
      const fn = lhs[exp[1]];
      return fn.apply(lhs, exp[2].map(evalExpr.bind(undefined, idenScope, argScope, hscb)));
    }
    case ExprTag.AssignVar: {
      const rhs = evalExpr(idenScope, argScope, hscb, exp.rhs);
      if (varStorage.has(exp.scopeId)) {
        const scopeMap = varStorage.get(exp.scopeId)!;
        scopeMap.set(exp.varId, rhs);
      } else {
        const scopeMap = new Map();
        scopeMap.set(exp.varId, rhs);
        varStorage.set(exp.scopeId, scopeMap);
      }
      return rhs;
    }
    case ExprTag.FreeVar: {
      return;
    }
    case ExprTag.Var: {
      return varStorage.get(exp.scopeId)?.get(exp.varId);
    }
    case ExprTag.FreeScope: {
      return varStorage.delete(exp.scopeId);
    }
    case ExprTag.InsertNode: {
      const parent = evalExpr(idenScope, argScope, hscb, exp.parent) as Element|Comment;
      const child = evalExpr(idenScope, argScope, hscb, exp.child) as Node;
      domBuilder.insertIntoBuilder(parent, child);
      return null;
    }
    case ExprTag.WithDomBuilder: {
      const builder = evalExpr(idenScope, argScope, hscb, exp.builder) as Element|Comment;
      const builderContent = evalExpr(idenScope, argScope, hscb, exp.builderContent) as Function;
      builderContent(builder);
      return builder;
    }
    case ExprTag.CreateElement: {
      return document.createElement(exp.tagName);
    }
    case ExprTag.CreateElementNS: {
      return document.createElementNS(exp.ns, exp.tagName);
    }
    case ExprTag.CreateText: {
      return document.createTextNode(exp.content);
    }
    case ExprTag.ElementProp: {
      const parent = evalExpr(idenScope, argScope, hscb, exp.node) as Element|Comment;
      const propValue = evalExpr(idenScope, argScope, hscb, exp.propValue);
      domBuilder.assignProperty(parent, exp.propName, propValue);
      return null;
    }
    case ExprTag.ElementAttr: {
      const parent = evalExpr(idenScope, argScope, hscb, exp.node) as Element|Comment;
      domBuilder.assignAttribute(parent, exp.attrName, exp.attrValue);
      return null;
    }
    case ExprTag.AddEventListener: {
      const parent = evalExpr(idenScope, argScope, hscb, exp.node) as Element|Comment;
      const listener = evalExpr(idenScope, argScope, hscb, exp.listener) as EventListener;
      domBuilder.addEventListener(parent, exp.eventName, listener);
      return null;
    }
    case ExprTag.ToggleClass: {
      const parent = evalExpr(idenScope, argScope, hscb, exp.node) as Element|Comment;
      domBuilder.toggleClass(parent, exp.className, Boolean(exp.enable));
      return null;
    }
    case ExprTag.AssignText: {
      const node = evalExpr(idenScope, argScope, hscb, exp.node) as Text;
      node.textContent = exp.content;
      return null;
    }
    case ExprTag.InsertBoundary: {
      const parent = evalExpr(idenScope, argScope, hscb, exp.parent) as Element|Comment;
      return domBuilder.insertBoundary(parent);
    }
    case ExprTag.ClearBoundary: {
      const boundary = evalExpr(idenScope, argScope, hscb, exp.boundary) as Comment;
      return domBuilder.clearBoundary(boundary, Boolean(exp.detach));
    }
    case ExprTag.RevSeq: {
      return exp.exprs.reduceRight<unknown>((_, e) => evalExpr(idenScope, argScope, hscb, e), null);
    }
    case ExprTag.Eval: {
      return eval(exp.rawJavaScript);
    }
    case ExprTag.TriggerEvent: {
      const arg = evalExpr(idenScope, argScope, hscb, exp.arg);
      const jsMsg: JavaScriptMessage = {
        tag: JavaScriptMessageTag.TriggerEvent,
        arg: unknownToJValue(arg),
        callbackId: exp.callbackId,
      }
      return hscb(jsMsg, argScope);
    }
    case ExprTag.AsyncCallback: {
      const arg = evalExpr(idenScope, argScope, hscb, exp.arg);
      const jsMsg: JavaScriptMessage = {
        tag: JavaScriptMessageTag.AsyncCallback,
        arg: unknownToJValue(arg),
        callbackId: exp.callbackId,
      }
      return hscb(jsMsg, argScope);
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
    const decimal = inp.toString();
    return { tag: JValueTag.JNum, decimal };
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
  JObj,
  JArr,
  JStr,
  JNum,
  JBool,
  JNull,
}

export type JValue =
  | { tag: JValueTag.JObj, 0: [string, JValue][] }
  | { tag: JValueTag.JArr, 0: JValue[] }
  | { tag: JValueTag.JStr, 0: string }
  | { tag: JValueTag.JNum, decimal: string }
  | { tag: JValueTag.JBool, 0: number }
  | { tag: JValueTag.JNull }
;

export const jvalue = b.recursive<JValue>(self => b.discriminate({
  [JValueTag.JObj]: b.record({ 0: b.array(b.tuple(b.string, self)) }),
  [JValueTag.JArr]: b.record({ 0: b.array(self) }),
  [JValueTag.JStr]: b.record({ 0: b.string }),
  [JValueTag.JNum]: b.record({ decimal: b.string }),
  [JValueTag.JBool]: b.record({ 0: b.int8 }),
  [JValueTag.JNull]: b.record({ }),
}));

export type StartLocation = {
  protocol: string,
  hostname: string,
  port: string,
  pathname: string,
  search: string,
  hash: string,
};

export const startLocation: b.Decoder<StartLocation> = b.record({
  protocol: b.string,
  hostname: b.string,
  port: b.string,
  pathname: b.string,
  search: b.string,
  hash: b.string,
});

export type StartFlags = {
  initial_url: StartLocation;
  window_inner_size: [number, number];
};

export const startFlags: b.Decoder<StartFlags> = b.record({
  initial_url: startLocation,
  window_inner_size: b.tuple(b.int64, b.int64),
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
  FreeScope,

  InsertNode,
  WithDomBuilder,
  CreateElement,
  CreateElementNS,
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
  TriggerEvent,
  AsyncCallback,
  UncaughtException,
}

export type Expr =
  | { tag: ExprTag.Null }
  | { tag: ExprTag.Boolean, 0: number }
  | { tag: ExprTag.Num, decimal: string }
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

  | { tag: ExprTag.AssignVar, scopeId: number, varId: number, rhs: Expr }
  | { tag: ExprTag.FreeVar, scopeId: number, varId: number }
  | { tag: ExprTag.Var, scopeId: number, varId: number }
  | { tag: ExprTag.FreeScope, scopeId: number }

  | { tag: ExprTag.InsertNode, parent: Expr, child: Expr }
  | { tag: ExprTag.WithDomBuilder, builder: Expr, builderContent: Expr }
  | { tag: ExprTag.CreateElement, tagName: string }
  | { tag: ExprTag.CreateElementNS, ns: string, tagName: string }
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
  | { tag: ExprTag.TriggerEvent, callbackId: number, arg: Expr }
  | { tag: ExprTag.AsyncCallback, callbackId: number, arg: Expr }
  | { tag: ExprTag.UncaughtException, message: string }
;

export const expr = b.recursive<Expr>(self => b.discriminate({
  [ExprTag.Null]: b.record({}),
  [ExprTag.Boolean]: b.record({ 0: b.int8 }),
  [ExprTag.Num]: b.record({ decimal: b.string }),
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

  [ExprTag.AssignVar]: b.record({ scopeId: b.int64, varId: b.int64, rhs: self }),
  [ExprTag.FreeVar]: b.record({ scopeId: b.int64, varId: b.int64 }),
  [ExprTag.Var]: b.record({ scopeId: b.int64, varId: b.int64 }),
  [ExprTag.FreeScope]: b.record({ scopeId: b.int64 }),

  [ExprTag.InsertNode]: b.record({ parent: self, child: self }),
  [ExprTag.WithDomBuilder]: b.record({ builder: self, builderContent: self }),
  [ExprTag.CreateElement]: b.record({ tagName: b.string }),
  [ExprTag.CreateElementNS]: b.record({ ns: b.string, tagName: b.string }),
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
  [ExprTag.TriggerEvent]: b.record({ callbackId: b.int64, arg: self }),
  [ExprTag.AsyncCallback]: b.record({ callbackId: b.int64, arg: self }),
  [ExprTag.UncaughtException]: b.record({ message: b.string }),
}));

export enum HaskellMessageTag {
  EvalExpr,
  Yield,
  HotReload,
  Exit,
}

export const haskellMessage = b.discriminate({
  [HaskellMessageTag.EvalExpr]: b.record({ expr: expr }),
  [HaskellMessageTag.Yield]: b.record({ expr: expr }),
  [HaskellMessageTag.HotReload]: b.record({ }),
  [HaskellMessageTag.Exit]: b.record({ }),
});

export enum JavaScriptMessageTag {
  Start,
  Return,
  TriggerEvent,
  AsyncCallback,
  BeforeUnload,
}

export const javascriptMessage = b.discriminate({
  [JavaScriptMessageTag.Start]: b.record({ startFlags }),
  [JavaScriptMessageTag.Return]: b.record({ 0: jvalue }),
  [JavaScriptMessageTag.TriggerEvent]: b.record({ arg: jvalue, callbackId: b.int64 }),
  [JavaScriptMessageTag.AsyncCallback]: b.record({ arg: jvalue, callbackId: b.int64 }),
  [JavaScriptMessageTag.BeforeUnload]: b.record({}),
});

export type HaskellMessage = typeof haskellMessage['_A'];
export type JavaScriptMessage = typeof javascriptMessage['_A'];

export const varStorage = new Map<number, Map<number, unknown>>();

export function mkStartMessage(): JavaScriptMessage {
  const initial_url: StartLocation = {
    protocol: location.protocol,
    hostname: location.hostname,
    port: location.port,
    pathname: location.pathname,
    search: location.search,
    hash: location.hash,
  };
  return {
    tag: JavaScriptMessageTag.Start,
    startFlags: {
      initial_url,
      window_inner_size: [window.innerWidth, window.innerHeight]
    }
  };
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
