import * as b from './binary';
import { absurd, IntMap } from './lib';

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
      const scopeStorage = varStorage.get(exp.scopeId);
      if (!scopeStorage) return;
      scopeStorage.delete(exp.varId);
      if (scopeStorage.size == 0) {
        varStorage.delete(exp.scopeId);
      }
      return;
    }
    case ExprTag.Var: {
      return varStorage.get(exp.scopeId)?.get(exp.varId);
    }
    case ExprTag.FreeScope: {
      varStorage.delete(exp.scopeId);
      const scopeFinalizers = finalizers.get(exp.scopeId);
      if (scopeFinalizers) scopeFinalizers.forEach(fn => fn ());
      return null;
    }
    case ExprTag.InsertNode: {
      const parent = evalExpr(idenScope, argScope, hscb, exp.parent) as Element|Comment;
      const child = evalExpr(idenScope, argScope, hscb, exp.child) as Node;
      domHelpers.insertIntoBuilder(parent, child);
      return null;
    }
    case ExprTag.WithDomBuilder: {
      const dest = evalExpr(idenScope, argScope, hscb, exp.dest) as Element|Comment;
      const builderFn = evalExpr(idenScope, argScope, hscb, exp.builderFn) as Function;
      builderFn(dest);
      return dest;
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
      domHelpers.assignProperty(parent, exp.propName, propValue);
      return null;
    }
    case ExprTag.ElementAttr: {
      const parent = evalExpr(idenScope, argScope, hscb, exp.node) as Element|Comment;
      domHelpers.assignAttribute(parent, exp.attrName, exp.attrValue);
      return null;
    }
    case ExprTag.InsertClassList: {
      const parent = evalExpr(idenScope, argScope, hscb, exp.node) as Element|Comment;
      const element = domHelpers.domBuilderElement(parent);
      exp.classList.forEach(className => element.classList.add(className));
      return null;
    }
    case ExprTag.RemoveClassList: {
      const parent = evalExpr(idenScope, argScope, hscb, exp.node) as Element|Comment;
      const element = domHelpers.domBuilderElement(parent);
      exp.classList.forEach(className => element.classList.remove(className));
      return null;
    }
    case ExprTag.AssignText: {
      const node = evalExpr(idenScope, argScope, hscb, exp.node) as Text;
      node.textContent = exp.content;
      return null;
    }
    case ExprTag.InsertBoundary: {
      const parent = evalExpr(idenScope, argScope, hscb, exp.parent) as Element|Comment;
      return domHelpers.insertBoundary(parent);
    }
    case ExprTag.ClearBoundary: {
      const boundary = evalExpr(idenScope, argScope, hscb, exp.boundary) as Comment;
      return domHelpers.clearBoundary(boundary, Boolean(exp.detach));
    }
    case ExprTag.AddEventListener: {
      const target = evalExpr(idenScope, argScope, hscb, exp.target) as Element|Comment;
      const eventName = evalExpr(idenScope, argScope, hscb, exp.eventName) as string;
      const listener = evalExpr(idenScope, argScope, hscb, exp.listener) as EventListener;
      domHelpers.addEventListener(target, eventName, listener);
      const existingScope = finalizers.get(exp.reactiveScope);
      const scopeFinalizers = existingScope ? existingScope : new IntMap<Function>();
      if (!existingScope) finalizers.set(exp.reactiveScope, scopeFinalizers);
      return scopeFinalizers.push(() => domHelpers.removeEventListener(target, eventName, listener));
    }
    case ExprTag.SetTimeout: {
      const callback = evalExpr(idenScope, argScope, hscb, exp.callback) as Function;
      const existingScope = finalizers.get(exp.reactiveScope);
      const scopeFinalizers = existingScope ? existingScope : new IntMap<Function>();
      if (!existingScope) finalizers.set(exp.reactiveScope, scopeFinalizers);
      let timeoutId: NodeJS.Timeout|null = null;
      const finalizerId = scopeFinalizers.push(() => timeoutId && clearTimeout(timeoutId));
      timeoutId = setTimeout(() => {
        scopeFinalizers.delete(finalizerId);
        timeoutId = null;
        callback();
      }, exp.timeout);
      return finalizerId;
    }
    case ExprTag.ApplyFinalizer: {
      const existingScope = finalizers.get(exp.reactiveScope);
      if (!existingScope) return false;
      const cancellerFn = existingScope.get(exp.finalizerId);
      if (!cancellerFn) return false;
      existingScope.delete(exp.finalizerId);
      cancellerFn();
      return true;
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
    case ExprTag.TriggerAnimation: {
      const arg = evalExpr(idenScope, argScope, hscb, exp.arg);
      const jsMsg: JavaScriptMessage = {
        tag: JavaScriptMessageTag.TriggerAnimation,
        arg: unknownToJValue(arg),
        callbackId: exp.callbackId,
      }
      return hscb(jsMsg, argScope);
    }
    case ExprTag.TriggerCallback: {
      const arg = evalExpr(idenScope, argScope, hscb, exp.arg);
      const jsMsg: JavaScriptMessage = {
        tag: JavaScriptMessageTag.TriggerCallback,
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
  InsertClassList,
  RemoveClassList,
  AssignText,
  InsertBoundary,
  ClearBoundary,

  AddEventListener,
  SetTimeout,
  ApplyFinalizer,

  RevSeq,
  Eval,
  TriggerEvent,
  TriggerAnimation,
  TriggerCallback,
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
  | { tag: ExprTag.WithDomBuilder, dest: Expr, builderFn: Expr }
  | { tag: ExprTag.CreateElement, tagName: string }
  | { tag: ExprTag.CreateElementNS, ns: string, tagName: string }
  | { tag: ExprTag.CreateText, content: string }
  | { tag: ExprTag.ElementProp, node: Expr, propName: string, propValue: Expr }
  | { tag: ExprTag.ElementAttr, node: Expr, attrName: string, attrValue: string }
  | { tag: ExprTag.InsertClassList, node: Expr, classList: string[] }
  | { tag: ExprTag.RemoveClassList, node: Expr, classList: string[] }
  | { tag: ExprTag.AssignText, node: Expr, content: string }
  | { tag: ExprTag.InsertBoundary, parent: Expr }
  | { tag: ExprTag.ClearBoundary, boundary: Expr, detach: number }

  | { tag: ExprTag.AddEventListener, reactiveScope: number, target: Expr, eventName: Expr, listener: Expr }
  | { tag: ExprTag.SetTimeout, reactiveScope: number, callback: Expr, timeout: number }
  | { tag: ExprTag.ApplyFinalizer, reactiveScope: number, finalizerId: number }

  | { tag: ExprTag.RevSeq, exprs: Expr[] }
  | { tag: ExprTag.Eval, rawJavaScript: string }
  | { tag: ExprTag.TriggerEvent, callbackId: number, arg: Expr }
  | { tag: ExprTag.TriggerAnimation, callbackId: number, arg: Expr }
  | { tag: ExprTag.TriggerCallback, callbackId: number, arg: Expr }
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
  [ExprTag.WithDomBuilder]: b.record({ dest: self, builderFn: self }),
  [ExprTag.CreateElement]: b.record({ tagName: b.string }),
  [ExprTag.CreateElementNS]: b.record({ ns: b.string, tagName: b.string }),
  [ExprTag.CreateText]: b.record({ content: b.string }),
  [ExprTag.ElementProp]: b.record({ node: self, propName: b.string, propValue: self }),
  [ExprTag.ElementAttr]: b.record({ node: self, attrName: b.string, attrValue: b.string }),
  [ExprTag.InsertClassList]: b.record({ node: self, classList: b.array(b.string) }),
  [ExprTag.RemoveClassList]: b.record({ node: self, classList: b.array(b.string) }),
  [ExprTag.AssignText]: b.record({ node: self, content: b.string }),
  [ExprTag.InsertBoundary]: b.record({ parent: self }),
  [ExprTag.ClearBoundary]: b.record({ boundary: self, detach: b.int8 }),

  [ExprTag.AddEventListener]: b.record({ reactiveScope: b.int64, target: self, eventName: self, listener: self }),
  [ExprTag.SetTimeout]: b.record({ reactiveScope: b.int64, callback: self, timeout: b.int64 }),
  [ExprTag.ApplyFinalizer]: b.record({ reactiveScope: b.int64, finalizerId: b.int64 }),

  [ExprTag.RevSeq]: b.record({ exprs: b.array(self) }),
  [ExprTag.Eval]: b.record({ rawJavaScript: b.string }),
  [ExprTag.TriggerEvent]: b.record({ callbackId: b.int64, arg: self }),
  [ExprTag.TriggerAnimation]: b.record({ callbackId: b.int64, arg: self }),
  [ExprTag.TriggerCallback]: b.record({ callbackId: b.int64, arg: self }),
  [ExprTag.UncaughtException]: b.record({ message: b.string }),
}));

export enum HaskellMessageTag {
  EvalExpr,
  Yield,
  HotReload,
  Done,
}

export const haskellMessage = b.discriminate({
  [HaskellMessageTag.EvalExpr]: b.record({ expr: expr }),
  [HaskellMessageTag.Yield]: b.record({ expr: expr }),
  [HaskellMessageTag.HotReload]: b.record({ }),
  [HaskellMessageTag.Done]: b.record({ }),
});

export enum JavaScriptMessageTag {
  Start,
  Return,
  TriggerEvent,
  TriggerAnimation,
  TriggerCallback,
  BeforeUnload,
}

export const javascriptMessage = b.discriminate({
  [JavaScriptMessageTag.Start]: b.record({ startFlags }),
  [JavaScriptMessageTag.Return]: b.record({ 0: jvalue }),
  [JavaScriptMessageTag.TriggerEvent]: b.record({ arg: jvalue, callbackId: b.int64 }),
  [JavaScriptMessageTag.TriggerAnimation]: b.record({ arg: jvalue, callbackId: b.int64 }),
  [JavaScriptMessageTag.TriggerCallback]: b.record({ arg: jvalue, callbackId: b.int64 }),
  [JavaScriptMessageTag.BeforeUnload]: b.record({}),
});

export type HaskellMessage = typeof haskellMessage['_A'];
export type JavaScriptMessage = typeof javascriptMessage['_A'];

export type ReactiveScope = number;
export type VarId = number;

export const varStorage = new Map<ReactiveScope, Map<VarId, unknown>>();
export const finalizers = new Map<ReactiveScope, IntMap<Function>>;

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

namespace domHelpers {
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

  export function removeEventListener(builder: Element|Comment, eventName: string, listener: EventListener): void {
    const element = domBuilderElement(builder);
    element.removeEventListener(eventName, listener);
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

  export function domBuilderElement(builder: Element|Comment): Element {
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
