/**
 * Small library for contructing HTML elements
 */
export function el (
  tagName:   string,
  props:     Record<string, unknown>,
  listeners: Partial<Record<keyof HTMLElementEventMap, EventListenerOrEventListenerObject>>
): HTMLElement {
  const newElm = document.createElement(tagName);
  Object.entries(props).forEach(([k, v]) => (newElm as any)[k] = v);
  Object.entries(listeners).forEach(([k, v]) => newElm.addEventListener(k, v));
  return newElm;
}

export function text (contents: string): Text {
  return new Text(contents);
}
