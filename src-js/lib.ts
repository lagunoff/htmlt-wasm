
/** Helper for totality checking */
export function absurd(_x: never): any {
  throw new Error('absurd: unreachable code');
}
