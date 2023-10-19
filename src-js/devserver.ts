import { absurd } from './lib';
import * as p from './protocol';
import { HaskellMessage, JavaScriptMessage, JavaScriptMessageTag, HaskellMessageTag, Bindings, List } from './protocol';

export async function devClient(devSocketUri: string) {
  const websocket = new WebSocket(devSocketUri);

  websocket.onopen = (_event) => {
    const binaryData = p.javascriptMessage.encode({ tag: JavaScriptMessageTag.Start });
    websocket.send(binaryData);
  };

  // Event handler for receiving messages from the server
  websocket.onmessage = async (event) => {
    const binaryDataReceived = await convertBlobToUint8Array(event.data);
    const upCommand = p.haskellMessage.decode(binaryDataReceived);
    haskellApp(upCommand, (downCmd: JavaScriptMessage) => websocket.send(p.javascriptMessage.encode(downCmd)));
  };

  // Event handler for errors
  websocket.onerror = (event) => {
    console.error("WebSocket error:", event);
  };

  // Event handler for when the connection is closed
  websocket.onclose = (event) => {
    console.log("WebSocket connection closed:", event);
  };
}

export async function haskellApp(upCmd: HaskellMessage, send: (downCmd: JavaScriptMessage) => void) {
  switch (upCmd.tag) {
    case HaskellMessageTag.EvalExpr: {
      const result = p.evalExpr(globalContext, null, send, upCmd.expr);
      const jvalue = p.unknownToJValue(result);
      return send({ tag: JavaScriptMessageTag.Return, 0: jvalue });
    }
    case HaskellMessageTag.HotReload: {
      window.location.reload();
      return;
    }
    case HaskellMessageTag.Exit: {
      return;
    }
  }
  absurd(upCmd);
}

const globalContext: List<Bindings> = [window as any, null]

export function convertBlobToUint8Array(blob: Blob): Promise<Uint8Array> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();

    reader.onload = () => {
      const arrayBuffer = reader.result as ArrayBuffer;
      const uint8Array = new Uint8Array(arrayBuffer);
      resolve(uint8Array);
    };

    reader.onerror = (error) => {
      reject(error);
    };

    reader.readAsArrayBuffer(blob);
  });
}
