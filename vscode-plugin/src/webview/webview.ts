import * as proto from "../shared/webviewProtocol.js"

/**
 *  Class that manages webview state.
 */
class Webview {

    constructor() {
    }

}

declare var acquireVsCodeApi: any

{
    const sys = new Webview()

    window.addEventListener('message', event => {
        const message = event.data as proto.e2w.Event // The json data that the extension sent
//        switch (message.tag) {
//        }
    })
}
