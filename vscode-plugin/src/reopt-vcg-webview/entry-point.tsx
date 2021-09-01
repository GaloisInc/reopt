import * as React from 'react'
import * as ReactDOM from 'react-dom'

import * as Constants from '@shared/constants'
import * as E2W from '@shared/extension-to-reopt-vcg-webview'
import * as Interfaces from '@shared/interfaces'

import { Webview } from './webview'

interface WebviewWindow extends Window {
    [E2W.webviewInitialDataKey]: E2W.WebviewInitialData,
}
declare const window: WebviewWindow

declare const acquireVsCodeApi: () => Interfaces.ReoptVCGViewVSCodeAPI
const vscode = acquireVsCodeApi()

const container = document.getElementById(Constants.activityWebviewId) as HTMLDivElement | null

if (container === null) {
    console.log(`Webview container with id ${Constants.webviewId} is missing`)
} else {
    ReactDOM.render(
        <Webview
            initialData={window[E2W.webviewInitialDataKey]}
            vscode={vscode}
        />,
        container,
    )
}
