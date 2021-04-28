import * as React from 'react'
import * as ReactDOM from 'react-dom'

import * as Constants from '@shared/constants'
import * as E2W from '@shared/extension-to-webview'
import * as Interfaces from '@shared/interfaces'

import { ActivityWebview } from './activity-webview'

interface ActivityWebviewWindow extends Window {
    [E2W.activityWebviewInitialDataKey]: E2W.ActivityWebviewInitialData,
}
declare const window: ActivityWebviewWindow

declare const acquireVsCodeApi: () => Interfaces.ActivityViewVSCodeAPI
const vscode = acquireVsCodeApi()

const container = document.getElementById(Constants.activityWebviewId) as HTMLDivElement | null

if (container === null) {
    console.log(`Webview container with id ${Constants.activityWebviewId} is missing`)
} else {
    ReactDOM.render(
        <ActivityWebview
            initialData={window[E2W.activityWebviewInitialDataKey]}
            vscode={vscode}
        />,
        container,
    )
}
