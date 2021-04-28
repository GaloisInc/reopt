import * as React from 'react'
import * as ReactDOM from 'react-dom'

import * as Constants from '@shared/constants'
import * as E2W from '@shared/extension-to-webview'
import * as Interfaces from '@shared/interfaces'

export function Webview(_props: {
    initialData: E2W.WebviewInitialData,
    vscode: Interfaces.MainViewVSCodeAPI,
}): JSX.Element {
    return (<div>This is the main webview.</div>)
}
