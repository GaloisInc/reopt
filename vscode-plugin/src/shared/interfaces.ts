import * as vscode from 'vscode'

import * as W2E from './webview-to-extension'

/**
 * This state is persisted between multiple "displays" of the webview, during a
 * single invocation of VSCode.  It is **not** persisted across VSCode
 * invocations, for this, you want to use the 'workspaceState' in the extension.
 */
export interface ActivityViewPersistedState {
    projectName?: string
}

export interface ActivityViewVSCodeAPI {
    getState(): ActivityViewPersistedState | undefined
    setState(s: ActivityViewPersistedState): void
    postMessage(m: W2E.ActivityWebviewToExtension): void
}

export interface MainViewPersistedState {
    results: string[]
}

export interface MainViewVSCodeAPI {
    getState(): MainViewPersistedState | undefined
    setState(s: MainViewPersistedState): void
    postMessage(m: W2E.MainWebviewToExtension): void
}

export interface SerializedPosition {
    readonly character: number
    readonly line: number
}

export interface SerializedRange {
    readonly end: SerializedPosition,
    readonly start: SerializedPosition,
}

export interface SerializedSymbolInformation {
    readonly containerName: string
    readonly fsPath: string
    readonly kind: vscode.SymbolKind
    readonly name: string
    readonly range: SerializedRange
}
