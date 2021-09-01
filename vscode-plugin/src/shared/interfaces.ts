import * as vscode from 'vscode'

import * as W2E from './activity-webview-to-extension'

/* Branded types so that we cannot accidentally pass one for the other. */
type Flavor<T, F> = T & { _flavor?: F }
export type ActivityWebview = Flavor<vscode.Webview, 'activity-webview'>
export type ReoptVCGWebview = Flavor<vscode.Webview, 'reopt-vcg-webview'>

export type ReactSetter<T> = React.Dispatch<React.SetStateAction<T>>

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

export interface ReoptVCGViewPersistedState {
    results: ReoptVCGEntry[]
}

export interface ReoptVCGViewVSCodeAPI {
    getState(): ReoptVCGViewPersistedState | undefined
    setState(s: ReoptVCGViewPersistedState): void
    postMessage(m: W2E.MainWebviewToExtension): void
}

export const initialReoptVCGViewPersistedState : ReoptVCGViewPersistedState = {
    results: [],
}

export interface DisassemblyLineInformation {
    address: string
    /** Where the address appears in the line. */
    addressRange: vscode.Range
    /** Where the bytes appear in the line. */
    bytesRange: vscode.Range
    /** Address of the first byte of this line. */
    firstByteAddress: BigInt
    instruction: string
    /** Address of the last byte of this line. */
    lastByteAddress: BigInt
    /** From the first address character to the end of the line. */
    wholeLineRange: vscode.Range
}

export type SerializationOf<T>
    = T extends BigInt ? string
    : T extends DisassemblyLineInformation ? SerializedDisassemblyLineInformation
    : T extends string ? string
    : T extends vscode.Position ? SerializedPosition
    : T extends vscode.Range ? SerializedRange
    : T extends vscode.SymbolKind ? vscode.SymbolKind // currently serializable as is
    : T extends vscode.SymbolInformation ? SerializedSymbolInformation
    : never

interface SerializedPosition {
    readonly character: number
    readonly line: number
}

interface SerializedRange {
    readonly end: SerializationOf<vscode.Position>,
    readonly start: SerializationOf<vscode.Position>,
}

type SerializedDisassemblyLineInformation = {
    [K in keyof DisassemblyLineInformation]: SerializationOf<DisassemblyLineInformation[K]>
}

// For SymbolInformation, we capture some information differently
interface SerializedSymbolInformation {
    readonly containerName: string
    readonly fsPath: string
    readonly kind: SerializationOf<vscode.SymbolKind>
    readonly name: string
    readonly range: SerializationOf<vscode.Range>
}


export type ReoptVCGEntry = {
    'SMT check-sat result': 'sat' | 'unsat'
    'Machine Code Address': string
    'LLVM Instruction Index': string
    'LLVM Function Name': string
    'LLVM Block Label': string
    'Goal Tag': string
    'Goal Extra Info': string
}


export type OutputAndEventsFiles = {
    outputFile: string
    eventsFile: string
}
