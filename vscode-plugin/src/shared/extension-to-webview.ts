import * as Interfaces from './interfaces'

/**
 * WARNING: make sure that all of the data you put in messages is serialized. In
 * particular, avoid using classes, since you will not get back the same object
 * on the other end!
 */

export const webviewInitialDataKey = 'webviewInitialData'
export interface WebviewInitialData {
}

export const activityWebviewInitialDataKey = 'activityWebviewInitialData'
export interface ActivityWebviewInitialData {
    readonly projectName?: string
    readonly symbols: Interfaces.SerializedSymbolInformation[]
}

export type ExtensionToActivityWebview
    = ClosedProject
    | OpenedProject
    | SymbolList

export const closedProject = 'closedProject'
export interface ClosedProject {
    readonly tag: typeof closedProject
}

export const openedProject = 'OpenedProject'
export interface OpenedProject {
    readonly tag: typeof openedProject
    readonly projectName: string
}

export const symbolList = 'SymbolList'
export interface SymbolList {
    readonly tag: typeof symbolList
    readonly symbols: Interfaces.SerializedSymbolInformation[]
}
