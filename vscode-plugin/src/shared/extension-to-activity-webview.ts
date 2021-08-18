import * as Interfaces from './interfaces'
import * as vscode from 'vscode'

/**
 * WARNING: make sure that all of the data you put in messages is serialized. In
 * particular, avoid using classes, since you will not get back the same object
 * on the other end!
 */

export const activityWebviewInitialDataKey = 'activityWebviewInitialData'
export interface ActivityWebviewInitialData {
    readonly projectName?: string
    readonly symbols: Interfaces.SerializationOf<vscode.SymbolInformation>[]
}

export type ExtensionToActivityWebview
    = ClosedProject
    | OpenedProject
    | SymbolList

export const closedProjectTag = 'ClosedProject'
export interface ClosedProject {
    readonly tag: typeof closedProjectTag
}

export const openedProjectTag = 'OpenedProject'
export interface OpenedProject {
    readonly tag: typeof openedProjectTag
    readonly projectName: string
}

export const symbolListTag = 'SymbolList'
export interface SymbolList {
    readonly tag: typeof symbolListTag
    readonly symbols: Interfaces.SerializationOf<vscode.SymbolInformation>[]
}
