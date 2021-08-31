import * as Interfaces from './interfaces'

/**
 * WARNING: make sure that all of the data you put in messages is serialized. In
 * particular, avoid using classes, since you will not get back the same object
 * on the other end!
 */

export const webviewInitialDataKey = 'webviewInitialData'
export type WebviewInitialData = {
    _tag: 'WebviewInitialData'
}

export type ExtensionToReoptVCGWebview
    = ClearReoptVCGEntries
    | ReoptVCGEntry

export const clearReoptVCGEntries = 'ClearReoptVCGEntries'
export type ClearReoptVCGEntries = Readonly<{
    tag: typeof clearReoptVCGEntries
}>

export const reoptVCGEntry = 'ReoptVCGEntry'
export type ReoptVCGEntry = Readonly<{
    tag: typeof reoptVCGEntry
    entry: Interfaces.ReoptVCGEntry
}>
