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
    = ReoptVCGOutput

export const reoptVCGOutput = 'reoptVCGOutput'
export type ReoptVCGOutput = Readonly<{
    tag: typeof reoptVCGOutput
    output: Interfaces.ReoptVCGEntry[]
}>
