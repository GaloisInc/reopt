import * as Interfaces from './interfaces'

/**
 * WARNING: make sure that all of the data you put in messages is serialized. In
 * particular, avoid using classes, since you will not get back the same object
 * on the other end!
 */


export const webviewInitialDataKey = 'webviewInitialData'
export type WebviewInitialData = {
    entries: Interfaces.ReoptVCGEntry[]
}


export enum Tags {
    addReoptVCGEntry = 'AddReoptVCGEntry',
    setReoptVCGEntries = 'SetReoptVCGEntries',
}

export type ExtensionToReoptVCGWebview
    = AddReoptVCGEntry
    | SetReoptVCGEntries

export type AddReoptVCGEntry = Readonly<{
    tag: Tags.addReoptVCGEntry
    entry: Interfaces.ReoptVCGEntry
}>

export type SetReoptVCGEntries = {
    tag: Tags.setReoptVCGEntries
    entries: Interfaces.ReoptVCGEntry[]
}
