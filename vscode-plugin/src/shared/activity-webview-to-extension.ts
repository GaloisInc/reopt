import * as vscode from 'vscode'

import * as Interfaces from './interfaces'

/**
 * WARNING: make sure that all of the data you put in messages is serialized. In
 * particular, avoid using classes, since you will not get back the same object
 * on the other end!
 */

export type ActivityWebviewToExtension
    = CloseProject
    | CreateProjectFile
    | GenerateCFG
    // | GenerateDisassembly
    | GenerateFunctions
    | GenerateLLVM
    | JumpToSymbol
    | OpenProject
    // | ReoptVCGOutput
    | ShowProjectFile

export const closeProject = 'CloseProject'
export interface CloseProject {
    readonly tag: typeof closeProject
}

export const createProjectFile = 'CreateProjectFile'
export interface CreateProjectFile {
    readonly tag: typeof createProjectFile
}

export const generateCFG = 'GenerateCFG'
export interface GenerateCFG {
    readonly tag: typeof generateCFG
}

// export const generateDisassembly = 'GenerateDisassembly'
// export interface GenerateDisassembly {
//     readonly tag: typeof generateDisassembly
// }

export const generateFunctions = 'GenerateFunctions'
export interface GenerateFunctions {
    readonly tag: typeof generateFunctions
}

export const generateLLVM = 'GenerateLLVM'
export interface GenerateLLVM {
    readonly tag: typeof generateLLVM
}

export const jumpToSymbol = 'JumpToSymbol'
export interface JumpToSymbol {
    readonly tag: typeof jumpToSymbol
    readonly symbol: Interfaces.SerializationOf<vscode.SymbolInformation>
}

export const openProject = 'OpenProject'
export interface OpenProject {
    readonly tag: typeof openProject
}

// export const reoptVCGOutput = 'ReoptVCGOutput'
// export interface ReoptVCGOutput {
//     readonly tag: typeof reoptVCGOutput
//     readonly output: Interfaces.ReoptVCGEntry[]
// }

export const showProjectFile = 'ShowProjectFile'
export interface ShowProjectFile {
    readonly tag: typeof showProjectFile
}

export type MainWebviewToExtension = never
