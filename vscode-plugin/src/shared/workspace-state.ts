import * as vscode from 'vscode'

import { ReoptVCGEntry } from './interfaces'
import { ProjectConfiguration } from './project-configuration'


export const reoptProjectConfiguration = 'reopt-project-configuration'
export const reoptProjectFile = 'reopt-project-file'
export const reoptVCGEntries = 'reopt-vcg-entries'


/**
 * WARNING: This type **must** be JSON-serializable!
 */
export type WorkspaceState = {
    [reoptProjectConfiguration]: ProjectConfiguration
    [reoptProjectFile]: string
    [reoptVCGEntries]: ReoptVCGEntry[]
}


export function clearVariable(
    context: vscode.ExtensionContext,
    key: keyof WorkspaceState,
): void {
    context.workspaceState.update(key, undefined)
}


function readVariable<Key extends keyof WorkspaceState>(
    context: vscode.ExtensionContext,
    key: Key,
): WorkspaceState[Key] | undefined {
    return context.workspaceState.get(key)
}


function writeVariable<Key extends keyof WorkspaceState>(
    context: vscode.ExtensionContext,
    key: Key,
    value: WorkspaceState[Key],
): void {
    context.workspaceState.update(key, value)
}


export function readReoptProjectFile(
    context: vscode.ExtensionContext,
): WorkspaceState[typeof reoptProjectFile] | undefined {
    return readVariable(context, reoptProjectFile)
}


export function readReoptProjectConfiguration(
    context: vscode.ExtensionContext,
): WorkspaceState[typeof reoptProjectConfiguration] | undefined {
    return readVariable(context, reoptProjectConfiguration)
}


export function readReoptVCGEntries(
    context: vscode.ExtensionContext,
): WorkspaceState[typeof reoptVCGEntries] {
    return readVariable(context, reoptVCGEntries) || []
}


export function writeReoptProjectFile(
    context: vscode.ExtensionContext,
    value: WorkspaceState[typeof reoptProjectFile],
): void {
    writeVariable(context, reoptProjectFile, value)
}


export function writeReoptProjectConfiguration(
    context: vscode.ExtensionContext,
    value: WorkspaceState[typeof reoptProjectConfiguration],
): void {
    writeVariable(context, reoptProjectConfiguration, value)
}


export function writeReoptVCGEntries(
    context: vscode.ExtensionContext,
    value: WorkspaceState[typeof reoptVCGEntries],
): void {
    writeVariable(context, reoptVCGEntries, value)
}
