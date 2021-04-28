import { isLeft } from 'fp-ts/Either'
import { toNullable } from 'fp-ts/Option'
import * as vscode from 'vscode'

import {
    ProjectConfiguration,
    decodeConfiguration,
    setWorkspaceConfiguration,
    setWorkspaceProjectFile,
} from '@shared/project-configuration'
import * as Promisified from '@shared/promisified'
import * as E2W from '@shared/extension-to-webview'


async function readReoptProjectFile(
    reoptProjectFile: string,
): Promise<ProjectConfiguration> {

    const projectContents = await Promisified.readFile(reoptProjectFile)

    const decoded = decodeConfiguration(projectContents.toString())

    if (isLeft(decoded)) {
        vscode.window.showErrorMessage(
            `Could not decode your configuration file. See your console.`
        )
        console.log(decoded.left)
        return Promise.reject()
    }

    return decoded.right

}


async function readAndSetProjectConfiguration(
    context: vscode.ExtensionContext,
    webview: vscode.Webview,
    reoptProjectFile: string,
): Promise<void> {
    const configuration = await readReoptProjectFile(reoptProjectFile)
    setWorkspaceConfiguration(context, configuration)
    webview.postMessage({
        tag: E2W.openedProject,
        projectName: toNullable(configuration.name),
    } as E2W.OpenedProject)
}


export async function openReoptProject(
    context: vscode.ExtensionContext,
    webview: vscode.Webview,
    reoptProjectFile: string,
): Promise<vscode.FileSystemWatcher> {

    setWorkspaceProjectFile(context, reoptProjectFile)
    readAndSetProjectConfiguration(context, webview, reoptProjectFile)

    const watcher = vscode.workspace.createFileSystemWatcher(
        reoptProjectFile,
    )
    watcher.onDidChange(async () => {
        readAndSetProjectConfiguration(context, webview, reoptProjectFile)
    })
    return watcher

}


export async function openReoptProjectViaDialog(
    context: vscode.ExtensionContext,
    webview: vscode.Webview,
): Promise<vscode.FileSystemWatcher> {

    const files = await vscode.window.showOpenDialog({
        filters: {
            'Reopt project': ['rpj'],
            'All files': ['*'],
        },
        openLabel: 'Open reopt project',
        title: 'Choose reopt project to open',
    })

    if (files === undefined || files.length !== 1) {
        return Promise.reject()
    }
    const reoptProjectFile = files[0].fsPath

    return openReoptProject(context, webview, reoptProjectFile)

}
