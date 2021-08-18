/**
 * Logic for handling messages coming from the activity webview.
*/

import { ExecFileException } from 'child_process'
import * as fs from 'fs'
import * as path from 'path'
import * as vscode from 'vscode'

import { option } from 'fp-ts'

import * as E2W from '@shared/extension-to-reopt-vcg-webview'
import {
    clearWorkspaceConfiguration,
    clearWorkspaceProjectFile,
    getWorkspaceConfiguration,
    getWorkspaceProjectFile,
    ProjectConfiguration,
} from '@shared/project-configuration'
import * as W2E from '@shared/reopt-vcg-webview-to-extension'

import { createReoptProject } from './create-reopt-project'
import {
    openReoptProject,
    openReoptProjectViaDialog,
} from './open-reopt-project'
import * as reopt from './reopt'
import { ActivityWebview } from '@shared/interfaces'


async function openOutputFile(outputFile: string): Promise<void> {
    const absoluteUri = vscode.Uri.file(path.resolve(outputFile))
    const doc = await vscode.workspace.openTextDocument(absoluteUri)
    await vscode.window.showTextDocument(doc, {
        preview: false,
    })
}


/** Just like vscode.OutputChannel, but the 'dispose' method is hidden. */
export type PrivateOutputChannel =
    Pick<vscode.OutputChannel, 'appendLine' | 'show'>
/**
 * When the user switches reopt projects, we drop the output channel for the
 * project to be closed, and open a new output channel for the project to be
 * opened.  This way, the messages in the output pane are always relevant to the
 * currently opened project.
 */
export type ReplaceOutputChannel =
    (newChannel: string) => PrivateOutputChannel


function makeDisplayError(
    replaceOutputChannel: ReplaceOutputChannel,
): (err: ExecFileException) => void {
    return (err) => {
        const channel = replaceOutputChannel('reopt error')
        // in my experience so far, the 'stack' has strictly more information
        // when present
        if (err.stack) {
            channel.appendLine(err.stack)

        } else {
            channel.appendLine(err.message)
        }
        channel.appendLine('The following command errored, scroll up for error messages.')
        channel.appendLine(err.cmd || 'No command, please report.')
        channel.show()
    }
}


/**
 * Returns a string that can be displayed to the user while reopt is running in
 * a given mode.
 * @param reoptMode - Mode reopt is running in
 * @returns User-friendly string description
 */
function getTitleForReoptMode(
    reoptMode: reopt.ReoptMode,
): string {
    switch (reoptMode) {
        case reopt.ReoptMode.GenerateCFG: return 'Generating CFG...'
        // case reopt.ReoptMode.GenerateObject: return 'Generating disassembly...'
        case reopt.ReoptMode.GenerateFunctions: return 'Generating functions...'
        case reopt.ReoptMode.GenerateLLVM: return 'Generating LLVM...'
    }
}



/**
 * Contains the shared logic for calling reopt and updating the IDE state, for
 * all interesting modes in which we can run reopt.
 * @param context - VSCode extension context
 * @param diagnosticCollection - current diagnostic collection
 * @param replaceOutputChannel - cf. 'ReplaceOutputChannel'
 * @returns Given a reopt mode, returns a function that runs reopt in that mode,
 * and displays the results.
 */
function makeReoptGenerate(
    context: vscode.ExtensionContext,
    diagnosticCollection: vscode.DiagnosticCollection,
    replaceOutputChannel: ReplaceOutputChannel,
    webview: ActivityWebview,
): (reoptMode: reopt.ReoptMode) => void {

    return (reoptMode) => {

        vscode.window

            .withProgress(
                {
                    location: vscode.ProgressLocation.Notification,
                    title: getTitleForReoptMode(reoptMode),
                    cancellable: true,
                },
                // TODO: we can probably kill the child process upon cancellation
                (_progress, _token) => reopt.runReoptToGenerateFile(context, reoptMode)
            )

            .then(
                // on fulfilled
                (({ outputFile, eventsFile }) => {
                    openOutputFile(outputFile)
                    processEventsFile(context, diagnosticCollection, eventsFile)
                }),
                // on rejected
                makeDisplayError(replaceOutputChannel),
            )

            .then(() => {

                const projectConfiguration = getWorkspaceConfiguration(context)
                if (!projectConfiguration) { return }
                const projectName = projectConfiguration.name
                if (option.isNone(projectName)) { return }

                // Fake running `reopt-vcg` by looking for a JSON file with the
                // same name as the project.
                const reoptVCGOutputFilename = `${projectName.value}.json`
                const reoptVCGOutputFile = vscode.Uri.joinPath(context.extensionUri, 'out', 'webview-static', reoptVCGOutputFilename).fsPath
                if (fs.existsSync(reoptVCGOutputFile)) {

                    const reoptVCGOutput = JSON.parse(fs.readFileSync(reoptVCGOutputFile).toString())

                    webview.postMessage({
                        tag: E2W.reoptVCGOutput,
                        output: reoptVCGOutput,
                    } as E2W.ReoptVCGOutput)

                }

            })

    }

}


/**
 *
 * @param context - VSCode extension context
 * @param webview - Extension webview
 * @param diagnosticCollection - Current diagnostic collection
 * @param replaceConfigurationWatcher - While a reopt project is open, we have a
 * filesystem watcher for its project file.  When a different project is open,
 * we call this to replace the old watcher with a new one.
 * @param replaceOutputChannel - see [[ReplaceOutputChannel]]
 * @returns
 */
export function makeMessageHandler(
    context: vscode.ExtensionContext,
    webview: vscode.Webview,
    diagnosticCollection: vscode.DiagnosticCollection,
    _replaceConfigurationWatcher: (w?: vscode.FileSystemWatcher) => void,
    replaceOutputChannel: ReplaceOutputChannel,
): (m: W2E.ReoptVCGWebviewToExtension) => Promise<void> {

    const reoptGenerate = makeReoptGenerate(
        context,
        diagnosticCollection,
        replaceOutputChannel,
        webview,
    )

    return async (_message: W2E.ReoptVCGWebviewToExtension) => {

        // switch (message.tag) {

        //     // forces exhaustivity checking
        //     default: {
        //         const exhaustiveCheck: never = message
        //         throw new Error(`Unhandled color case: ${exhaustiveCheck}`)
        //     }

        // }

    }
}


/**
 * Reopt generates an events file when running.  This processes it, displaying
 * relevant information to the user at the end of a run: what errors happened in
 * the process and where/why.
 * @param context - VSCode extension context
 * @param diagnosticsCollection - Current diagnostics collection
 * @param eventsFile - The generated events file
 */
async function processEventsFile(
    context: vscode.ExtensionContext,
    diagnosticsCollection: vscode.DiagnosticCollection,
    eventsFile: fs.PathLike,
): Promise<void> {

    // We need the disassembly file to show errors.  This makes sure that it has
    // been generated.
    const disassemblyFile = await reopt.runReoptToGenerateDisassembly(context)

    reopt.displayReoptEventsAsDiagnostics(
        context,
        diagnosticsCollection,
        disassemblyFile,
        eventsFile,
    )

}
