/**
 * Logic for handling messages coming from the activity webview.
*/

import { ExecFileException } from 'child_process'
import * as fs from 'fs'
import * as path from 'path'
import * as vscode from 'vscode'

import { option } from 'fp-ts'

import * as E2W from '@shared/extension-to-activity-webview'
import * as E2ReoptVCGW from '@shared/extension-to-reopt-vcg-webview'
import {
    clearWorkspaceConfiguration,
    clearWorkspaceProjectFile,
    getWorkspaceConfiguration,
    getWorkspaceProjectFile,
    ProjectConfiguration,
} from '@shared/project-configuration'
import * as W2E from '@shared/activity-webview-to-extension'

import { createReoptProject } from './create-reopt-project'
import {
    openReoptProject,
    openReoptProjectViaDialog,
} from './open-reopt-project'
import * as reopt from './reopt'
import * as reoptVCG from './reopt-vcg'
import { ActivityWebview, ReoptVCGEntry, ReoptVCGWebview } from '@shared/interfaces'
import { ReoptVCGViewProvider } from './reopt-vcg-view-provider'
import { InterfaceContributions } from 'mocha'
import { Tail } from 'tail'


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
        case reopt.ReoptMode.GenerateDisassembly: return 'Generating disassembly...'
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
    reoptVCGWebviewPromise: Promise<ReoptVCGWebview>,
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
                processOutputAndEvents(context, diagnosticCollection, reoptMode, reoptVCGWebviewPromise),
                // on rejected
                makeDisplayError(replaceOutputChannel),
            )

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
    activityWebview: ActivityWebview,
    diagnosticCollection: vscode.DiagnosticCollection,
    reoptVCGWebviewPromise: Promise<ReoptVCGWebview>,
    replaceConfigurationWatcher: (w?: vscode.FileSystemWatcher) => void,
    replaceOutputChannel: ReplaceOutputChannel,
): (m: W2E.ActivityWebviewToExtension) => Promise<void> {

    const reoptGenerate = makeReoptGenerate(
        context,
        diagnosticCollection,
        replaceOutputChannel,
        reoptVCGWebviewPromise,
    )

    return async (message: W2E.ActivityWebviewToExtension) => {

        switch (message.tag) {

            case W2E.closeProject: {
                clearWorkspaceConfiguration(context)
                clearWorkspaceProjectFile(context)
                activityWebview.postMessage({
                    tag: E2W.closedProjectTag,
                } as E2W.ClosedProject)
                replaceConfigurationWatcher(undefined)
                return
            }

            case W2E.createProjectFile: {
                const reoptProjectFile = await createReoptProject()
                const watcher = await openReoptProject(context, activityWebview, reoptProjectFile)
                replaceConfigurationWatcher(watcher)
                return
            }

            case W2E.generateCFG: {
                reoptGenerate(reopt.ReoptMode.GenerateCFG)
                return
            }

            // case W2E.generateDisassembly: {
            //     reoptGenerate(reopt.ReoptMode.GenerateObject)
            //     return
            // }

            case W2E.generateFunctions: {
                reoptGenerate(reopt.ReoptMode.GenerateFunctions)
                return
            }

            case W2E.generateLLVM: {
                reoptGenerate(reopt.ReoptMode.GenerateLLVM)
                return
            }

            case W2E.jumpToSymbol: {
                const { fsPath, range } = message.symbol

                const doc = await vscode.workspace.openTextDocument(fsPath)
                const editor = await vscode.window.showTextDocument(doc)
                const startPosition = new vscode.Position(range.start.line, range.start.character)
                const endPosition = new vscode.Position(range.end.line, range.end.character)

                // Move symbol into view and select it
                editor.selection = new vscode.Selection(startPosition, endPosition)
                editor.revealRange(
                    new vscode.Range(startPosition, endPosition),
                    vscode.TextEditorRevealType.AtTop,
                )

                return
            }

            case W2E.openProject: {
                const watcher = await openReoptProjectViaDialog(context, activityWebview)
                replaceConfigurationWatcher(watcher)
                return
            }

            case W2E.showProjectFile: {

                const projectFile = getWorkspaceProjectFile(context)
                if (projectFile === undefined) {
                    // this should not be possible, but just in case...
                    vscode.window.showErrorMessage(
                        'Could not show project file: please reopen the project.',
                    )
                    return
                }
                const doc = await vscode.workspace.openTextDocument(projectFile)
                const editor = await vscode.window.showTextDocument(doc)

                return
            }

            // forces exhaustivity checking
            default: {
                const exhaustiveCheck: never = message
                throw new Error(`Unhandled color case: ${exhaustiveCheck}`)
            }

        }
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


function processOutputAndEvents(
    context: vscode.ExtensionContext,
    diagnosticCollection: vscode.DiagnosticCollection,
    reoptMode: reopt.ReoptMode,
    reoptVCGWebviewPromise: Promise<ReoptVCGWebview>,
) {
    return async ({
        outputFile,
        eventsFile,
    }: { outputFile: string, eventsFile: string }) => {

        openOutputFile(outputFile)
        processEventsFile(context, diagnosticCollection, eventsFile)

        // Then, if the file was LLVM, we can run reopt-vcg
        if (reoptMode !== reopt.ReoptMode.GenerateLLVM) { return }

        const projectConfiguration = getWorkspaceConfiguration(context)
        if (!projectConfiguration) { return }
        const projectName = projectConfiguration.name
        const annotationsFile = projectConfiguration.annotations
        if (option.isNone(projectName)) { return }

        // Cannot run reopt-vcg if there is no annotations file
        if (option.isNone(annotationsFile)) {
            vscode.window.showErrorMessage(
                'Not running reopt-vcg because the configuration does not specify an annotations file.'
            )
            return
        }

        const workingDirectory = path.dirname(projectConfiguration.binaryFile)

        const resolvedAnnotationsFile = path.resolve(workingDirectory, annotationsFile.value)

        if (!fs.existsSync(resolvedAnnotationsFile)) {
            vscode.window.showErrorMessage(
                `Not running reopt-vcg because the annotations file does not exist: ${annotationsFile.value} resolved to ${resolvedAnnotationsFile}`
            )
            return
        }

        // We also need a handler to the reopt-vcg webview to send the results
        // to.
        // FIXME: I'm worried that if the user has never clicked on the panel
        // that shows the result, we will stay here forever.
        const webview = await reoptVCGWebviewPromise

        // Tell the webview to forget about pre-existing entries
        webview.postMessage({
            tag: E2ReoptVCGW.clearReoptVCGEntries,
        } as E2ReoptVCGW.ClearReoptVCGEntries)

        // FIXME: currently the progress stops when the JSONs file is returned, but
        // it should continue while the reopt-vcg process is running.
        const jsonsFile = await (
            vscode.window
                .withProgress(
                    {
                        location: vscode.ProgressLocation.Notification,
                        title: 'Running reopt-vcg',
                        cancellable: true,
                    },
                    // TODO: we can probably kill the child process upon cancellation
                    (_progress, _token) => reoptVCG.runReoptVCGToGenerateJSONs(
                        context,
                        annotationsFile.value,
                    )
                )
        )

        const resolvedJSONsFile = path.resolve(workingDirectory, jsonsFile)

        const tail = new Tail(resolvedJSONsFile, { fromBeginning: true })
        tail.on('line', line => {

            webview.postMessage({
                tag: E2ReoptVCGW.reoptVCGEntry,
                entry: JSON.parse(line),
            } as E2ReoptVCGW.ReoptVCGEntry)

        })

        // if (!fs.existsSync(resolvedJSONsFile)) {
        //     vscode.window.showErrorMessage(
        //         `Not displaying reopt-vcg results because JSONs file does not exist: ${jsonsFile}`
        //     )
        // }

        // reoptVCG.processReoptVCGOutput(
        //     resolvedJSONsFile,
        //     output => {
        //         console.log(`Read ${output.length} entries from JSONs file.`)
        //         webview.postMessage({
        //             tag: E2ReoptVCGW.reoptVCGOutput,
        //             output,
        //         } as E2ReoptVCGW.ReoptVCGOutput)

        //     },
        // )

        // Fake running `reopt-vcg` by looking for a JSON file with
        // the same name as the project.
        // const reoptVCGOutputFilename = `${projectName.value}.jsons`
        // const reoptVCGOutputFile = vscode.Uri.joinPath(context.extensionUri, 'out', 'webview-static', reoptVCGOutputFilename).fsPath
        // if (!fs.existsSync(reoptVCGOutputFile)) { return }

        // const reoptVCGOutput: ReoptVCGEntry[] = JSON.parse(fs.readFileSync(reoptVCGOutputFile).toString())

        // webview.postMessage({
        //     tag: E2ReoptVCGW.reoptVCGOutput,
        //     output: reoptVCGOutput,
        // } as E2ReoptVCGW.ReoptVCGOutput)

    }
}
