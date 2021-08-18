import * as ChildProcess from 'child_process'
import { getOrElse } from 'fp-ts/Option'
import * as fs from 'fs'
import * as os from 'os'
import * as path from 'path'
import * as readline from 'readline'
import * as vscode from 'vscode'

import * as Constants from '@shared/constants'
import { DisassemblyLineInformation } from '@shared/interfaces'
import {
    getWorkspaceConfiguration,
    ProjectConfiguration,
} from '@shared/project-configuration'

import {
    getWorkspaceAddresses,
    populateAddresses,
} from './workspace-addresses'


export enum ReoptMode {
    GenerateCFG,
    // GenerateObject,
    GenerateFunctions,
    GenerateLLVM,
}


function replaceExtensionWith(replacement: string): (exe: string) => string {
    return (executablePath: string) => {
        const dir = path.dirname(executablePath)
        const executableWithoutExtension = path.basename(
            path.basename(executablePath),
            path.extname(executablePath),
        )
        return `${dir}/${executableWithoutExtension}.${replacement}`
    }
}


const replaceExtensionWithCFG = replaceExtensionWith('cfg')
// const replaceExtensionWithDis = replaceExtensionWith('dis')
const replaceExtensionWithFns = replaceExtensionWith('fns')
const replaceExtensionWithLL = replaceExtensionWith('ll')


function outputForReoptMode(
    projectConfiguration: ProjectConfiguration,
    reoptMode: ReoptMode,
): string {

    switch (reoptMode) {

        case ReoptMode.GenerateCFG: {
            return (
                getOrElse
                    (() => replaceExtensionWithCFG(projectConfiguration.binaryFile))
                    (projectConfiguration.outputNameCFG)
            )
        }

        // case ReoptMode.GenerateObject: {
        //     return (
        //         getOrElse
        //             (() => replaceExtensionWithDis(projectConfiguration.binaryFile))
        //             (projectConfiguration.outputNameDisassemble)
        //     )
        // }

        case ReoptMode.GenerateFunctions: {
            return (
                getOrElse
                    (() => replaceExtensionWithFns(projectConfiguration.binaryFile))
                    (projectConfiguration.outputNameFunctions)
            )
        }

        case ReoptMode.GenerateLLVM: {
            return (
                getOrElse
                    (() => replaceExtensionWithLL(projectConfiguration.binaryFile))
                    (projectConfiguration.outputNameLLVM)
            )
        }

    }

}


function argsForReoptMode(
    projectConfiguration: ProjectConfiguration,
    reoptMode: ReoptMode,
): [string, string] {

    const output = outputForReoptMode(projectConfiguration, reoptMode)

    switch (reoptMode) {

        case ReoptMode.GenerateCFG: {
            return ['export-cfg', output]
        }

        // case ReoptMode.GenerateObject: {
        //     return [`--export-object=${output}`]
        // }

        case ReoptMode.GenerateFunctions: {
            return ['export-fns', output]
        }

        case ReoptMode.GenerateLLVM: {
            return ['export-llvm', output]
        }

    }

}


// TODO: get that schema from the Haskell side
type ReoptError =
    [
        number,
        number,
        number,
        string,
    ]


/**
 * Goes through the list of events obtained from a run of reopt, and displays
 * the errors as VSCode diagnostics over the disassembly file.
 * @param context - VSCode extension context
 * @param diagnosticCollection - Current diagnostic collection
 * @param disassemblyFile - Path to the disassembly file (assumed to have been
 * created)
 * @param eventsFile - Path to the events file (assumed to have been created)
 */
export async function displayReoptEventsAsDiagnostics(
    context: vscode.ExtensionContext,
    diagnosticCollection: vscode.DiagnosticCollection,
    disassemblyFile: fs.PathLike,
    eventsFile: fs.PathLike,
): Promise<void> {

    const addresses = await getOrGenerateAddresses(context) || []

    const disassemblyUri = vscode.Uri.file(disassemblyFile.toLocaleString())

    const textDocument = await vscode.workspace.openTextDocument(disassemblyUri)
    const textEditor = await vscode.window.showTextDocument(textDocument, {
        preview: false,
    })

    const makeDiagnostic = makeDiagnosticBuilderForAddresses(addresses)

    // Highlight for the bytes in blocks that caused errors
    const bytesDecoration = vscode.window.createTextEditorDecorationType({
        backgroundColor: "#45383F",
        textDecoration: "#FF6464 wavy underline",
    })

    const lineReader = readline.createInterface(
        fs.createReadStream(eventsFile)
    )

    // The API for processing a file line-by-line is not of a very functional
    // nature, so we will populate a mutable array of ranges, and register all
    // those when reaching the end of file.
    const diagnostics = [] as vscode.Diagnostic[]
    let allBytesRanges = [] as vscode.Range[]
    lineReader.on('line', async (line) => {
        const { diagnostic, bytesRanges } = await makeDiagnostic(line)
        diagnostics.push(diagnostic)
        allBytesRanges = allBytesRanges.concat(bytesRanges)
    })

    lineReader.on('close', () => {
        // Currently, diagnostics are created for a disassembly file that has
        // been created in a temporary directory, fresh for each new run of
        // reopt.  As a result, the current [[disassemblyUri]] is not the same
        // as the one for the previous batch of diagnostics, so
        // [[diagnosticCollection.set]] will not overwrite the old ones.  So we
        // clear the collection instead.
        diagnosticCollection.clear()
        diagnosticCollection.set(
            disassemblyUri,
            diagnostics,
        )
        textEditor.setDecorations(bytesDecoration, allBytesRanges)
    })

}


/**
 * Runs reopt in one of the supported modes and returns the absolute path to the
 * output file generated, if successful.
 * @param context - VSCode extension context
 * @param extraArgs - TODO
 * @param reoptMode - What mode to run reopt in
 * @returns Standard output, standard error
 */
export function runReopt(
    context: vscode.ExtensionContext,
    extraArgs: string[],
): Promise<[string, string]> {

    const workspaceConfiguration = vscode.workspace.getConfiguration(
        Constants.reoptSectionKey,
    )

    const projectConfiguration = getWorkspaceConfiguration(context)

    if (projectConfiguration === undefined) {
        vscode.window.showErrorMessage(
            'Could not find a project configuration, have you opened a reopt project?'
        )
        return Promise.reject()
    }

    const reoptExecutable = workspaceConfiguration.get<string>(
        Constants.reoptExecutableKey,
    )

    if (reoptExecutable === undefined) {
        vscode.window.showErrorMessage(
            `Could not find key ${Constants.reoptExecutableKey} in your configuration, aborting.`
        )
        return Promise.reject()
    }

    return new Promise(async (resolve, reject) => {

        ChildProcess.execFile(

            reoptExecutable,

            ([] as string[]).concat(
                projectConfiguration.annotations.map(a => `--annotations=${a}`),
                projectConfiguration.excludes.map(e => `--exclude=${e}`),
                projectConfiguration.headers.map(h => `--header=${h}`),
                projectConfiguration.includes.map(i => `--include=${i}`),
                extraArgs,
                [
                    projectConfiguration.binaryFile,
                ],
            ),

            {
                cwd: path.dirname(projectConfiguration.binaryFile),
            },

            (err, stdout, stderr) => {

                // error events are now reported off-band, not sure this still
                // happens
                if (err) {
                    return reject(err)
                }

                resolve([stdout, stderr])

            },

        )

    })

}


export async function runReoptToGenerateDisassembly(
    context: vscode.ExtensionContext,
): Promise<string> {
    const tempDir = await fs.promises.mkdtemp(path.join(os.tmpdir(), 'temp-crux-llvm-'))
    const disassemblyFile = `${tempDir}/disassemble`

    await runReopt(context, [
        '--disassemble',
        `--output=${disassemblyFile}`,
    ])

    populateAddresses(context, disassemblyFile)

    return disassemblyFile
}


export async function getProjectConfiguration(
    context: vscode.ExtensionContext,
): Promise<ProjectConfiguration> {

    const projectConfiguration = getWorkspaceConfiguration(context)

    if (projectConfiguration === undefined) {
        vscode.window.showErrorMessage(
            'Could not find a project configuration, have you opened a reopt project?'
        )
        return Promise.reject()
    }

    return projectConfiguration

}


export async function runReoptToGenerateFile(
    context: vscode.ExtensionContext,
    reoptMode: ReoptMode,
): Promise<{ outputFile: string, eventsFile: string }> {
    const tempDir = await fs.promises.mkdtemp(path.join(os.tmpdir(), 'temp-crux-llvm-'))
    const eventsFile = `${tempDir}/events`
    // const outputFile = `${tempDir}/disassemble`

    const projectConfiguration = await getProjectConfiguration(context)

    const [flag, outputFile] = argsForReoptMode(projectConfiguration, reoptMode)
    await runReopt(context, [
        `--${flag}=${outputFile}`,
        `--export-events=${eventsFile}`,
    ])

    return { outputFile, eventsFile }
}


// TODO: I would like this to be generated on the Haskell side using
// aeson-typescript.
type ReoptCFGError = [number, number, number, number, string, string]


type DiagnosticAndBytesRanges = {
    diagnostic: vscode.Diagnostic
    bytesRanges: vscode.Range[]
}


/**
 * Given the information for addresses in the disassembly file, returns a
 * function that consumes a line of the reopt event log, and constructs the
 * corresponding diagnostic, alongside the byte ranges that the diagnostic
 * applies to.
 * The ranges correspond to chunks of lines in the disassembly file that contain
 * the bytes of the block, so that we can highlight the problematic bytes.
 * @param addresses - Information about addresses in the current disassembly
 * file.
 * @returns A function that, given a line of the reopt event log, constructs the
 * diagnostic and byte ranges.
 */
function makeDiagnosticBuilderForAddresses(
    addresses: DisassemblyLineInformation[],
): (line: string) => Promise<DiagnosticAndBytesRanges> {
    return async (line) => {
        const [funId, blockId, _instrIndex, blockSize, which, message] = JSON.parse(line) as ReoptCFGError
        const address = blockId.toString(16)
        const zero = new vscode.Position(0, 0)
        const zeroRange = new vscode.Range(zero, zero)

        // Look for the line information corresponding to this address
        const addressDisassemblyLineInfo = addresses.find(a => a.address === address)

        const range = addressDisassemblyLineInfo?.addressRange || zeroRange

        const diagnostic = new vscode.Diagnostic(
            range,
            `${which}: ${message}`,
            vscode.DiagnosticSeverity.Error,
        )

        /**
         * The error comes with a block (defined by its first byte and block
         * size).  We find all of our registered address ranges from the
         * disassembly file that are entirely contained in the block, so that
         * they can be highlighted.
         */

        const blockFirstByte = BigInt(blockId)
        const blockLastByte = BigInt(blockId) + BigInt(blockSize) - BigInt(1)

        const bytesRanges = addresses
            .filter(a =>
                blockFirstByte <= a.firstByteAddress
                && a.lastByteAddress <= blockLastByte
            )
            .map(a => a.bytesRange)

        return {
            diagnostic,
            bytesRanges,
        }

    }
}


/**
 * We often need information about the addresses that appear in the disassembly
 * file of an executable.  We try to compute this information only once.  This
 * function returns it, if it exists, otherwise it first generates and registers
 * it.
 * @param context - VSCode extension context.
 * @returns Information about the addresses in the reopt disassembly file, if
 * any.
 */
async function getOrGenerateAddresses(
    context: vscode.ExtensionContext,
): Promise<DisassemblyLineInformation[] | undefined> {
    const addresses = getWorkspaceAddresses(context)
    if (addresses !== undefined) {
        return Promise.resolve(addresses)
    }
    /**
     * If addresses is undefined, we attempts to set it by running reopt in
     * generate disassembly mode.
     */
    await runReoptToGenerateDisassembly(context)
    return getWorkspaceAddresses(context)
}
