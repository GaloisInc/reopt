import * as ChildProcess from 'child_process'
import { getOrElse } from 'fp-ts/Option'
import * as fs from 'fs'
import * as path from 'path'
import * as vscode from 'vscode'

import * as Constants from '@shared/constants'
import {
    getWorkspaceConfiguration,
    ProjectConfiguration,
} from '@shared/project-configuration'

import {
    LocatedAddress,
    getWorkspaceAddresses,
    setWorkspaceAddresses,
    clearWorkspaceAddresses,
} from './workspace-addresses'

export enum ReoptMode {
    GenerateCFG,
    GenerateDisassembly,
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
const replaceExtensionWithDis = replaceExtensionWith('dis')
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

        case ReoptMode.GenerateDisassembly: {
            return (
                getOrElse
                    (() => replaceExtensionWithDis(projectConfiguration.binaryFile))
                    (projectConfiguration.outputNameDisassemble)
            )
        }

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
): string[] {

    const output = outputForReoptMode(projectConfiguration, reoptMode)

    switch (reoptMode) {

        case ReoptMode.GenerateCFG: {
            return ['--cfg', `--output=${output}`]
        }

        case ReoptMode.GenerateDisassembly: {
            return ['--disassemble', `--output=${output}`]
        }

        case ReoptMode.GenerateFunctions: {
            return ['--functions', `--output=${output}`]
        }

        case ReoptMode.GenerateLLVM: {
            return ['--llvm', `--output=${output}`]
        }

    }

}


/**
 * Runs reopt in one of the supported modes and returns the absolute path to the
 * output file generated, if successful.
 * @param context - VSCode extension context
 * @param reoptMode - What mode to run reopt in
 * @returns Absolute path to the output file generated, if successful.
 */
export function runReopt(
    context: vscode.ExtensionContext,
    diagnosticCollection: vscode.DiagnosticCollection,
    reoptMode: ReoptMode,
): Promise<string> {

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
                projectConfiguration.excludes.map(e => `--exclude=${e}`),
                projectConfiguration.headers.map(h => `--header=${h}`),
                projectConfiguration.includes.map(i => `--include=${i}`),
                argsForReoptMode(projectConfiguration, reoptMode),
                [
                    projectConfiguration.binaryFile,
                ],
            ),

            {
                cwd: path.dirname(projectConfiguration.binaryFile),
            },

            (err, _stdout, stderr) => {

                if (err) {
                    reportErrorsAsDiagnostics(
                        context,
                        projectConfiguration,
                        diagnosticCollection,
                        reoptMode,
                        stderr,
                    )
                    return reject(err)
                }

                const output = outputForReoptMode(projectConfiguration, reoptMode)

                const absoluteOutput = (
                    path.isAbsolute(output)
                        ? output
                        : path.resolve(path.dirname(projectConfiguration.binaryFile), output)
                )

                if (reoptMode === ReoptMode.GenerateDisassembly) {
                    populateAddresses(context, absoluteOutput)
                }

                return resolve(absoluteOutput)

            },

        )
    })

}


function makeReoptDiagnostic(
    addresses: LocatedAddress[],
): (line: string) => vscode.Diagnostic {
    return (line) => {
        const zero = new vscode.Position(0, 0)
        const zeroRange = new vscode.Range(zero, zero)
        const suffix = line.replace('  Failed: ', '')

        if (suffix.startsWith('0x')) {
            const colon = suffix.indexOf(':')
            const address = suffix.slice(2, colon)
            const message = suffix.slice(colon + 1)

            const addressLocation = addresses.find(a => a.address === address)
            const range = addressLocation?.range || zeroRange

            return new vscode.Diagnostic(range, message, vscode.DiagnosticSeverity.Error)
        } else {
            return new vscode.Diagnostic(zeroRange, suffix, vscode.DiagnosticSeverity.Error)
        }

    }
}


function reoptErrorsAsDiagnostics(
    addresses: LocatedAddress[],
    stderr: string,
): vscode.Diagnostic[] {
    return (
        stderr
            .split('\n')
            .filter(line => line.startsWith('  Failed:'))
            .map(makeReoptDiagnostic(addresses))
    )
}


async function getOrGenerateAddresses(
    context: vscode.ExtensionContext,
    diagnosticCollection: vscode.DiagnosticCollection,
): Promise<LocatedAddress[] | undefined> {
    const addresses = getWorkspaceAddresses(context)
    if (addresses !== undefined) {
        return Promise.resolve(addresses)
    }
    // If addresses is undefined, we attempts to set it by running reopt in
    // generate disassembly mode.
    await runReopt(context, diagnosticCollection, ReoptMode.GenerateDisassembly)
    return getWorkspaceAddresses(context)
}


async function reportErrorsAsDiagnostics(
    context: vscode.ExtensionContext,
    projectConfiguration: ProjectConfiguration,
    diagnosticCollection: vscode.DiagnosticCollection,
    reoptMode: ReoptMode,
    stderr: string,
): Promise<void> {

    if (reoptMode === ReoptMode.GenerateDisassembly) {
        // We report errors as diagnostics located in the disassembly file, so
        // it does not make sense to do so when generating the disassembly file
        // raises errors.
        return
    }

    const disassemblyFile = outputForReoptMode(
        projectConfiguration,
        ReoptMode.GenerateDisassembly,
    )
    const disassemblyUri = vscode.Uri.file(disassemblyFile)

    clearWorkspaceAddresses(context) // debug
    const addresses = await getOrGenerateAddresses(context, diagnosticCollection) || []

    diagnosticCollection.set(
        disassemblyUri,
        reoptErrorsAsDiagnostics(addresses, stderr),
    )

}


/**
 * Populate the workspace state with the list of addresses from the output of
 * reopt in disassembly mode, and their location in the disassembly file.  This
 * is then used for locating errors when reporting diagnostics for other runs of
 * reopt.
 * @param stdout
 */
function populateAddresses(
    context: vscode.ExtensionContext,
    disassemblyFilePath: fs.PathLike,
): void {
    // this could be made more efficient if necessary
    const output = fs.readFileSync(disassemblyFilePath).toString()
    const addresses = (
        Array
            .from(output.split('\n').entries())
            .filter(entry => entry[1].startsWith('  '))
            .map((entry) => {
                const [lineNumber, line] = entry
                const start = 2
                const end = line.indexOf(':')
                return {
                    address: line.slice(start, end),
                    range: new vscode.Range(
                        new vscode.Position(lineNumber, 2),
                        new vscode.Position(lineNumber, end),
                    )
                }
            })
    )
    setWorkspaceAddresses(context, addresses)
}
