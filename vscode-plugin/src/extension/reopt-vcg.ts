import * as ChildProcess from 'child_process'
import * as fs from 'fs'
import * as path from 'path'
import * as readline from 'readline'

import * as vscode from 'vscode'

import * as Constants from '@shared/constants'
import { ReoptVCGEntry } from '@shared/interfaces'
import {
    getWorkspaceConfiguration,
} from '@shared/project-configuration'


export enum ReoptMode {
    GenerateCFG,
    // GenerateObject,
    GenerateFunctions,
    GenerateLLVM,
}


export function processReoptVCGOutput(
    jsonsFile: fs.PathLike,
    callback: (output: ReoptVCGEntry[]) => void,
): void {

    const lineReader = readline.createInterface(
        fs.createReadStream(jsonsFile)
    )

    const entries = [] as ReoptVCGEntry[]

    lineReader.on('line', async (line) => {
        entries.push(JSON.parse(line))
    })

    lineReader.on('close', () => callback(entries))

}


export function runReoptVCG(
    context: vscode.ExtensionContext,
    params: {
        annotationsFile: string
        jsonsFile: string
    }
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

    const reoptVCGKey = Constants.reoptVCGExecutableKey

    const reoptVCGExecutable = workspaceConfiguration.get<string>(reoptVCGKey)

    if (reoptVCGExecutable === undefined) {
        vscode.window.showErrorMessage(
            `Could not find key ${reoptVCGKey} in your configuration, aborting.`
        )
        return Promise.reject()
    }

    return new Promise((resolve, _reject) => {

        ChildProcess.execFile(

            reoptVCGExecutable,

            [
                params.annotationsFile,
                '--json-goals', params.jsonsFile,
            ],

            {
                cwd: path.dirname(projectConfiguration.binaryFile),
            },

            (err, stdout, stderr) => {

                // Currently, reopt-vcg returns a bad status code if any VCG
                // failed, so we accept failures.
                if (err) {
                    vscode.window.showErrorMessage(err.message)
                    // if (err.code) {
                    //     console.log(`reopt-vcg return exit code: ${err.code}`)
                    // } else if (err.killed) {
                    //     console.log('reopt-vcg was killed')
                    // }
                }

                resolve([stdout, stderr])

            },

        )

    })

}
