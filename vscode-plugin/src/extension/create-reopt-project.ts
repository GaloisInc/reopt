import { option } from 'fp-ts'
import * as fs from 'fs'
import * as path from 'path'
import * as vscode from 'vscode'

import * as Configuration from '@shared/project-configuration'
import * as Promisified from '@shared/promisified'

import { checkMagicNumber } from './check-magic-number'


export async function createReoptProject(
    uri?: vscode.Uri,
): Promise<string> {

    const executableFile = await getExecutableFile(uri)

    const isELF = await checkMagicNumber(executableFile)

    if (!isELF) {
        vscode.window.showErrorMessage(
            `This does not look like an ELF executable file, please check. ${executableFile}`
        )
        return Promise.reject()
    }

    const directory = path.dirname(executableFile)

    await Promisified.stat(directory) // this will error if the directory does not exist

    const extension = path.extname(executableFile)
    const executableNameWithoutExtension = path.basename(executableFile, extension)

    const projectFilePath = path.join(directory, `${executableNameWithoutExtension}.rpj`)

    if (fs.existsSync(projectFilePath)) {

        vscode.window.showInformationMessage(
            `Project file already exists, opening: ${projectFilePath}`
        )

    } else {

        const configuration = Object.assign(
            {},
            Configuration.defaultConfiguration,
            {
                binaryFile: executableFile,
                name: option.some(executableNameWithoutExtension),
            } as Configuration.ProjectConfiguration,
        )

        await Promisified.writeFile(
            projectFilePath,
            JSON.stringify(
                Configuration.ProjectConfiguration.encode(configuration),
                null,
                4, // indentation in number of spaces
            ),
        )

        vscode.window.showInformationMessage('Project file created')

    }

    // also open the project file as a text document
    const doc = await vscode.workspace.openTextDocument(projectFilePath)
    await vscode.window.showTextDocument(doc)

    return projectFilePath

}


async function getExecutableFile(
    uri?: vscode.Uri,
): Promise<string> {
    if (uri) {
        return uri.fsPath
    } else {
        const files = await vscode.window.showOpenDialog({
            openLabel: 'Create reopt project for this executable',
            title: 'Choose executable to use for new reopt project',
        })
        if (files === undefined || files.length !== 1) {
            return Promise.reject()
        }
        return files[0].fsPath
    }
}
