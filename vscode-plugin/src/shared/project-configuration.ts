import { option } from 'fp-ts'
import { Either, mapLeft } from 'fp-ts/Either'
import * as ty from 'io-ts'
import { ValidationError } from 'io-ts'
import { JsonFromString, optionFromNullable } from 'io-ts-types'
import { failure } from 'io-ts/lib/PathReporter'
import * as vscode from 'vscode'


const configurationStateKey = 'reopt-configuration'
export function clearWorkspaceConfiguration(
    context: vscode.ExtensionContext,
): void {
    context.workspaceState.update(configurationStateKey, undefined)
}
export function getWorkspaceConfiguration(
    context: vscode.ExtensionContext,
): ProjectConfiguration | undefined {
    return context.workspaceState.get(configurationStateKey)
}
export function setWorkspaceConfiguration(
    context: vscode.ExtensionContext,
    configuration: ProjectConfiguration,
): void {
    context.workspaceState.update(configurationStateKey, configuration)
}


const projectFileStateKey = 'reopt-project'
export function clearWorkspaceProjectFile(
    context: vscode.ExtensionContext,
): void {
    context.workspaceState.update(projectFileStateKey, undefined)
}
export function getWorkspaceProjectFile(
    context: vscode.ExtensionContext,
): string | undefined {
    return context.workspaceState.get(projectFileStateKey)
}
export function setWorkspaceProjectFile(
    context: vscode.ExtensionContext,
    projectFile: string,
): void {
    context.workspaceState.update(projectFileStateKey, projectFile)
}


export const ProjectConfiguration = ty.type({
    binaryFile: ty.string,
    excludes: ty.array(ty.string),
    headers: ty.array(ty.string),
    includes: ty.array(ty.string),
    name: optionFromNullable(ty.string),
    outputNameCFG: optionFromNullable(ty.string),
    outputNameDisassemble: optionFromNullable(ty.string),
    outputNameFunctions: optionFromNullable(ty.string),
    outputNameLLVM: optionFromNullable(ty.string),
})


export type ProjectConfiguration = ty.TypeOf<typeof ProjectConfiguration>


export const defaultConfiguration: ProjectConfiguration = {
    // This field will be instantiated by the code that creates the
    // configuration file
    binaryFile: '<name of your executable>',
    excludes: [],
    headers: [],
    includes: [],
    name: option.none,
    outputNameCFG: option.none,
    outputNameDisassemble: option.none,
    outputNameFunctions: option.none,
    outputNameLLVM: option.none,
}


function showValidationErrors(
    input: string,
): (errors: ValidationError[]) => string {
    return (errors: ValidationError[]) => `Validation failed for input:

${JSON.stringify(input, null, 2)}

Error details:

${failure(errors).map(s => '- ' + s).join('\n')}
`
}


const ConfigurationFromString = JsonFromString.pipe(ProjectConfiguration)


export function decodeConfiguration(s: string): Either<string, ProjectConfiguration> {
    return (
        mapLeft(showValidationErrors(s))(ConfigurationFromString.decode(s))
    )
}
