import PackageJSON from './package.json'

export const activityWebviewId = 'reopt-activity'

export const createProjectCommand = commandExistsOrFailLoudly('reopt.createProject')
export const jumpToFunctionCommand = commandExistsOrFailLoudly('reopt.jumpToFunction')

export type PackageCommands
    = typeof createProjectCommand
    | typeof jumpToFunctionCommand

/**
 * There does not seem to be a way to get strongly-typed JSON out of the JSON
 * module import, so this check is dynamic rather than static.  We make sure
 * that the commands we bound exist in the 'package.json'.
 *
 * This check could be done statically with something like:
 * https://github.com/tc39/proposal-import-assertions
 *
 * @param command Command to check for existence.
 * @returns The command if the check succeeded, throws otherwise.
 */
function commandExistsOrFailLoudly<Literal extends string>(
    command: Literal,
): Literal {
    const found = PackageJSON.contributes.commands.find(
        (c: { command: string }) => c.command === command
    )
    if (found === undefined) {
        throw new Error(`The following command does not exist in package.json: ${command}.  Please fix 'shared/constants.ts'.`)
    }
    return command
}


export const reoptSectionKey = 'reopt'

export const reoptExecutableKey = 'reoptExecutable'

/**
 * The following line makes it so that we get a static error if the property
 * changes in package.json and we don't reflect it here.
*/
PackageJSON.contributes.configuration.properties[
    `${reoptSectionKey}.${reoptExecutableKey}` as 'reopt.reoptExecutable'
]
