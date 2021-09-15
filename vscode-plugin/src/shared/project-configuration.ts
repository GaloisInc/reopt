import { option } from 'fp-ts'
import { Either, mapLeft } from 'fp-ts/Either'
import * as ty from 'io-ts'
import { ValidationError } from 'io-ts'
import { JsonFromString, optionFromNullable } from 'io-ts-types'
import { failure } from 'io-ts/lib/PathReporter'


/**
 * Make sure this schema corresponds to rpj-schema.json.  Ideally it would be
 * derived from it.
 */
export const ProjectConfiguration = ty.type({
    annotations: optionFromNullable(ty.string),
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
    annotations: option.none,
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

${failure(errors).map(s => `- ${s}`).join('\n')}
`
}


const ConfigurationFromString = JsonFromString.pipe(ProjectConfiguration)


export function decodeConfiguration(s: string): Either<string, ProjectConfiguration> {
    return (
        mapLeft(showValidationErrors(s))(ConfigurationFromString.decode(s))
    )
}
