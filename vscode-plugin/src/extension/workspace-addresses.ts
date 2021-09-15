/**
 * This module provides helpers for creating a mapping from program addresses to
 * text ranges in a disassembly file, so that errors reported in terms of
 * program addresses can be displayed at the appropriate location in the editor.
 *
 * We try and compute this mapping exactly once per run of reopt in disassembly
 * mode, so we register the mapping in the workspace and try to retrieve it from
 * there if available.
 */

import * as fs from 'fs'

import * as vscode from 'vscode'

import * as Deserializers from '@shared/deserializers'
import { DisassemblyLineInformation, SerializationOf } from '@shared/interfaces'
import * as Serializers from '@shared/serializers'


const addressesStateKey = 'reopt-addresses' // do NOT export
type AddressesStateType = SerializationOf<DisassemblyLineInformation>[]


export function clearWorkspaceAddresses(
    context: vscode.ExtensionContext,
): void {
    context.workspaceState.update(addressesStateKey, undefined)
}


export function getWorkspaceAddresses(
    context: vscode.ExtensionContext,
): DisassemblyLineInformation[] | undefined {
    const addresses
        = context.workspaceState.get<AddressesStateType>(addressesStateKey)
    return addresses?.map(Deserializers.deserializeDisassemblyLineInformation)
}


export function setWorkspaceAddresses(
    context: vscode.ExtensionContext,
    addresses: DisassemblyLineInformation[],
): void {
    context.workspaceState.update(
        addressesStateKey,
        addresses.map(Serializers.serializeDisassemblyLineInformation)
    )
}


/**
 * Populate the workspace state with the list of addresses from the output of
 * reopt in disassembly mode, and their location in the disassembly file.  This
 * is then used for locating errors when reporting diagnostics for other runs of
 * reopt.
 * @param context - VSCode extension context
 * @param disassemblyFilePath - Path to the last generated disassembly file
 */
export function populateAddresses(
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

                /*
                 * A line follows the following structure:
                 * - two spaces (using underscore here for readability)             _{2}
                 * - a hex address, capture group 1:                                ([0-9a-f]+)
                 * - a colon, followed by a tab                                     :\t
                 * - a bunch of hex bytes, grouped 2 characters by 2, separated
                 *   by one space, capture group 2:                                 ((?:[0-9a-f][0-9a-f] ?)*[0-9a-f][0-9a-f])
                 * NOTE: we make sure the last byte is not followed by a space
                 * - if the bytes were null, the line ends, otherwise,
                 * - some amount of spaces (possibly zero) for alignment,           _*
                 * - a tab,                                                         \t?
                 * - and finally the instruction decoded, capture group 3.          (.*)
                 */
                const lineRE = / {2}([0-9a-f]+):\t((?:[0-9a-f][0-9a-f] ?)*[0-9a-f][0-9a-f]) *\t?(.*)/

                const [lineNumber, line] = entry

                const groups = line.match(lineRE)

                if (groups === null) {
                    console.log('CHECK')
                    throw line
                }

                const address = groups[1]
                const bytes = groups[2]
                const instruction = groups[3]

                const addressStart = 2
                const addressEnd = line.indexOf(':')

                const firstByteAddress = BigInt(`0x${address}`)
                const numberOfBytes = (bytes.length + 1) / 3
                // if there are 4 bytes, the last is at +3
                const lastByteAddress = firstByteAddress + BigInt(numberOfBytes) - BigInt(1)

                return {
                    address,
                    addressRange: new vscode.Range(
                        new vscode.Position(lineNumber, addressStart),
                        new vscode.Position(lineNumber, addressEnd),
                    ),
                    bytesRange: new vscode.Range(
                        new vscode.Position(lineNumber, 1 + line.indexOf('\t')),
                        new vscode.Position(lineNumber, 1 + line.indexOf('\t') + bytes.length),
                    ),
                    firstByteAddress,
                    instruction,
                    lastByteAddress,
                    wholeLineRange: new vscode.Range(
                        new vscode.Position(lineNumber, addressStart),
                        new vscode.Position(lineNumber, line.length),
                    ),
                }

            })
    )
    setWorkspaceAddresses(context, addresses)
}
