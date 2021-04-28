import * as vscode from 'vscode'

import * as Interfaces from './interfaces'

export function serializePosition(
    position: vscode.Position,
): Interfaces.SerializedPosition {
    // Looks stupid but just to be extra cautious
    const { character, line } = position
    return {
        character,
        line,
    }
}


export function serializeRange(
    range: vscode.Range,
): Interfaces.SerializedRange {
    const { end, start } = range
    return {
        end: serializePosition(end),
        start: serializePosition(start),
    }
}

export function serializeSymbolInformation(
    symbol: vscode.SymbolInformation,
): Interfaces.SerializedSymbolInformation {

    const { containerName, kind, location, name } = symbol
    const { range, uri } = location
    const { fsPath } = uri

    return {
        containerName,
        fsPath,
        kind,
        name,
        // WARNING: here the type-checker provides a false sense of safety in
        // terms of JSON seralizability, it will allow 'symbol.location.range'
        // as a value of type 'SerializedRange', even though a 'vscode.Range'
        // does not serialize correctly!
        range: serializeRange(range)
    }

}
