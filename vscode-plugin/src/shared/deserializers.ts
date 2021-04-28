import * as vscode from 'vscode'

import * as Interfaces from './interfaces'

export function deserializeSymbolInformation(
    symbol: Interfaces.SerializedSymbolInformation,
): vscode.SymbolInformation {

    const { range } = symbol
    const { end, start } = range

    return new vscode.SymbolInformation(
        symbol.name,
        symbol.kind,
        symbol.containerName,
        new vscode.Location(
            vscode.Uri.file(symbol.fsPath),
            new vscode.Range(
                new vscode.Position(start.line, start.character),
                new vscode.Position(end.line, end.character),
            )
        )
    )

}
