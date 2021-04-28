import * as vscode from 'vscode'

function* lineIterator(
    document: vscode.TextDocument,
): Generator<vscode.TextLine> {
    for (let i = 0; i < document.lineCount; i++) {
        yield document.lineAt(i)
    }
}

function inspectLineForSymbol(
    document: vscode.TextDocument,
    line: vscode.TextLine,
): vscode.SymbolInformation[] {

    const text = line.text

    if (text.startsWith('declare') || text.startsWith('define')) {
        const atSignIndex = text.indexOf('@')
        const suffix = text.slice(atSignIndex)
        const parenIndex = suffix.indexOf('(')
        const functionName = suffix.slice(0, parenIndex)

        const start = new vscode.Position(
            line.range.start.line,
            atSignIndex,
        )

        const end = new vscode.Position(
            line.range.end.line,
            atSignIndex + parenIndex,
        )

        return [
            new vscode.SymbolInformation(
                functionName,
                vscode.SymbolKind.Function,
                '',
                new vscode.Location(document.uri, new vscode.Range(start, end))
            )
        ]

    }

    return []

}

export class LLVMDocumentSymbolProvider implements vscode.DocumentSymbolProvider {

    provideDocumentSymbols(
        document: vscode.TextDocument,
        _token?: vscode.CancellationToken,
    ): vscode.SymbolInformation[] {

        return (
            Array.from(lineIterator(document)).flatMap(
                (line) => inspectLineForSymbol(document, line)
            )
        )
    }

}
