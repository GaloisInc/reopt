import * as vscode from 'vscode'

interface LLVMFunction {
    name: string
    location: {
        line: number
    }
}

function* fakeLLVMParser(
    llvmContents: string,
): Generator<LLVMFunction> {

    for (const [index, element] of llvmContents.split('\n').entries()) {
        if (element.startsWith('declare') || element.startsWith('define')) {
            const suffix = element.slice(element.indexOf('@'))
            const functionName = suffix.slice(0, suffix.indexOf('('))
            yield { name: functionName, location: { line: index } }
        }
    }

}

export class ReoptNodeProvider implements vscode.TreeDataProvider<LLVMFunction>, vscode.TextDocumentContentProvider {

    getChildren(_e?: LLVMFunction): LLVMFunction[] {
        const currentDocument = vscode.window.activeTextEditor?.document
        if (
            currentDocument === undefined
            || currentDocument.languageId !== 'llvm'
        ) { return [] }
        return Array.from(fakeLLVMParser(currentDocument.getText()))
    }

    getTreeItem(e: LLVMFunction) {
        return new vscode.TreeItem(e.name)
    }

    provideTextDocumentContent(): vscode.ProviderResult<string> {
        const currentDocument = vscode.window.activeTextEditor?.document
        return currentDocument?.getText()
    }

}
