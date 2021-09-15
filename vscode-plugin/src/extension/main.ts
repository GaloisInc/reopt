import * as vscode from 'vscode'

import * as Constants from '@shared/constants'
import { ReoptVCGWebview } from '@shared/interfaces'

import { ActivityViewProvider } from './activity-view-provider'
import { createReoptProject } from './create-reopt-project'
import { ReoptVCGViewProvider } from './reopt-vcg-view-provider'
import { LLVMDocumentSymbolProvider } from './symbol-provider'


// class ReoptViewer implements vscode.CustomTextEditorProvider {

//     static readonly viewType = 'reopt.project'

//     constructor(private chan: vscode.OutputChannel) {

//     }

//     /**
//      * Resolve a custom editor for a given text resource.
//      *
//      * This is called when a user first opens a resource for a `CustomTextEditorProvider`, or if they reopen an
//      * existing editor using this `CustomTextEditorProvider`.
//      */
//     resolveCustomTextEditor(document: vscode.TextDocument, webviewPanel: vscode.WebviewPanel, _token: vscode.CancellationToken): void {
//         const contents = document.getText()

//         try {
//             const prj = JSON.parse(contents)
//             this.chan.appendLine(prj === null ? 'null' : 'not null')
//             this.chan.appendLine(prj === undefined ? 'undefined' : 'not undefined')
//             this.chan.appendLine(prj ? typeof prj : 'null')
//         } catch (e) {
//             if (!(e instanceof SyntaxError)) { throw e }

//             const se = e as any


//             this.chan.appendLine(`line: ${JSON.stringify(e)}`)
//             this.chan.appendLine(`col: ${e.stack}`)
//             this.chan.appendLine(`message: ${e.message}`)
//         }


//         const webview = webviewPanel.webview
//         const webviewProto = webview.cspSource
//         webview.html = (`
// <!DOCTYPE html>
// <html lang="en">
// <head>
//     <meta charset="UTF-8">
//     <meta http-equiv="Content-Security-Policy" content="default-src ${webviewProto}; style-src ${webviewProto} 'unsafe-inline'"/>
//     <meta name="viewport" content="width=device-width, initial-scale=1.0">
//     <title>Reopt Groups</title>
// </head>
// <body>
// <div>Hello World</div>
// </body>
// </html>
//         `)
//     }
// }


class Extension {

    constructor(context: vscode.ExtensionContext) {

        const subscribe = (d: vscode.Disposable) => context.subscriptions.push(d)

        const registerCommand = (
            command: Constants.PackageCommands,
            handler: (uri: vscode.Uri) => void,
        ) => {
            subscribe(vscode.commands.registerCommand(command, handler))
        }

        // Register our custom editor providers
        const c = vscode.window.createOutputChannel('reopt')
        subscribe(c)

        registerCommand(Constants.createProjectCommand, createReoptProject)

        // registerCommand('reopt.viewFile', (uri:vscode.Uri) => this.decompileFile(uri))

        // vscode.window.registerCustomEditorProvider(ReoptViewer.viewType, new ReoptViewer(c))

        // vscode.window.registerTreeDataProvider('reopt-view', new ReoptNodeProvider())

        subscribe(vscode.languages.registerDocumentSymbolProvider(
            { language: 'llvm' },
            new LLVMDocumentSymbolProvider(),
        ))

        // const treeDataProvider = new ReoptNodeProvider()
        // context.subscriptions.push(vscode.workspace.registerTextDocumentContentProvider('ftp', treeDataProvider))
        // const tree = vscode.window.createTreeView('reopt-activity-view', { treeDataProvider })

        /* We need the activity webview to tell the reopt-vcg webview when to
        start working. This is quite ugly, should be made better later. */
        const reoptVCGWebviewPromise = new Promise<ReoptVCGWebview>(resolve => {
            subscribe(
                vscode.window.registerWebviewViewProvider(
                    ReoptVCGViewProvider.viewType,
                    new ReoptVCGViewProvider(context, resolve),
                ),
            )
        })

        subscribe(
            vscode.window.registerWebviewViewProvider(
                ActivityViewProvider.viewType,
                new ActivityViewProvider(context, reoptVCGWebviewPromise),
            ),
        )

    }

}

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext): void {
    new Extension(context)
}


/**
 * This is called when the extension is deactivated.  It should dispose of all
 * held resources.
 * @returns Nothing
 */
export function deactivate(): void {
    // If we're being diligent in registering subscriptions in the extension, we
    // have no additional cleanup here.
    return
}
