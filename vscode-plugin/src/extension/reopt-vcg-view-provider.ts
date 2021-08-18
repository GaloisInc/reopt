import { getOrElse } from 'fp-ts/Option'
import * as vscode from 'vscode'

import * as Constants from '@shared/constants'
import * as E2W from '@shared/extension-to-reopt-vcg-webview'

import * as ReoptVCGMessageHandler from './reopt-vcg-message-handler'
import { getNonce } from './nonce'
import { LLVMDocumentSymbolProvider } from './symbol-provider'
import { getWorkspaceConfiguration } from '@shared/project-configuration'
import { ReoptVCGWebview } from '@shared/interfaces'


export class ReoptVCGViewProvider implements vscode.WebviewViewProvider {

    public static readonly viewType = 'reopt-vcg-view'
    readonly #context: vscode.ExtensionContext
    #outputChannel?: vscode.OutputChannel
    #projectConfigurationWatcher?: vscode.FileSystemWatcher
    #resolveReoptVCGWebview: (w : ReoptVCGWebview) => void

    constructor(
        context: vscode.ExtensionContext,
        resolveReoptVCGWebview: (w : ReoptVCGWebview) => void,
    ) {
        this.#context = context
        this.#resolveReoptVCGWebview = resolveReoptVCGWebview
    }

    public resolveWebviewView(
        webviewView: vscode.WebviewView,
        _context: vscode.WebviewViewResolveContext,
        _token: vscode.CancellationToken,
    ) {
        const webview = webviewView.webview

        const webviewBundleURI = vscode.Uri.joinPath(
            this.#context.extensionUri, 'out', 'reopt-vcg-webview.bundle.js',
        )


        webview.options = {
            enableScripts: true,
            localResourceRoots: [
                this.#context.extensionUri,
                webviewBundleURI,
            ],
        }


        // All URIs must be transformed from local filesystem URIs to webview
        // URIs.
        webview.html = this.getHTMLForWebview(
            webview.cspSource,
            {
                webviewBundle: webview.asWebviewUri(webviewBundleURI),
            },
            // symbols,
        )

        this.#resolveReoptVCGWebview(webview)

    }


    private getHTMLForWebview(
        cspSource: string,
        uris: {
            webviewBundle: vscode.Uri,
        },
        // initialSymbols: vscode.SymbolInformation[],
    ): string {

        // this allows the content security policy to allow execution of exactly
        // the scripts with this random identifier
        const nonce = getNonce()

        const projectConfiguration = getWorkspaceConfiguration(this.#context)

        const initialData: E2W.WebviewInitialData = {
            _tag: 'WebviewInitialData',
        }

        return `
            <!DOCTYPE html>
                <html lang="en" class="html">
                <head>
                <meta charset="UTF-8">
				<meta http-equiv="Content-Security-Policy" content="
                    default-src 'none';
                    style-src ${cspSource};
                    script-src 'nonce-${nonce}';
                ">
				<meta name="viewport" content="width=device-width, initial-scale=1.0">
				<title>Reopt Activity View</title>
			</head>
			<body>
                <script nonce="${nonce}" type="text/javascript">
                    window.${E2W.webviewInitialDataKey} = ${JSON.stringify(initialData)}
                </script>
                <div class="container" id="${Constants.activityWebviewId}" />
                <script nonce="${nonce}" src="${uris.webviewBundle}" />
			</body>
			</html>
        `

    }

}
