import { getOrElse } from 'fp-ts/Option'
import * as vscode from 'vscode'

import * as Constants from '@shared/constants'
import * as E2W from '@shared/extension-to-webview'

import * as ActivityMessageHandler from './activity-message-handler'
import { getNonce } from './nonce'
import { LLVMDocumentSymbolProvider } from './symbol-provider'
import { getWorkspaceConfiguration } from '@shared/project-configuration'


export class ActivityViewProvider implements vscode.WebviewViewProvider {

    public static readonly viewType = 'reopt-activity-view'
    readonly #context: vscode.ExtensionContext
    readonly #diagnosticCollection: vscode.DiagnosticCollection
    #outputChannel?: vscode.OutputChannel
    #projectConfigurationWatcher?: vscode.FileSystemWatcher


    constructor(
        context: vscode.ExtensionContext,
    ) {
        this.#context = context
        this.#diagnosticCollection = vscode.languages.createDiagnosticCollection('reopt')
    }


    /**
     * We are always watching the currently-opened project file for
     * modifications, so that we can update the webview upon changes.  This
     * method disposes of the previous watcher, if any, and registers
     * 'newWatcher' as the current watcher.
     * @param newWatcher - new project configuration watcher
     */
    private replaceProjectConfigurationWatcher(
        newWatcher?: vscode.FileSystemWatcher,
    ): void {
        if (this.#projectConfigurationWatcher) {
            this.#projectConfigurationWatcher.dispose()
        }
        this.#projectConfigurationWatcher = newWatcher
    }


    /** We always have at most one output channel.  Currently, the only way of
     * disposing it is to replace it. */
    private replaceOutputChannel(
        newChannel: string,
    ): ActivityMessageHandler.PrivateOutputChannel {
        if (this.#outputChannel) {
            this.#outputChannel.dispose()
        }
        this.#outputChannel = vscode.window.createOutputChannel(newChannel)
        return this.#outputChannel
    }


    public resolveWebviewView(
        webviewView: vscode.WebviewView,
        _context: vscode.WebviewViewResolveContext,
        _token: vscode.CancellationToken,
    ) {

        const stylesheetURI = vscode.Uri.joinPath(
            this.#context.extensionUri, 'out', 'webview-static', 'activity-webview.css'
        )
        const webviewBundleURI = vscode.Uri.joinPath(
            this.#context.extensionUri, 'out', 'activity-webview.bundle.js',
        )

        const webview = webviewView.webview

        webview.options = {
            enableScripts: true,
            localResourceRoots: [
                this.#context.extensionUri,
                stylesheetURI,
                webviewBundleURI,
            ],
        }

        const activeDocument = vscode.window.activeTextEditor?.document
        const symbols =
            (activeDocument
                ? new LLVMDocumentSymbolProvider().provideDocumentSymbols(activeDocument)
                : []
            )

        if (symbols.length > 30) {
            vscode.window.activeTextEditor?.revealRange(
                symbols[27].location.range
            )
        }

        webview.onDidReceiveMessage(
            ActivityMessageHandler.makeMessageHandler(
                this.#context,
                webview,
                this.#diagnosticCollection,
                this.replaceProjectConfigurationWatcher.bind(this),
                this.replaceOutputChannel.bind(this),
            ),
        )

        // All URIs must be transformed from local filesystem URIs to webview
        // URIs.
        webview.html = this.getHTMLForWebview(
            webview.cspSource,
            {
                stylesheet: webview.asWebviewUri(stylesheetURI),
                webviewBundle: webview.asWebviewUri(webviewBundleURI),
            },
            // symbols,
        )

    }


    private getHTMLForWebview(
        cspSource: string,
        uris: {
            stylesheet: vscode.Uri,
            webviewBundle: vscode.Uri,
        },
        // initialSymbols: vscode.SymbolInformation[],
    ): string {

        // this allows the content security policy to allow execution of exactly
        // the scripts with this random identifier
        const nonce = getNonce()

        const projectConfiguration = getWorkspaceConfiguration(this.#context)

        const initialData: E2W.ActivityWebviewInitialData = {
            projectName: (
                projectConfiguration
                    ? getOrElse<string | undefined>
                        (() => undefined)
                        (projectConfiguration.name)
                    : undefined
            ),
            symbols: [],
            // symbols: initialSymbols.map(Serializers.serializeSymbolInformation),
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
				<link href="${uris.stylesheet}" rel="stylesheet" />
			</head>
			<body>
                <script nonce="${nonce}" type="text/javascript">
                    window.${E2W.activityWebviewInitialDataKey} = ${JSON.stringify(initialData)}
                </script>
                <div class="container" id="${Constants.activityWebviewId}" />
                <script nonce="${nonce}" src="${uris.webviewBundle}" />
			</body>
			</html>
        `

    }

}
