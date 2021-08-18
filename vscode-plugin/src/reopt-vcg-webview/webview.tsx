import * as React from 'react'
import * as ReactDOM from 'react-dom'
import * as vscode from 'vscode'

import * as Constants from '@shared/constants'
import * as E2W from '@shared/extension-to-reopt-vcg-webview'
import * as Interfaces from '@shared/interfaces'
import { UnreachableCaseError } from 'ts-essentials'


export function Webview(_props: {
    initialData: E2W.WebviewInitialData,
    vscode: Interfaces.MainViewVSCodeAPI,
}): JSX.Element {

    const [reoptVCGOutput, setReoptVCGOutput] = React.useState([] as Interfaces.ReoptVCGEntry[])

    /** Listen to messages coming from the extension. */
    React.useEffect(() => {
        const handler = (e: MessageEvent) => {
            return makeMessageListener({
                setReoptVCGOutput,
            })(e.data)
        }
        window.addEventListener('message', handler)
        return () => {
            window.removeEventListener('message', handler)
        }
    })

    const colorForResult = (result: 'sat' | 'unsat') => {
        switch (result) {
            case 'sat': return 'red'
            case 'unsat': return 'green'
            default: throw new UnreachableCaseError(result)
        }
    }

    const renderVCGs = reoptVCGOutput.map((vcg, index) => (
        <tr key={index}>
            <td>{vcg['Machine Code Address']}</td>
            <td style={{ color: colorForResult(vcg['SMT check-sat result']) }}>{vcg['Goal Tag']}</td>
            <td>{vcg['Goal Extra Info']}</td>
        </tr>
    ))

    return (
        <div>
            <table>
                {renderVCGs}
            </table>
        </div>
    )

}


function makeMessageListener(setters: {
    setReoptVCGOutput: Interfaces.ReactSetter<Interfaces.ReoptVCGEntry[]>,
}) {
    return (message: E2W.ExtensionToReoptVCGWebview) => {
        switch (message.tag) {

            case E2W.reoptVCGOutput: {
                setters.setReoptVCGOutput(message.output)
                break
            }

            default: {
                throw new UnreachableCaseError(message.tag)
            }

        }
    }
}
