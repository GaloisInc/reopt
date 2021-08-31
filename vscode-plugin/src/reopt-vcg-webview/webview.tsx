import { Context } from 'immutability-helper'
import * as React from 'react'
import { UnreachableCaseError } from 'ts-essentials'

import * as E2W from '@shared/extension-to-reopt-vcg-webview'
import * as Interfaces from '@shared/interfaces'


const update = (new Context()).update


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

    const textForResult = (result: 'sat' | 'unsat') => {
        switch (result) {
            case 'unsat': return '✓'
            case 'sat': return '❌'
            default: throw new UnreachableCaseError(result)
        }
    }

    const vcgCount = reoptVCGOutput.length

    const goodVCGs = reoptVCGOutput.filter(vcg => vcg['SMT check-sat result'] === 'unsat')
    const badVCGs = reoptVCGOutput.filter(vcg => vcg['SMT check-sat result'] === 'sat')

    const renderVCGs = (
        badVCGs
            .map((vcg, index) => {

                const result = vcg['SMT check-sat result']

                return (
                    <tr key={index}>
                        <td>{vcg['Machine Code Address']}</td>
                        <td style={{ color: colorForResult(result) }}>{textForResult(result)}</td>
                        <td>{vcg['Goal Tag']}</td>
                        <td>{vcg['Goal Extra Info']}</td>
                    </tr>
                )
            })
    )

    return (
        <div>
            <p>
                {vcgCount} VCGs attempted ({goodVCGs.length} proved safe, {badVCGs.length} currently unsafe)
                {/* <input
                    style={{ marginLeft: '40px' }}
                    type="checkbox"
                    name="show-safe-vcgs"
                />
                <label htmlFor="show-safe-vcgs">Display safe VCGs</label> */}
            </p>
            <table>
                <tbody>
                    {renderVCGs}
                </tbody>
            </table>
        </div>
    )

}


function makeMessageListener(setters: {
    setReoptVCGOutput: Interfaces.ReactSetter<Interfaces.ReoptVCGEntry[]>,
}) {
    return (message: E2W.ExtensionToReoptVCGWebview) => {
        switch (message.tag) {

            case E2W.clearReoptVCGEntries: {
                setters.setReoptVCGOutput([])
                break
            }

            case E2W.reoptVCGEntry: {
                setters.setReoptVCGOutput(entries =>
                    update(entries, { $push: [message.entry] })
                )
                break
            }

            default: {
                throw new UnreachableCaseError(message)
            }

        }
    }
}
