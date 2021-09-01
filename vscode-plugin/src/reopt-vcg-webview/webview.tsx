import { Context } from 'immutability-helper'
import * as React from 'react'
import { UnreachableCaseError } from 'ts-essentials'

import * as E2W from '@shared/extension-to-reopt-vcg-webview'
import * as Interfaces from '@shared/interfaces'


const update = (new Context()).update


export function Webview(props: {
    initialData: E2W.WebviewInitialData,
    vscode: Interfaces.ReoptVCGViewVSCodeAPI,
}): JSX.Element {

    /** If we have a persistent state, it is authoritative, otherwise, the
     * initial data is used. */
    const [reoptVCGEntries, setReoptVCGEntries] = React.useState(
        props.vscode.getState()?.results || props.initialData.entries
    )
    const [showValidVCGs, setShowValidVCGs] = React.useState(false)

    // On the very first run, replace pre-existing persisted state with the
    // state stored in the workspace.  This is useful because there may be
    // events happening to the workspace state while the webview is not active,
    // so the initial data is more authoritative here.
    React.useEffect(() => {
        const oldPersisted = props.vscode.getState() || Interfaces.initialReoptVCGViewPersistedState
        props.vscode.setState(update(oldPersisted, { results: { $set: props.initialData.entries } }))
        setReoptVCGEntries(props.initialData.entries)
    }, [] /* only on startup */)

    const addAndPersistReoptVCGEntry = (newEntry: Interfaces.ReoptVCGEntry) => {
        // Because the webview can go out of view, the persisted state has the
        // authoritative set of entries.
        const oldPersisted = props.vscode.getState() || Interfaces.initialReoptVCGViewPersistedState
        const allEntries = update(oldPersisted.results, { $push: [newEntry] })
        props.vscode.setState(update(oldPersisted, { results: { $set: allEntries } }))
        console.log(`Setting ${allEntries.length} entries in the React state and persisted state`)
        setReoptVCGEntries(allEntries)
    }

    const setAndPersistReoptVCGEntries = (entries: Interfaces.ReoptVCGEntry[]) => {
        const oldPersisted = props.vscode.getState() || Interfaces.initialReoptVCGViewPersistedState
        props.vscode.setState(update(oldPersisted, { results: { $set: entries } }))
        setReoptVCGEntries(entries)
    }

    /** Listen to messages coming from the extension. */
    React.useEffect(() => {
        const handler = (e: MessageEvent) => {
            return makeMessageListener({
                addAndPersistReoptVCGEntry,
                setReoptVCGEntries: setAndPersistReoptVCGEntries,
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

    const vcgCount = reoptVCGEntries.length

    const goodVCGs = reoptVCGEntries.filter(vcg => vcg['SMT check-sat result'] === 'unsat')
    const badVCGs = reoptVCGEntries.filter(vcg => vcg['SMT check-sat result'] === 'sat')

    const renderVCGs = (
        (showValidVCGs ? reoptVCGEntries : badVCGs)
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
                <span style={{ float: 'right' }}>
                    <input
                        onChange={e => setShowValidVCGs(e.target.checked)}
                        name="show-safe-vcgs"
                        type="checkbox"
                    />
                    <label htmlFor="show-safe-vcgs">Display safe VCGs</label>
                </span>
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
    addAndPersistReoptVCGEntry(entry: Interfaces.ReoptVCGEntry): void,
    setReoptVCGEntries(entries: Interfaces.ReoptVCGEntry[]): void,
}) {
    return (message: E2W.ExtensionToReoptVCGWebview) => {
        switch (message.tag) {

            case E2W.Tags.addReoptVCGEntry: {
                setters.addAndPersistReoptVCGEntry(message.entry)
                break
            }

            case E2W.Tags.setReoptVCGEntries: {
                setters.setReoptVCGEntries(message.entries)
                break
            }

            default: {
                throw new UnreachableCaseError(message)
            }

        }
    }
}
