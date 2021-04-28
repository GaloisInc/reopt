import * as React from 'react'

import * as E2W from '@shared/extension-to-webview'
import * as Interfaces from '@shared/interfaces'
import * as W2E from '@shared/webview-to-extension'

type ReactSetter<T> = React.Dispatch<React.SetStateAction<T>>

function makeMessageListener(setters: {
    setProjectName: ReactSetter<string | undefined>,
    setSymbols: ReactSetter<Interfaces.SerializedSymbolInformation[]>,
}) {
    return (message: E2W.ExtensionToActivityWebview) => {
        switch (message.tag) {

            case E2W.closedProject: {
                setters.setProjectName(undefined)
                break
            }

            case E2W.openedProject: {
                setters.setProjectName(message.projectName)
                break
            }

            case E2W.symbolList: {
                setters.setSymbols(message.symbols)
                break
            }

            // forces exhaustivity checking
            default: {
                const exhaustiveCheck: never = message
                throw new Error(`Unhandled color case: ${exhaustiveCheck}`)
            }

        }
    }
}

enum SortBy {
    Name = 'name',
    Position = 'position',
}

function getSort(
    sortBy: SortBy,
): (
        a: [number, Interfaces.SerializedSymbolInformation],
        b: [number, Interfaces.SerializedSymbolInformation]
    ) => number {
    switch (sortBy) {

        case SortBy.Name: {
            return (a, b) => a[1].name.localeCompare(b[1].name)
        }

        case SortBy.Position: {
            return (a, b) => a[0] - b[0]
        }

    }
}

function jumpToSymbol(
    vscode: Interfaces.ActivityViewVSCodeAPI,
    symbol: Interfaces.SerializedSymbolInformation,
) {
    vscode.postMessage({
        tag: W2E.jumpToSymbol,
        symbol,
    })
}

export function ActivityWebview(props: {
    initialData: E2W.ActivityWebviewInitialData,
    vscode: Interfaces.ActivityViewVSCodeAPI,
}): JSX.Element {

    const { initialData, vscode } = props

    const persisted = props.vscode.getState()

    const [filter, setFilter] = React.useState('')
    const [projectName, setProjectName] = React.useState(
        persisted?.projectName || initialData.projectName
    )
    const [sortBy, setSortBy] = React.useState(SortBy.Position)
    const [symbols, setSymbols] = React.useState(initialData.symbols)

    /*
    This effect tracks all values that are part of the persisted state, and
    triggers an update to the persisted state when any of them changes.  Make
    sure to update the list with all properties fields that should be tracked.
    */
    React.useEffect(() => {
        props.vscode.setState({
            projectName,
        })
    }, [projectName])

    /**
     * This effect listens to messages coming from the extension.
     */
    React.useEffect(() => {
        const handler = (e: MessageEvent) => {
            return makeMessageListener({
                setProjectName,
                setSymbols,
            })(e.data)
        }
        window.addEventListener('message', handler)
        return () => {
            window.removeEventListener('message', handler)
        }
    })

    // const renderSymbols = (
    //     Array.from(
    //         symbols
    //             .filter(s => s.name.indexOf(filter) >= 0)
    //             .entries()
    //     )
    //         .sort(getSort(sortBy))
    //         .map(([_originalIndex, symbol]) => (
    //             <div key={symbol.name}>
    //                 <a
    //                     onClick={() => jumpToSymbol(props.vscode, symbol)}
    //                     style={{ cursor: 'pointer' }}
    //                 >
    //                     {symbol.name}
    //                 </a>
    //             </div>
    //         ))
    // )

    const showProjectFile = () => {
        vscode.postMessage({ tag: W2E.showProjectFile } as W2E.ShowProjectFile)
    }

    const renderProjectName = (
        <h3>
            {
                projectName === undefined
                    ? "No project open"
                    : <a onClick={showProjectFile}>{projectName}</a>
            }
        </h3>
    )

    const isProjectOpen = !(projectName === undefined)

    const renderGenerateButtons = (

        <div>

            <input
                type="button"
                value="Generate disassembly"
                onClick={() => vscode.postMessage({ tag: W2E.generateDisassembly })}
            />

            <input
                type="button"
                value="Generate functions"
                onClick={() => vscode.postMessage({ tag: W2E.generateFunctions })}
            />

            <input
                type="button"
                value="Generate CFG"
                onClick={() => vscode.postMessage({ tag: W2E.generateCFG })}
            />

            <input
                type="button"
                value="Generate LLVM"
                onClick={() => vscode.postMessage({ tag: W2E.generateLLVM })}
            />

        </div>

    )

    const renderCloseProject = (
        <input
            className="red"
            type="button"
            value="Close reopt project"
            onClick={() => vscode.postMessage({ tag: W2E.closeProject })}
        />
    )

    return (
        <div className="column">
            <div className="column-header">

                <input
                    type="button"
                    value="Create reopt project..."
                    onClick={() => vscode.postMessage({ tag: W2E.createProjectFile })}
                />

                <input
                    type="button"
                    value="Open reopt project..."
                    onClick={() => vscode.postMessage({ tag: W2E.openProject })}
                />

                {isProjectOpen && renderCloseProject}

                {renderProjectName}

                {isProjectOpen && renderGenerateButtons}

                {/*
                <div className="row">
                    <select value={sortBy} onChange={e => setSortBy(e.target.value as SortBy)}>
                        <option value={SortBy.Name}>Sort by name</option>
                        <option value={SortBy.Position}>Sort by position</option>
                    </select>
                </div>

                <div className="row">
                    <input type='text'
                        autoCorrect='false'
                        placeholder='Filter...'
                        spellCheck='false'
                        value={filter}
                        onChange={e => setFilter(e.target.value)}
                    />
                </div>
 */}

            </div>

            <div className="column-body">
            </div>

        </div >
    )

}
