/**
 * Namespace for messages and types passed from extension to webview.
 */
export namespace e2w {

    export const enum Tag {
    }

    /**
     * Base interface for updates passed from vscode extension to webview.
     */
    export type Event = void

}

/**
 * Namespace for messages passed from webview to extension
 */
export namespace w2e {
    export const enum Tag {
    }

    /** Type for events from webview to extension. */
    export type Event = void
}