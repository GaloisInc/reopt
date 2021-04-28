import * as vscode from 'vscode'

export interface LocatedAddress {
    address: string
    range: vscode.Range
}

const addressesStateKey = 'reopt-addresses'
export function clearWorkspaceAddresses(
    context: vscode.ExtensionContext,
): void {
    context.workspaceState.update(addressesStateKey, undefined)
}
export function getWorkspaceAddresses(
    context: vscode.ExtensionContext,
): LocatedAddress[] | undefined {
    return context.workspaceState.get(addressesStateKey)
}
export function setWorkspaceAddresses(
    context: vscode.ExtensionContext,
    addresses: LocatedAddress[],
): void {
    context.workspaceState.update(addressesStateKey, addresses)
}
