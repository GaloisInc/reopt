import * as vscode from 'vscode'

import * as Interfaces from './interfaces'

type DeserializerFor<T> = (t: Interfaces.SerializationOf<T>) => T


export const deserializePosition
    : DeserializerFor<vscode.Position> = (pos) => {
        return new vscode.Position(pos.line, pos.character)
    }


export const deserializeRange
    : DeserializerFor<vscode.Range> = (range) => {
        return new vscode.Range(
            deserializePosition(range.start),
            deserializePosition(range.end),
        )
    }


export const deserializeSymbolInformation
    : DeserializerFor<vscode.SymbolInformation> = (
        symbol: Interfaces.SerializationOf<vscode.SymbolInformation>,
    ) => {

        const { range } = symbol

        return new vscode.SymbolInformation(
            symbol.name,
            symbol.kind,
            symbol.containerName,
            new vscode.Location(
                vscode.Uri.file(symbol.fsPath),
                deserializeRange(range),
            )
        )

    }


export const deserializeBigInt
    : DeserializerFor<BigInt>
    = (s) => {
        return BigInt(`0x${s}`)
    }


export const deserializeDisassemblyLineInformation
    : DeserializerFor<Interfaces.DisassemblyLineInformation> =
    (info) => {

        return {
            address: info.address,
            addressRange: deserializeRange(info.addressRange),
            bytesRange: deserializeRange(info.bytesRange),
            firstByteAddress: deserializeBigInt(info.firstByteAddress),
            instruction: info.instruction,
            lastByteAddress: deserializeBigInt(info.lastByteAddress),
            wholeLineRange: deserializeRange(info.wholeLineRange),
        }

    }
