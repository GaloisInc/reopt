import * as vscode from 'vscode'

import * as Interfaces from './interfaces'


type SerializerFor<T> = (t: T) => Interfaces.SerializationOf<T>


export const serializePosition
    : SerializerFor<vscode.Position>
    = ({ character, line }) => {
        return {
            character,
            line,
        }
    }


export const serializeRange
    : SerializerFor<vscode.Range>
    = ({ end, start }) => {
        return {
            end: serializePosition(end),
            start: serializePosition(start),
        }
    }


export const serializeSymbolInformation
    : SerializerFor<vscode.SymbolInformation>
    = (symbol) => {

        const { containerName, kind, location, name } = symbol
        const { range, uri } = location
        const { fsPath } = uri

        return {
            containerName,
            fsPath,
            kind,
            name,
            // WARNING: here the type-checker provides a false sense of safety in
            // terms of JSON seralizability, it will allow 'symbol.location.range'
            // as a value of type 'SerializedRange', even though a 'vscode.Range'
            // does not serialize correctly!
            range: serializeRange(range),
        }

    }


export const serializeBigInt
    : SerializerFor<BigInt>
    = (bi) => {
        // currently using BigInt for 64-bit addresses, so hex encoding is
        // pleasant
        return bi.toString(16)
    }


export const serializeDisassemblyLineInformation
    : SerializerFor<Interfaces.DisassemblyLineInformation>
    = (info) => {
        return {
            address: info.address,
            addressRange: serializeRange(info.addressRange),
            bytesRange: serializeRange(info.bytesRange),
            firstByteAddress: serializeBigInt(info.firstByteAddress),
            instruction: info.instruction,
            lastByteAddress: serializeBigInt(info.lastByteAddress),
            wholeLineRange: serializeRange(info.wholeLineRange),
        }
    }
