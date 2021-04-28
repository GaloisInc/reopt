import * as Promisified from '@shared/promisified'

const elfMagicNumber = Uint8Array.from([0x7F, 0x45, 0x4C, 0x46])

export async function checkMagicNumber(
    filePath: string,
): Promise<boolean> {

    const fd = await Promisified.open(filePath, 'r')
    const buffer = new Uint8Array(elfMagicNumber.length)
    const bytesRead = await Promisified.read(fd, buffer, 0, elfMagicNumber.length, 0)
    if (bytesRead !== elfMagicNumber.length) {
        return false
    }
    Promisified.close(fd)

    return (Buffer.compare(buffer, elfMagicNumber) === 0)

}
