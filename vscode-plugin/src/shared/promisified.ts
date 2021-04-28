import { promisify } from 'es6-promisify'
import * as fs from 'fs'

export const close = promisify(fs.close)
export const open = promisify(fs.open)
export const read = promisify(fs.read)
export const readFile = promisify(fs.readFile)
export const stat = promisify(fs.stat)
export const writeFile = promisify(fs.writeFile)
