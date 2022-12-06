import * as fs from 'fs'
import * as util from 'util'

export const close = util.promisify(fs.close)
export const open = util.promisify(fs.open)
export const read = util.promisify(fs.read)
export const readFile = util.promisify(fs.readFile)
export const stat = util.promisify(fs.stat)
export const writeFile = util.promisify(fs.writeFile)
