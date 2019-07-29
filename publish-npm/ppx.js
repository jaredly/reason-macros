#!/usr/bin/env node

const fs = require('fs')
const {spawnSync} = require('child_process')
const path = require('path')

const binary = path.resolve(__dirname, `macros-${process.platform}.exe`);
if (!fs.existsSync(binary)) {
  console.error(`No macros binary for ${process.platform} found (looked for ${binary})`)
  process.exit(1)
}

const result = spawnSync(binary, process.argv.slice(2), {stdio: 'inherit', maxBuffer: Number.MAX_SAFE_INTEGER})
if (result.status !== 0) {
  process.exit(result.status)
}
