#!/bin/bash
set -e
node_modules/bs-platform/lib/bs_ppx_tools.exe <(./node_modules/.bin/bsrefmt src/Macros.re --print binary) >(./node_modules/.bin/bsrefmt --parse binary --print re > publish/src/Macros.re)
sleep .1
wc -l publish/src/Macros.re
# cp publish/src/Js_deep.re publish-esy/src/
echo Done