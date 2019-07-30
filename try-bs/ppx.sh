DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
# $DIR/../_build/install/default/bin/ppx $DIR/Shared.macros.re $1 $2
$DIR/node_modules/reason-macros-bin/ppx.js $DIR/Shared.macros.re $1 $2