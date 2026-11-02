#!/bin/sh
set -eu
FIRSTHAND_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
if command -v Rscript >/dev/null 2>&1; then
  FIRSTHAND_RSCRIPT=$(command -v Rscript)
elif [ -x /Library/Frameworks/R.framework/Resources/bin/Rscript ]; then
  FIRSTHAND_RSCRIPT=/Library/Frameworks/R.framework/Resources/bin/Rscript
else
  printf '%s\n' 'Rscript not found. Install R from https://cran.r-project.org/' >&2
  exit 1
fi
exec "$FIRSTHAND_RSCRIPT" "$FIRSTHAND_DIR/firsthand.R" "$@"
