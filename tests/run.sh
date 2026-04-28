#!/usr/bin/env bash
# Run the site test suite.
# Works around Debian/Ubuntu multiarch: libcrypto/libssl live under
# /usr/lib/x86_64-linux-gnu which sbcl's default foreign-library search
# skips. We prepend it to CFFI's path before loading site.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE/.."

# Tests must NOT rely on AWS credentials or network. Clear any that leaked in.
unset AWS_ACCESS_KEY AWS_SECRET_KEY AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY

exec sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(ql:quickload :cffi :silent t)' \
  --eval '(pushnew #P"/usr/lib/x86_64-linux-gnu/" cffi:*foreign-library-directories* :test #'"'"'equal)' \
  --load site.asd \
  --eval '(ql:quickload :site/tests :silent t)' \
  --eval '(asdf:test-system :site)'
