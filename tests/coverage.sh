#!/usr/bin/env bash
# Run the test suite with SBCL's sb-cover instrumentation and emit an HTML
# report under tests/coverage/ plus a terse per-file summary to stdout.
#
# sb-cover measures expression-level coverage: each form is marked hit or
# not. The report splits coverage into "expressions" (all forms) and
# "branches" (forms where the result is consumed for control flow). Good
# proxy for "code I've actually exercised," not a correctness oracle.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE/.."

unset AWS_ACCESS_KEY AWS_SECRET_KEY AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY

OUT="$HERE/coverage"
rm -rf "$OUT"
mkdir -p "$OUT"

# Wipe the fasl cache so everything recompiles with instrumentation.
rm -rf /home/ray/.cache/common-lisp/sbcl-*-linux-x64-s/home/ray/projects/site/ 2>/dev/null || true

sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(ql:quickload :cffi :silent t)' \
  --eval '(pushnew #P"/usr/lib/x86_64-linux-gnu/" cffi:*foreign-library-directories* :test #'"'"'equal)' \
  --eval '(require :sb-cover)' \
  --eval '(declaim (optimize sb-cover:store-coverage-data))' \
  --load site.asd \
  --eval '(asdf:oos (quote asdf:load-op) :site :force t)' \
  --eval '(declaim (optimize (sb-cover:store-coverage-data 0)))' \
  --eval '(ql:quickload :site/tests :silent t)' \
  --eval '(asdf:test-system :site)' \
  --eval "(sb-cover:report #P\"$OUT/\")"

echo
echo "=== coverage summary ==="
python3 - <<'PYEOF' "$OUT"
import re, sys, pathlib
out = pathlib.Path(sys.argv[1])
index = out / "cover-index.html"
if not index.exists():
    print("no cover-index.html produced")
    sys.exit(0)
text = index.read_text()
# Each data row in sb-cover's report:
#   <tr class='odd|even'>
#     <td class='text-cell'><a href='HASH.html'>FILE.lisp</a></td>
#     <td>EHIT</td><td>ETOT</td><td>EPCT</td>
#     <td>BHIT</td><td>BTOT</td><td>BPCT</td>
#   </tr>
pat = re.compile(
    r"<tr class='(?:odd|even)'>\s*"
    r"<td class='text-cell'><a [^>]*>([^<]+\.lisp)</a></td>\s*"
    r"<td>(\d+)</td><td>(\d+)</td><td>\s*([^<]+)</td>\s*"
    r"<td>(\d+)</td><td>(\d+)</td><td>\s*([^<]+)</td>\s*"
    r"</tr>"
)
rows = pat.findall(text)
if not rows:
    print("(regex didn't match; open the HTML manually)")
    print(f"full HTML report: {index}")
    sys.exit(0)

print(f"{'file':<22} {'expr (%)':>18} {'branch (%)':>18}")
print("-" * 62)
tot_eh = tot_et = tot_bh = tot_bt = 0
for fname, eh, et, ep, bh, bt, bp in rows:
    eh, et, bh, bt = int(eh), int(et), int(bh), int(bt)
    tot_eh += eh; tot_et += et; tot_bh += bh; tot_bt += bt
    expr = f"{eh}/{et} ({ep.strip()})"
    if bt == 0:
        br = f"— / —"
    else:
        br = f"{bh}/{bt} ({bp.strip()})"
    print(f"{fname:<22} {expr:>18} {br:>18}")
print("-" * 62)
ep = (tot_eh * 100.0 / tot_et) if tot_et else 0
bp = (tot_bh * 100.0 / tot_bt) if tot_bt else 0
print(f"{'TOTAL':<22} {f'{tot_eh}/{tot_et} ({ep:.1f})':>18} {f'{tot_bh}/{tot_bt} ({bp:.1f})':>18}")
print()
print(f"full HTML report: file://{index}")
PYEOF
