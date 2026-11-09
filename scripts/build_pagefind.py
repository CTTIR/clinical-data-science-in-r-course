#!/usr/bin/env python3
"""Build the Pagefind static search index over docs/.

Quarto `post-render` step (wired in _quarto.yml). Cross-platform via npx.
Index scope: lab detail pages only — courseN_*/labs/*.html. The four
course-index files are stamped with `data-pagefind-ignore="all"` so
they don't dilute search relevance with their auto-listing of all labs.

Missing tools or output fail the build; a successful site includes search.
"""
from __future__ import annotations

import re
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DOCS = ROOT / "_site"


def main() -> int:
    if not DOCS.exists():
        print(f"build_pagefind: ERROR — {DOCS.relative_to(ROOT)} does not exist", file=sys.stderr)
        return 1

    executable = ROOT / "node_modules" / ".bin" / (
        "pagefind.cmd" if sys.platform == "win32" else "pagefind")
    if not executable.is_file():
        print("build_pagefind: ERROR — run npm ci before rendering", file=sys.stderr)
        return 1

    skipped = _ignore_course_indexes()
    if skipped:
        print(f"build_pagefind: marked {skipped} course-index pages with data-pagefind-ignore")

    cmd = [
        str(executable),
        "--site", str(DOCS),
        "--glob", "{lessons,exercises}/*.html",
        "--output-subdir", "pagefind",
    ]
    print("build_pagefind: " + " ".join(cmd))
    # Never accept an index left by an earlier render.
    index_root = DOCS / "pagefind"
    if index_root.exists():
        shutil.rmtree(index_root)
    result = subprocess.run(cmd, cwd=ROOT)
    if result.returncode != 0:
        print(f"build_pagefind: ERROR — pagefind exited {result.returncode}", file=sys.stderr)
        return result.returncode

    index_root = DOCS / "pagefind"
    if not all((index_root / name).is_file() and (index_root / name).stat().st_size
               for name in ("pagefind.js", "pagefind-entry.json")):
        print(
            f"build_pagefind: ERROR — expected {index_root.relative_to(ROOT)}/pagefind.js "
            f"after build, but it was not created",
            file=sys.stderr,
        )
        return 1
    # Preserve notices with the distributed browser runtime and R downloads.
    shutil.copytree(ROOT / 'node_modules/pagefind/LICENSE', index_root / 'licenses')
    shutil.copyfile(ROOT / 'LICENSE', DOCS / 'LICENSE-CODE.md')
    print(f"build_pagefind: OK -> {index_root.relative_to(ROOT)}/")
    return 0


_BODY_OPEN_RE = re.compile(r"<body\b(?![^>]*\bdata-pagefind-ignore\b)([^>]*)>")


def _ignore_course_indexes() -> int:
    """Insert data-pagefind-ignore='all' into each course-index <body>.

    Idempotent — files that already carry the attribute are left alone.
    Returns the number of files actually rewritten this pass.
    """
    n = 0
    for index in DOCS.glob("course*/index.html"):
        text = index.read_text(encoding="utf-8", errors="replace")
        new, count = _BODY_OPEN_RE.subn(
            r'<body data-pagefind-ignore="all"\1>', text, count=1
        )
        if count:
            index.write_text(new, encoding="utf-8")
            n += 1
    return n


if __name__ == "__main__":
    sys.exit(main())
