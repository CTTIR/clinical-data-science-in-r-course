#!/usr/bin/env python3
"""Check deployment links, complete network coverage, search, and private-page exclusion."""
import json
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit
ROOT=Path(__file__).resolve().parent.parent
SITE=ROOT/'_site'
errors=[]
class Links(HTMLParser):
    def handle_starttag(self, tag, attrs):
        attrs=dict(attrs)
        for attr in ('href','src'):
            if attr not in attrs: continue
            url=urlsplit(attrs[attr])
            if url.scheme or url.netloc or not url.path: continue
            path=unquote(url.path)
            if path.startswith('/clinical-data-science-in-r-course/'):
                target=SITE/path.removeprefix('/clinical-data-science-in-r-course/')
            elif path.startswith('/'): target=SITE/path.lstrip('/')
            else: target=self.page.parent/path
            if not target.exists(): errors.append(f'{self.page.relative_to(SITE)}: missing {path}')
for page in SITE.rglob('*.html'):
    text=page.read_text()
    parser=Links(); parser.page=page; parser.feed(text)
    for slug in ('clinical-data-science-in-r-course', 'clinical-data-science-in-r', 'methods-in-r', 'scientometrics-in-r', 'strategy-in-r'):
        if 'r-heller.github.io/' + slug + '/' in text: errors.append(f'{page.name}: stale URL for {slug}')
    if '?@sec' in text or '?@fig' in text or '?@tbl' in text: errors.append(f'{page.name}: unresolved reference')
for name in ['index.html','overview.html','pagefind/pagefind.js','pagefind/pagefind-entry.json','artifacts/graph.json']:
    if not (SITE/name).is_file(): errors.append(f'Missing {name}')
assert not (SITE/'instructor').exists(), 'Instructor materials must not be rendered'
graph=json.loads((SITE/'artifacts/graph.json').read_text())
assert len(graph['nodes'])==22 and len(graph['topics'])==4 and graph['edges']
for node in graph['nodes']:
    if not (SITE/node['url']).is_file(): errors.append(f'Missing network target {node["url"]}')
if errors: raise SystemExit('\n'.join(sorted(set(errors))))
print('Site checks passed: local links, 22 network targets, search artifacts, repository URLs and instructor exclusion')
