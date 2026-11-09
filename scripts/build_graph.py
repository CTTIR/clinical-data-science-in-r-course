#!/usr/bin/env python3
"""Build the learning network from the published workshop and exercise sources."""
import csv
import json
import re
from collections import Counter
from itertools import combinations
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PARTS = [
    ('part1', 'I · Clinical questions and data', '#1f77b4'),
    ('part2', 'II · Testing and comparing models', '#ff7f0e'),
    ('part3', 'III · Justifying complex models', '#2ca02c'),
    ('part4', 'IV · Translating models into practice', '#9467bd'),
]
WORKSHOP_TAGS = {
    1: ['clinical-question', 'data-quality', 'study-design'],
    2: ['language-models', 'evidence', 'security'],
    3: ['diagnostics', 'validation', 'evidence'],
    4: ['imaging', 'validation', 'study-design'],
    5: ['causal-inference', 'longitudinal-data', 'study-design'],
    6: ['simulation', 'forecasting', 'model-evaluation'],
    7: ['regulation', 'security', 'data-protection'],
    8: ['reporting', 'evidence', 'model-evaluation'],
}

def build():
    nodes = []
    files = sorted([*ROOT.glob('lessons/*.qmd'), *ROOT.glob('exercises/*.qmd')])
    for path in files:
        relative = path.relative_to(ROOT)
        match = re.search(r'(?:^|-)w([1-8])(?:-|$)', path.stem)
        workshop = int(match[1]) if match else 0
        part = (workshop + 1) // 2 if workshop else 4
        title = re.search(r'^title:\s*"([^"]+)"', path.read_text(), re.M)
        if not title:
            raise ValueError(f'Missing title: {relative}')
        kind = ('Workshop' if relative.parts[0] == 'lessons' else
                'R lab' if path.stem.startswith('lab-') else
                'Case sheet' if path.stem.startswith('casebook-') else
                'Self-test' if path.stem == 'self-test' else 'Project')
        tags = list(WORKSHOP_TAGS.get(workshop, ['reporting', 'evidence', 'model-evaluation']))
        if kind == 'R lab': tags.append('r-lab')
        if kind == 'Project': tags.append('project')
        nodes.append(dict(id=str(relative.with_suffix('')), title=title[1],
                          url=str(relative.with_suffix('.html')), topic=f'part{part}',
                          tags=tags, labels=[], date='', year=0, summary='',
                          week=workshop, session=0 if kind == 'Workshop' else 1, kind=kind))
    if len(nodes) != 22:
        raise ValueError(f'Expected 8 workshops and 14 exercises; found {len(nodes)}')
    if len({n['id'] for n in nodes}) != len(nodes):
        raise ValueError('Duplicate activity identifiers')
    edges = []
    for a, b in combinations(nodes, 2):
        weight = len(set(a['tags']) & set(b['tags']))
        if weight:
            edge = dict(source=a['id'], target=b['id'], weight=weight)
            if weight == 1: edge['dashed'] = True
            edges.append(edge)
    counts = Counter(t for n in nodes for t in n['tags'])
    topics = [dict(id=id, label=label, color=color, order=i,
                   href=next(n['url'] for n in nodes if n['topic']==id and n['kind']=='Workshop'),
                   count=sum(n['topic']==id for n in nodes))
              for i, (id, label, color) in enumerate(PARTS)]
    graph = dict(nodes=nodes, edges=edges, topics=topics,
                 tags=[dict(id=t,label=t,count=c) for t,c in sorted(counts.items(), key=lambda x:(-x[1],x[0]))], labels=[])
    out = ROOT/'artifacts'
    out.mkdir(exist_ok=True)
    (out/'graph.json').write_text(json.dumps(graph, ensure_ascii=False, indent=2)+'\n')
    with (out/'labs.csv').open('w') as f:
        writer=csv.writer(f); writer.writerow(['id','title','url','part','workshop','kind','tags'])
        for n in nodes: writer.writerow([n['id'],n['title'],n['url'],n['topic'],n['week'],n['kind'],'|'.join(n['tags'])])
    pairs=Counter(pair for n in nodes for pair in combinations(sorted(n['tags']),2))
    with (out/'cooccurrence.csv').open('w') as f:
        writer=csv.writer(f); writer.writerow(['tag_a','tag_b','count'])
        for (a,b),count in pairs.most_common(): writer.writerow([a,b,count])
    print(f'Learning network: {len(nodes)} activities, {len(edges)} edges, {len(topics)} parts')
    return graph

if __name__=='__main__': build()
