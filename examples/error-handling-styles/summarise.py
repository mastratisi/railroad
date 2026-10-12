"""Print the results table from a report written by changes.py.

    python3 summarise.py [report.json]      (default: results.json)
"""
import json, os, sys

path = sys.argv[1] if len(sys.argv) > 1 else os.path.join(os.path.dirname(os.path.abspath(__file__)), 'results.json')
R = json.load(open(path))
STYLES = ('Rail', 'Case', 'Helper')
order = [k for k in R if k != 'S0']

print('edit sites / lines touched (diff -U0 against the unchanged handler files)')
print(f"{'':4}{'Rail':>16}{'Case':>16}{'Helper':>16}   change")
tot = {s: [0, 0] for s in STYLES}
for k in order:
    cells = []
    for s in STYLES:
        h, p, m = R[k]['edit'][s]
        tot[s][0] += h; tot[s][1] += p + m
        cells.append(f'{h} / {p + m}')
    print(f"{k:4}" + ''.join(f'{c:>16}' for c in cells) + '   ' + R[k]['title'])
print(f"{'sum':4}" + ''.join(f"{str(t[0]) + ' / ' + str(t[1]):>16}" for t in tot.values()))

print('\nnaive stage of the shape changes: only the data layer changed')
print(f"{'':4}{'compile errors / functions':>0}")
for k in order:
    if 'naive' not in R[k]:
        continue
    cells = [f"{R[k]['naive'][s]['errors']} / {len(R[k]['naive'][s]['loud_fns'])}" for s in STYLES]
    drift = [x for x, v in R[k]['naive_status'].items() if v == 'DRIFT']
    print(f"{k:4}" + ''.join(f'{c:>16}' for c in cells) + ('   SILENT DRIFT: ' + ', '.join(drift) if drift else ''))

print('\nfixed stage: every property and unit check must be ok')
for k in R:
    out = R[k]['fixed_out']
    bad = [l for l in out if not l.endswith(' ok')]
    print(f"{k:4}{len(out) - len(bad)} ok, {len(bad)} not ok" + (f'   {bad[:2]}' if bad else ''))
