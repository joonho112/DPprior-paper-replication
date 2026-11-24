#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
"""Check the application exhibits against separately frozen manuscript and analysis values."""
import argparse
import hashlib
import json
import math
import re
import sys
from collections import Counter
from pathlib import Path
from statistics import mean

from simulation import DisplayRows, Mismatch, close, compare, csv_rows, index


TABLES = {
    'main-table-4': ('main-table', ('app', 'prior')),
    'osm-table-H.3': ('all-priors', ('app', 'prior')),
    'osm-table-H.4': ('all-posteriors', ('app', 'prior')),
    'osm-table-H.16': ('spread', ('app', 'prior')),
    'osm-table-H.17': ('changes', ('app', 'prior_a', 'prior_b')),
    'osm-table-H.18': ('chain-checks', ('app', 'metric')),
}
N_UNITS = {'star': 79, 'bd': 48, 'rasch': 500}
N_GROUP = {('star','upper'): 8, ('star','central'): 39,
           ('bd','upper'): 5, ('bd','central'): 25}
APP_NAMES = {'Project STAR': 'star', 'Writing-to-learn': 'bd', 'Vocabulary checklist': 'rasch'}
NUMBERS = re.compile(r'-?\d+(?:\.\d+)?')


def require(test, message):
    if not test:
        raise Mismatch(message)


def canonical(rows):
    """Normalize numeric grid keys and harmless aliases across archived publications."""
    aliases = {'Vague':'Gamma(1,1)', 'Gaussian benchmark':'Gaussian',
               'Gaussian Benchmark':'Gaussian', 'Normal benchmark':'Gaussian'}
    for r in rows:
        for key in ('p','x'):
            if key in r:
                r[key] = format(float(r[key]), '.12g')
        if 'prior' in r:
            r['prior'] = aliases.get(r['prior'], r['prior'])
    return rows


def pool_table(rows, actual):
    ref = index(rows, ('app','group','metric'), 'manuscript pooling groups')
    obs = index(actual, ('app','group'), 'Table H.19')
    expected_keys = {(a,g) for a in ('star','bd') for g in ('upper','central','excess','mcse')}
    require(obs.keys() == expected_keys, 'H.19 group identities or count changed')
    checked = 0
    for (app, group), r in obs.items():
        src = 'upper_minus_central' if group in ('excess','mcse') else group
        co = ref[app,src,'coassign']
        distance = ref[app,src,'displacement']
        if (app,group) in N_GROUP:
            for key, val in {'n':N_GROUP[app,group], 'coassign_gamma':co['gamma'], 'coassign_dual':co['dual'],
                             'distance_gamma':distance['gamma'],'distance_dual':distance['dual']}.items():
                close(val, r[key], f'H.19 {app}/{group} {key}')
                checked += 1
        for key, source, scale in [('coassign_reduction_pp',co,100),('distance_reduction',distance,1)]:
            val = float(source['mcse_delta']) if group == 'mcse' else -float(source['delta'])
            close(val*scale, r[key], f'H.19 {app}/{group} {key}')
            checked += 1
    return checked


def check_groups(fig, table):
    require(Counter((r['app'],r['group']) for r in fig) == N_GROUP, 'Figure 6 fixed group membership changed')
    for (app,group), n in N_GROUP.items():
        r = next(x for x in table if x['app']==app and x['group']==group)
        z = [x for x in fig if x['app']==app and x['group']==group]
        for a,b in [('coassign','delta_coassign'), ('displacement','delta_displacement')]:
            for x in z:
                close(float(x[a+'_dual']) - float(x[a+'_gamma']), x[b], f'Figure 6 {b} source direction')
            expected = -mean(float(x[b]) for x in z) * (100 if a=='coassign' else 1)
            close(expected, r['coassign_reduction_pp' if a=='coassign' else 'distance_reduction'],
                  f'Figure 6 / H.19 sign reversal {app} {group}')
    return 2*(len(fig)+len(N_GROUP))


def check_weights(root, scores, h4, ranks):
    canonical_scores = {int(x['score']):int(x['respondents']) for x in scores if x['prior']=='Gamma(1,1)'}
    require(sorted(canonical_scores) == list(range(14)) and sum(canonical_scores.values()) == 500,
            'Expected 14 raw scores and 500 vocabulary respondents')
    raw = csv_rows(root/'data-frozen/rasch/score_counts.csv')
    require(len(raw) == len(canonical_scores) and {int(x['raw_score']):int(x['n']) for x in raw} == canonical_scores,
            'Rasch score frequency is missing, duplicated or changed')
    grouped = csv_rows(root/'data-frozen/application-summaries/chain-mean-by-unit-or-score.csv')
    for block in ('chains_12','chains_34'):
        for prior in ('gamma','tsmm','dual'):
            z=[r for r in grouped if r['block']==block and r['prior']==prior and r['app']=='rasch']
            require(len(z)==14 and {int(x['unit_or_score']):int(x['frequency']) for x in z}==canonical_scores,
                    f'Rasch frequency changed for {prior} {block}')
    for r in h4:
        if r['app']!='rasch':
            continue
        z=[x for x in scores if x['prior']==r['prior']]
        rms=math.sqrt(sum(canonical_scores[int(x['score'])]*float(x['change_logits'])**2 for x in z)/500)
        close(rms,r['unit_rms'],'Table H.4 frequency-weighted Rasch RMS')
    for r in ranks:
        draws=16000 if r['block_a']=='full' else 8000
        close(draws*N_UNITS[r['app']],r['n_ranks'],'All-chain / split pooled ranks')
    return len(ranks)+14+6*14+5


def check_printed(entries, tables):
    verified=0
    for r in entries:
        name=r['artifact']
        obs = tables[name][tuple(r['key'].split('|'))]
        token=r['printed']; decimals=len(token.partition('.')[2])
        actual=float(obs[r['field']])*float(r['scale'])
        require(abs(actual-float(token)) <= .5*10**-decimals + 1e-10,
                f"{name} {r['key']} {r['field']} expected printed {token}, found {actual}")
        verified+=1
    return verified


def display_rows(path):
    if path.suffix == '.html':
        parser=DisplayRows()
        parser.feed(path.read_text())
        return parser.rows
    # The longtable generator places the column headings before endhead.
    body=path.read_text().split(r'\midrule\endhead',1)[1].split(r'\bottomrule',1)[0]
    return [[x.strip().replace(r'\%','%').replace(r'\_','_') for x in line.split(r'\\',1)[0].split('&')]
            for line in body.splitlines() if '&' in line]


def rendered_tables(out, printed):
    expected={(r['artifact'],r['key'],r['field']):r['printed'] for r in printed}
    count=0
    for ext in ('.html','.tex'):
        observed={}
        for id in [*TABLES,'osm-table-H.19']:
            for row in display_rows(out/(id+ext)):
                app=APP_NAMES.get(row[0],row[0])
                key=app+'|'+row[1]
                if id=='main-table-4':
                    key=app+'|'+row[3]
                    keys='prior_mean_count prior_majority posterior_mean_count posterior_one largest_population_weight finite_sd'.split()
                    vals=row[4:10]
                elif id.endswith('H.3'):
                    keys='prior_mean_count q05 q95 prior_majority prior_tail_09'.split()
                    vals=NUMBERS.findall(row[3])+row[4:6]
                    if len(NUMBERS.findall(row[2]))==2:
                        keys+=['a','b']; vals+=NUMBERS.findall(row[2])
                elif id.endswith('H.4'):
                    keys='posterior_mean_count q025 q975 posterior_one largest_observed_share largest_population_weight unit_rms'.split()
                    vals=NUMBERS.findall(row[2])+row[3:7]
                elif id.endswith('H.16'):
                    keys='sd_mean sd_lower sd_upper iqr_mean iqr_lower iqr_upper'.split()
                    vals=NUMBERS.findall(row[2])+NUMBERS.findall(row[3])
                elif id.endswith('H.17'):
                    key+='|'+row[2];keys='w2 w2_central_90 unit_rms'.split();vals=row[3:6]
                elif id.endswith('H.18'):
                    key=app+'|'+('w2' if 'All quantiles' in row[1] else 'w2_central_90' if 'percentiles' in row[1] else 'unit_rms')
                    keys='full chains_12 chains_34 within_gamma within_dual'.split();vals=row[2:7]
                else:
                    group={'Middle':'central','Upper':'upper','Excess reduction':'excess','Monte Carlo SE':'mcse'}.get(row[1],row[1])
                    key=app+'|'+group
                    keys='n coassign_gamma coassign_dual coassign_reduction_pp distance_gamma distance_dual distance_reduction'.split()
                    vals=row[2:9]
                for k,v in zip(keys,vals):
                    if (id,key,k) in expected:observed[id,key,k]=v
        require(observed.keys()==expected.keys(),f'Missing printed fields in {ext} application tables: {list(expected.keys()-observed.keys())[:3]}')
        for key,token in expected.items():
            require(observed[key]==token,f'{ext} {key} expected {token}, rendered {observed[key]}')
        count+=len(observed)
    return count


def verify(root):
    expected=root/'verification/expected/exhibits/applications'
    out=root/'outputs/exhibits'
    working=root/'data-derived/exhibits/applications'
    manifest=json.loads((expected/'manifest.json').read_text())
    for filename,sha in manifest['files'].items():
        require(hashlib.sha256((expected/filename).read_bytes()).hexdigest()==sha,
                f'Independent application reference changed: {filename}')
    def read(directory,name):
        return canonical(csv_rows(directory/(name+'.csv')))
    def ref(name): return read(expected,name)
    def data(name): return read(working,name)
    def frame(id): return read(out,id)
    checked={}
    tables={}
    for name,(origin,keys) in TABLES.items():
        z=frame(name)
        checked[name]=compare(name,ref(origin),z,keys)
        tables[name]=index(z,keys,name)
    h19=frame('osm-table-H.19')
    checked['osm-table-H.19']=pool_table(ref('pooling-groups'),h19)
    tables['osm-table-H.19']=index(h19,('app','group'),'H.19')
    for label,expected_name,keys,observed in [
        ('Figure 5A','figure5-a',('application',),data('figure-5-panel-a')),
        ('Figure 5 all quantile profiles','profiles',('app','block','p'),frame('main-figure-5')),
        ('Figure 6 points','pooling-points',('app','unit','group'),frame('main-figure-6')),
        ('H.2 CDFs','cdf',('app','prior','x'),frame('osm-figure-H.2')),
        ('H.2 prior ranges','cdf-ranges',('app','set','x'),data('pointwise-prior-ranges')),
        ('H.2 maximum gaps','cdf-maxima',('app','set'),data('grid-maxima')),
        ('H.3 intervals','interval-exceptions',('app','unit','prior_label'),frame('osm-figure-H.3')),
        ('H.3 EAP changes','score-changes',('prior','score'),data('rasch-score-changes')),
        ('H.4 densities','density',('app','prior','x'),frame('osm-figure-H.4')),
        ('H.4 distances','density-distances',('app','prior'),data('population-density-distances')),
        ('Alternate upper cutoffs','cutoffs',('app','upper_fraction'),data('pooling-cutoff-contrasts')),
        ('H.5 co-clustering','fixed-count',('app','prior','K'),frame('osm-figure-H.5')),
    ]:
        checked[label]=compare(label,ref(expected_name),observed,keys)
    chosen=ref('figure5-b')
    ranks={r['p'] for r in chosen}
    panel=[r for r in data('figure-5-panel-b') if r['block']=='full' and r['p'] in ranks]
    checked['Figure 5B selected signed percentiles']=compare('Figure 5B',chosen,panel,('app','p'))
    for r in panel:
        close(float(r['dual_quantile'])-float(r['gamma_quantile']),r['shift'],'Figure 5B signed quantile change')
    upper=[r for r in ref('pooling-points') if r['group']=='upper']
    total_delta={app:sum(float(r['delta_displacement']) for r in upper if r['app']==app)
                 for app in ('star','bd')}
    contributions=[dict(app=r['app'],unit=r['unit'],share_of_total_upper_reduction=float(r['delta_displacement'])/total_delta[r['app']]) for r in upper]
    checked['Upper-unit contributions']=compare('Upper-unit contributions',contributions,data('upper-unit-contributions'),('app','unit'))
    checked['Figure 6 and H.19 direction']=check_groups(frame('main-figure-6'),h19)
    checked['Rasch score weights and chain support']=check_weights(root,ref('score-changes'),frame('osm-table-H.4'),data('pair-and-split-metrics'))
    halves=ref('pooling-halves')
    split=data('pooling-split-contrasts')
    part=[]
    for r in split:
        for prior in ('gamma','dual'):
            part.append(dict(app=r['app'],prior=prior,group=r['group'],half=r['half'],
                             coassign=r['coassign_'+prior],displacement=r['displacement_'+prior]))
    split_index=index(part,('app','prior','half','group'),'pooling split groups')
    for app in ('star','bd'):
        for prior in ('gamma','dual'):
            for half in ('1','2'):
                upper=split_index[app,prior,half,'upper']
                central=split_index[app,prior,half,'central']
                part.append(dict(app=app,prior=prior,group='upper_minus_central',half=half,
                                 **{k:float(upper[k])-float(central[k]) for k in ('coassign','displacement')}))
    checked['Pooling chain-halves']=compare('Pooling split',halves,part,('app','prior','group','half'))
    printed=ref('printed')
    checked['printed precision']=check_printed(printed,tables)
    checked['literal HTML/TeX cells']=rendered_tables(out,printed)
    return checked


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[2])
    args=parser.parse_args()
    try:
        print(json.dumps(verify(args.root.resolve()), indent=2))
    except (Mismatch, KeyError, OSError, ValueError) as exc:
        print(f"Application verification failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
