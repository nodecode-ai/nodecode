#!/usr/bin/env python3
"""turn-probe-analyze.py RUNDIR --- the per-round phase table of one turn-probe.sh run: store facts
(admission, rounds, tool facts, terminal) aligned with the engine phase log on the stream-done line, plus
screen-change and CPU figures. Columns: harness time before the request, context compile, serialize,
POST->headers (provider), headers->stream end (provider), tool body, fact commits, git baseline+checkpoint."""
import json,re,sys,sqlite3,datetime,statistics,os
run=sys.argv[1]
meta=json.load(open(f'{run}/meta.json'))
dbp=f"{run}/nodecode-ephemeral-{meta['pid']}.db"
import os as _o
if not _o.path.exists(dbp): dbp=f'{run}/store.db'
db=sqlite3.connect(f'file:{dbp}?mode=ro',uri=True)
rows=db.execute("select log_position,occurred_at,kind,payload,turn_id from events order by log_position").fetchall()
def ms(ts): return datetime.datetime.fromisoformat(ts.replace('Z','+00:00')).timestamp()*1000
turns=[r for r in rows if r[2]=='turn.started' and json.loads(r[3]).get('input')==meta['prompt']]
turn_id=turns[0][4]
ev=[(p,ms(t),k,json.loads(pl) if pl else {}) for p,t,k,pl,tid in rows if tid==turn_id]
t_enter=meta['t_enter']*1000
# phase log
phases=[]
for line in open(f'{run}/phase.log'):
    m=re.match(r't=(\d+) (\S+)(.*)',line.strip())
    if not m: continue
    t=int(m.group(1)); label=m.group(2); rest=m.group(3)
    d=dict(re.findall(r'(\S+?)=(\S+)',rest))
    if label=='tool': d['name']=d.get('name'); 
    phases.append({'t':t,'label':label,'ms':float(d.get('ms',0)),'rest':rest.strip()})
# align: the 'tool' line is written right before fact-result; the next fact-result line ends at tool_result occurred_at.
tool_results=[e for e in ev if e[2]=='turn.tool_result']
prq=[e for e in ev if e[2]=='turn.provider_request']
sd_lines=[p for p in phases if p['label']=='stream-done']
offsets=[prq[i][1]-sd_lines[i]['t'] for i in range(min(len(prq),len(sd_lines)))]
off=statistics.median(offsets) if offsets else None
print(f"turn {turn_id}  prompt={meta['prompt']!r}")
print(f"clock offset (wall-internal) median={off} spread={ (max(offsets)-min(offsets)) if offsets else None} ms over {len(offsets)} tool rounds")
W=lambda tint: tint+off if off is not None else None
# per-round table
started=[e for e in ev if e[2]=='turn.started'][0][1]
pr=[e for e in ev if e[2]=='turn.provider_request']
usage=[e for e in ev if e[2]=='turn.usage']
calls=[e for e in ev if e[2]=='turn.tool_call_started']
completed=[e for e in ev if e[2] in ('turn.completed','turn.failed','turn.cancelled')]
ph_by=lambda lab:[p for p in phases if p['label']==lab]
posts=ph_by('post-headers'); ctx=ph_by('context-compile'); ser=ph_by('serialize'); sdl=ph_by('stream-done')
print()
print(f"enter->turn.started(admission committed): {started-t_enter:7.0f} ms")
prev_end=started
tot={'harness_pre':0,'headers':0,'ttfp':0,'stream':0,'tool':0,'facts':0,'post':0}
hdr=f"{'rnd':>3} {'pre-req harness':>15} {'ctx-compile':>11} {'serialize':>9} {'POST->hdrs':>10} {'hdrs->stream end':>16} {'stream':>8} {'parts':>6} {'tok/s':>6} {'out tok':>7} {'tool exec':>9} {'facts+pub':>9} {'baseline+ckpt':>13}"
print(hdr)
for i,e in enumerate(pr):
    d=e[3]; sd=d.get('stream-duration-ms') or 0; parts=d.get('stream-part-count')
    u=usage[i][3] if i<len(usage) else {}
    out=u.get('output-tokens')
    post=posts[i] if i<len(posts) else None
    c=ctx[i] if i<len(ctx) else None; s=ser[i] if i<len(ser) else None
    post_start=W(post['t']-post['ms']) if post else None
    hdrs_at=W(post['t']) if post else None
    first_part=W(sdl[i]['t']) if i<len(sdl) else e[1]
    gen=(first_part-hdrs_at) if hdrs_at else float('nan')
    pre=post_start-prev_end if post_start else float('nan')
    ttfp=first_part-hdrs_at if hdrs_at else float('nan')
    # tool phases following this round
    tool_ms=fact_ms=bc_ms=0.0
    if i<len(calls):
        # phases between this provider_request wall time and next provider_request
        lo=e[1]-off if off else 0; hi=(pr[i+1][1]-off) if i+1<len(pr) and off else 1e18
        for p in phases:
            if lo<=p['t']<=hi:
                if p['label']=='tool': tool_ms+=p['ms']
                elif p['label'] in ('fact-call','fact-result'): fact_ms+=p['ms']
                elif p['label'] in ('baseline','checkpoint'): bc_ms+=p['ms']
    print(f"{i:>3} {pre:>15.0f} {c['ms'] if c else float('nan'):>11.1f} {s['ms'] if s else float('nan'):>9.1f} {post['ms'] if post else float('nan'):>10.0f} {gen:>16.0f} {sd:>8} {parts:>6} {(out/(sd/1000)) if out and sd else 0:>6.0f} {out if out else 0:>7} {tool_ms:>9.0f} {fact_ms:>9.1f} {bc_ms:>13.0f}")
    tot['harness_pre']+=0 if pre!=pre else pre; tot['headers']+=post['ms'] if post else 0; tot['ttfp']+=0 if gen!=gen else gen; tot['stream']+=sd; tot['tool']+=tool_ms; tot['facts']+=fact_ms; tot['post']+=bc_ms
    # next round starts after this round's tool_result
    trs=[x for x in tool_results if x[1]>e[1]]
    prev_end=trs[0][1] if trs else e[1]
end=completed[0][1] if completed else None
print()
print(f"totals ms: {tot}")
# the prompt-cache view per round: what the provider re-prefilled, and whether the harness moved the prefix
print()
print(f"{'rnd':>3} {'uncached':>9} {'cached':>8} {'prev prompt':>11} {'cache miss':>10} {'prefix shared':>13} {'diverged in':>14} {'req bytes':>9}")
prev_total=None
for i,e in enumerate(pr):
    d=e[3]; u=usage[i][3] if i<len(usage) else {}
    inp=u.get('input-tokens') or 0; cac=u.get('cached-input-tokens') or 0
    miss=(prev_total-cac) if prev_total is not None else 0
    post=posts[i] if i<len(posts) else None
    rb=re.search(r'req-bytes=(\d+)',post['rest']).group(1) if post and 'req-bytes=' in post['rest'] else ''
    print(f"{i:>3} {inp:>9} {cac:>8} {prev_total if prev_total is not None else '':>11} {miss if prev_total is not None else '':>10} {d.get('prefix-shared-bytes','') if d.get('prefix-shared-bytes') is not None else '':>13} {d.get('prefix-diverged-in') or '':>14} {rb:>9}")
    prev_total=inp+cac
if end: print(f"wall enter->turn terminal fact: {end-t_enter:.0f} ms; last provider_request->terminal: {end-pr[-1][1]:.0f} ms")
# frames: first visible change after enter
frames=[tuple(l.split()) for l in open(f'{run}/frames.log')] if os.path.exists(f'{run}/frames.log') else []
ft=[float(a)*1000 for a,b in frames]
after=[t for t in ft if t>t_enter]
if after: print(f"first screen change after Enter: {after[0]-t_enter:.0f} ms; screen changes during turn: {len(after)}")
if meta.get('t_done'): print(f"driver saw terminal fact at: {meta['t_done']*1000-t_enter:.0f} ms after Enter")
# cpu
cpu=[l.split() for l in open(f'{run}/cpu.log')] if os.path.exists(f'{run}/cpu.log') else []
if len(cpu)>2:
    t0,u0,s0=float(cpu[0][0]),int(cpu[0][1]),int(cpu[0][2]); t1,u1,s1=float(cpu[-1][0]),int(cpu[-1][1]),int(cpu[-1][2])
    # during the turn only
    seg=[c for c in cpu if t_enter/1000<=float(c[0])<=(end/1000 if end else 1e18)]
    if len(seg)>2:
        a,b=seg[0],seg[-1]; dt=float(b[0])-float(a[0]); cj=(int(b[1])-int(a[1])+int(b[2])-int(a[2]))/100.0
        print(f"owner+shell process CPU during turn: {cj:.2f} cpu-s over {dt:.1f} s = {100*cj/dt:.1f}% of one core; RSS {int(a[3])//1024}->{int(b[3])//1024} MB")
