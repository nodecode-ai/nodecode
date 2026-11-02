#!/usr/bin/env python3
"""turn-probe-dsh.py LABEL PROMPT [MODEL] --- the same ask through the DeepSeek Harness SDK runtime
(deepseek-official provider, api.deepseek.com), every notification stamped on receipt, the session's
events written beside them. Run with a venv carrying deepseek-harness-sdk (the ARC bench's
~/arc3-bench/dsh/.venv has one). The key comes from DEEPSEEK_API_KEY, else from the providers.deepseek
entry of the jsonc named by DSH_PROBE_CONFIG. PROBE_OUT and the workspace match turn-probe.sh."""
import json,os,re,sys,time
from pathlib import Path
S=Path(os.environ.get('PROBE_OUT','/tmp/nodecode-turn-probe'))
label,prompt=sys.argv[1],sys.argv[2]; model=sys.argv[3] if len(sys.argv)>3 else 'deepseek-v4-flash'
key=os.environ.get('DEEPSEEK_API_KEY')
if not key:
    raw=open(os.environ['DSH_PROBE_CONFIG']).read()
    cfg=json.loads(re.sub(r'^\s*//.*$','',raw,flags=re.M))
    key=cfg['providers']['deepseek']['api_key']
stamp=time.strftime('%Y%m%dT%H%M%SZ',time.gmtime())
run=S/f'{stamp}-{label}'; (run/'dsh-home').mkdir(parents=True)
from deepseek_harness import DeepSeekHarness
log=open(run/'notifications.jsonl','w')
t_first=None
def on(n):
    global t_first
    now=time.time()
    if t_first is None: t_first=now
    ev=n.payload.get('event') if isinstance(n.payload,dict) else None
    rec={'t':now,'method':n.method,'type':(ev or {}).get('type'),'etime':(ev or {}).get('time'),
         'status':n.payload.get('status') if isinstance(n.payload,dict) else None}
    if ev and ev.get('type') in ('tool/call','tool/result','step/start','step/end','turn/start','turn/end','assistant/message','user/message','system/message'):
        rec['data']=json.dumps(ev.get('data'))[:400]
    elif n.method!='session.event':
        rec['payload']=json.dumps(n.payload)[:300]
    log.write(json.dumps(rec)+'\n'); log.flush()
t0=time.time()
with DeepSeekHarness(provider='deepseek-official',model=model,cwd=str(S/'ws'),dsh_home=str(run/'dsh-home'),
                     profile='sdk',api_key=key) as h:
    t_init=time.time()
    s=h.start_session(f'probe-{stamp}')
    t_send=time.time()
    r=s.run(prompt,on_notification=on)
    t_end=time.time()
json.dump({'label':label,'prompt':prompt,'model':model,'t0':t0,'t_init':t_init,'t_send':t_send,'t_first_notification':t_first,'t_end':t_end,
           'final':r.final_response,'finish':r.finish_reason,'n_events':len(r.events)},open(run/'meta.json','w'),indent=1)
json.dump(r.events,open(run/'events.json','w'))
print(f'run={run}\ninit {t_init-t0:.2f}s, turn {t_end-t_send:.2f}s, events {len(r.events)}, finish={r.finish_reason}\nfinal: {r.final_response[:300]}')
