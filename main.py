import json,os,urllib.request as u
U=os.environ["MAKEFUN_ENV_URL"]
V=(0,100,320,330,500,900,0)
J=[33,31,18,14,-14,-18,-31,-33];D=[15,17,-15,-17];R=[1,-1,16,-16]
RR={0:2,7:1,112:8,119:4}
K=R+D
Bd=[0]*128;S=1;C=E=EV=KW=KB=ND=0
class A(Exception):pass
def nm(s):return "abcdefgh"[s&7]+str((s>>4)+1)
def pv(p,s):
 a=abs(p);v=V[a];r=s>>4;f=s&7;c=6-abs(f-3)-abs(r-3)
 if a==1:v+=r*10 if p>0 else (7-r)*10
 elif a in (2,3,5):v+=c*(6,4,0,3)[a-2]
 return v if p>0 else -v
def so(ms):ms.sort(key=lambda m:-V[abs(Bd[m>>7&127])]);return ms
def at(s,w):
 for ds,pt in (((-15,-17) if w else (15,17),1),(J,2),(K,6)):
  for d in ds:
   q=s+d
   if not q&136 and Bd[q]==(pt if w else -pt):return 1
 for ds,pt in ((D,3),(R,4)):
  for d in ds:
   q=s+d
   while not q&136:
    p=Bd[q]
    if p:
     if (p>0)==w and abs(p) in (pt,5):return 1
     break
    q+=d
 return 0
def gn(cp):
 ms=[]
 def ad(f,q,r):
  if (r==6 if S>0 else r==1):
   for z in (5,2):ms.append(f|(q<<7)|(z<<14))
  else:ms.append(f|(q<<7))
 for f in range(128):
  if f&136:continue
  p=Bd[f]
  if not p or p*S<0:continue
  t=abs(p)
  if t==1:
   d=16*S;q=f+d;r=f>>4
   if not Bd[q]:
    ad(f,q,r)
    if r==(1 if S>0 else 6) and not Bd[q+d]:ms.append(f|((f+2*d)<<7))
   for dd in (d-1,d+1):
    q=f+dd
    if not q&136 and (Bd[q]*S<0 or q==E):ad(f,q,r)
  elif t in (2,6):
   for d in J if t==2 else K:
    q=f+d
    if not q&136 and Bd[q]*S<=0:ms.append(f|(q<<7))
   if t==6 and not cp:
    for b,k,r,sg in ((1,4,7,1),(2,4,0,-1),(4,116,119,1),(8,116,112,-1)):
     if C&b and Bd[k]==6*S and Bd[r]==4*S and not Bd[k+sg] and not Bd[k+2*sg] and (sg>0 or not Bd[k-3]) and not any(at(x,S<0) for x in (k,k+sg,k+2*sg)):ms.append(k|((k+2*sg)<<7))
  else:
   for d in D+R if t==5 else D if t==3 else R:
    q=f+d
    while not q&136:
     c=Bd[q]
     if c:
      if c*S<0:ms.append(f|(q<<7))
      break
     ms.append(f|(q<<7));q+=d
 if cp:ms=[m for m in ms if Bd[m>>7&127] or m>>14 or (m>>7&127)==E]
 return ms
def mk(m):
 global C,E,EV,KW,KB
 f=m&127;t=m>>7&127;z=m>>14&7;p=Bd[f];c=Bd[t];o=(p,c,C,E,EV,KW,KB,-1,0,-1,-1)
 EV-=pv(p,f)
 if c:EV-=pv(c,t)
 if t==E and abs(p)==1 and not c:
  q=t-16*S;o=o[:7]+(q,Bd[q],-1,-1);EV-=pv(Bd[q],q);Bd[q]=0
 Bd[t]=z if z else p;Bd[f]=0;EV+=pv(Bd[t],t)
 if abs(p)==6:
  KW,KB=(t,KB) if S>0 else (KW,t)
  if abs(t-f)==2:
   rf,rt=(t+1,t-1) if t>f else (t-2,t+1);o=o[:7]+(o[7],o[8],rf,rt);Bd[rt]=Bd[rf];Bd[rf]=0
  C&=~((3,12)[S<0])
 elif abs(p)==4:C&=~RR.get(f,0)
 C&=~RR.get(t,0)
 E=f+16*S if abs(p)==1 and abs(t-f)==32 else -1
 return o
def un(m,o):
 global C,E,EV,KW,KB
 f=m&127;t=m>>7&127;p,c,Cr,Ep,Ev,Kw,Kb,eq,eqp,rf,rt=o
 if rf>=0:Bd[rf]=Bd[rt];Bd[rt]=0
 Bd[f]=p;Bd[t]=c
 if eq>=0:Bd[eq]=eqp
 C,E,EV,KW,KB=Cr,Ep,Ev,Kw,Kb
def sr(d,a,b,pl):
 global ND,S,BM
 ND+=1
 if ND>25e3:raise A()
 if d<=0:
  s=EV*S
  if s>=b:return b
  if s>a:a=s
 ms=so(gn(d<=0))
 bs=-10**6;nl=0;bm=0
 for m in ms:
  o=mk(m);S=-S
  if at(KW if S<0 else KB,S>0):S=-S;un(m,o);continue
  nl+=1;v=-sr(d-1,-b,-a,pl+1);S=-S;un(m,o)
  if v>bs:bs=v;bm=m
  a=max(a,v)
  if a>=b:break
 if not nl:return (-99999+pl if at(KW if S>0 else KB,S<0) else 0) if d>0 else a
 if pl==0:BM=bm
 return bs if d>0 else max(a,bs)
def ps(x):
 global S,C,E,EV,KW,KB
 Bd[:]=[0]*128
 EV=0
 a,b,c,d=x.split()[:4];r=7;f=0
 for ch in a:
  if ch=='/':r-=1;f=0
  elif ch.isdigit():f+=int(ch)
  else:
   s=r*16+f;p=('pnbrqk'.index(ch.lower())+1)*(1 if ch<'a' else -1);Bd[s]=p;EV+=pv(p,s)
   f+=1
 KW=Bd.index(6);KB=Bd.index(-6)
 S=1 if b=='w' else -1;C=0
 for ch in c:C|={'K':1,'Q':2,'k':4,'q':8}.get(ch,0)
 E=(int(d[1])-1)*16+ord(d[0])-97 if d!='-' else -1
def ch(L):
 global ND,S,BM
 BM=0
 for d in range(1,6):
  ND=0
  try:sr(d,-10**6,10**6,0)
  except A:break
 if not BM:return L[0]
 z=nm(BM&127)+nm(BM>>7&127)
 if BM>>14:z+="qrbn"[5-(BM>>14)]
 return z if z in L else L[0]
def pt(p,b):
 return json.load(u.urlopen(u.Request(U+p,data=json.dumps(b).encode())))
def mn():
 while 1:
  g=pt("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":
   ps(g["fen"]);g=pt("/move",{"game":g["game"],"move":ch(g["legal_moves"])})
if __name__=="__main__":mn()
