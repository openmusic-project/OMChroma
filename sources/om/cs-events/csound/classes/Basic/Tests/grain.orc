sr = 96000 
kr = 96000 
ksmps = 1 
nchnls = 1 
instr 1 
insnd = 10 
ibasfrq = sr / ftlen(insnd) ; Use original sample rate of insnd file 
kamp expseg 220, p3/2, 600, p3/2, 220 
kpitch line ibasfrq, p3, ibasfrq * .8 
kdens line 600, p3, 200 
kaoff line 0, p3, 5000 
kpoff line 0, p3, ibasfrq * .5 
kgdur line .4, p3, .1 
imaxgdur = p3
ar grain kamp, kpitch, kdens, kaoff, kpoff, kgdur, insnd, 5, imaxgdur, 1.0 
out ar 
endin
