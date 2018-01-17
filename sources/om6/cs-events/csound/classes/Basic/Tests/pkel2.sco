/*score parameters
igrainrate	= p4		; grain rate
igrainsize	= p5		; grain size in ms
igrainFreq	= p6		; frequency of source wave within grain
iosc2Dev	= p7		; partikkel instance 2 grain rate deviation factor
iMaxSync	= p8		; max soft sync amount (increasing to this value during length of note)
*/
;		GrRate	GrSize	GrFund	Osc2Dev	MaxSync

i1 0 	10	2	20	880	1.3	0.3
s
i1 0 	10	5	20	440	0.8	0.3	
s
i1 0 	6	55	15	660	1.8	0.45
s
i1 0 	6	110	10	440	0.6	0.6	
s
i1 0 	6	220	3	660	2.6	0.45
s
i1 0 	6	220	3	660	2.1	0.45
s
i1 0 	6	440	3	660	0.8	0.22

e
