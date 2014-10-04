Code to reproduce Helmus et al. 2014 Nature 513:543-546
=========================================


File|Description or Variable|Explanation
-----|-----------|-----------------|------------------------------------------
Fig_1_Distributions_Helmus_Nature2014.R|R Code to Reproduce Figure 1		
Fig_2_SAIR_Helmus_Nature2014.R|R Code to Reproduce Figure 2
Fig_3_Jcurve_Helmus_Nature2014.R|R Code to Reproduce Figure 3
Table_1_AreaIsoShifted_Helmus_Nature2014.r|R Code to Reproduce Table 1
Table_2_EcoIso_Helmus_Nature2014.r|R Code to Reproduce Table 2
Helmus_Data_Fig1.csv|	Figure 1 Data|native and exotic Anolis distributions|
Helmus_Data_Fig2.csv|Figure 2 Data|
	|bank					|Bank Name
	|country				|Country of Bank
	|past.native.sr			|Number of native anole species on the bank (known as of 2014)
	|present.sr			|Number of exotic anole species on the bank (known as of 2014)
	|insitu.pa				|1 = in situ speciation has occurred on the bank according to anoletree.nex
	|area.km2				|Bank dry land area
	|isolation.1			|Principal Component Axis One (of the 4 isolation metrics below)
	|isolation.2			|Principal Component Axis Two
	|isolation.3			|Principal Component Axis Three
	|isolation.all			|Square root of total pairwise distance from all banks
	|isolation.sa			|Square root of distance from northern South America 
	|isolation.cuba			|Square root of distance from Cuba
	|isolation.origins		|Square root of total distance from both Cuba and northern South America
Helmus_Data_Fig3.csv|	Figure 3 Data
	|year					|Year exotic establishment was documented
	|cumulative # of exotics	|Running sum of the total number of exotic anole populations (per bank) in the Caribbean
Helmus_Data_Table1.csv|Table 1 Data|Same as Figure 2 Data
Helmus_Data_Table2.csv|Table 2 Data|Same as Figure 2 Data with economic isolation
				|economic.isolation| 1-(total ships docking in 2007 and 2008/max value)
anoletree.nex|Nexus phylogeny of Anolis|Mahler et al. 2010 Evolution 64:2731-2745

The MIT License (MIT)
=========================================
Copyright (c) 2014 Matthew R. Helmus

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.


