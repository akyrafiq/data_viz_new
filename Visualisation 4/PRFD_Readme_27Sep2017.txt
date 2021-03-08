PRFD (PORTUGUESE RURAL FIRE DATABASE) 1980-2005
[This version updated 5 August 2011]


I. DATABASE AVAILABILITY AND CITATION

Data available at http: Nat. Hazards Earth Syst. Sci., 11, 3343–3358, doi:10.5194/nhess-11-3343-2011, 2011
These data can be freely used for non-commercial research provided that the following source is acknowledged:

Pereira, M. G., Malamud, B. D., Trigo, R. M., and Alves, P. I.: 
The history and characteristics of the 1980−2005 Portuguese rural fire database. 
Nat. Hazards Earth Syst. Sci., 11, 3343–3358, doi:10.5194/nhess-11-3343-2011, 2011.


II. DATABASE DESCRIPTION

The modified Portuguese Rural Fire Database (PRFD) is representative of fires that have occurred in Continental Portugal, 1980–2005, with the original data  provided by the Autoridade Florestal Nacional (AFN, 2011a), the Portuguese Forest Service. Pereira et al. (2011) describe the procedures used to modify and correct the original database, along with a more detailed description of the database itself than what is included in this readme file. 

III. BROAD OVERVIEW OF EACH FIRE RECORD VARIABLES

The 453,577 fire records in the modified PRFD database includes the following information for each fire record (29 variables):

(i)	Rural fire ignition and extinction date and time.
(ii)	Rural fire ignition location, in terms of administrative divisions of Portugal. In 2007, this consisted of 18 districts, subdivided into 278 counties, and further subdivided into 4050 parishes.
(iii)	The amount of area burned in the fire (in ha). 
(iv)	The land use cover type (forest and shrublands) and total rural fire area on that land-use type for which the rural fire occurred. Forests are defined as land occupied by >10% forest trees, a minimum area of 0.5 ha and width >20 m. Shrublands are defined as land occupied by shrub, natural origin, no agriculture or forestry, a minimum area 
of 0.5 ha and width >20 m.
(v)	Cause of rural fire. 
(vi)	Some additional data is available but only for sub-periods of the original 1980−2005 period: 
	• spatial coordinates of the rural fire ignition location (2001−2005)
	• burned area in agricultural lands (in ha) (2001−2005) 
	• land ownership (public or private) (1993−2003) 
	• type of fire (single or reignition) (1984−2005) 
	• time of the first fire fight attempt (2001−2003)


IV. DATA FORMAT

The data consists of a matrix of values with 453,577 lines (fire record number) by 29 columns (variables) saved in a tab separated text format.

Column	Type	Variable	
1	integer	District code (1-18, according to District name sorted alphabetically)
2	string	District name
3	string	County name
4	string	Parish name
5	float	Public shrubland burnt area (in ha)
6	float	Private shrubland burnt area (in ha)
7	float	Total shrubland burnt area (public + private) (in ha)
8	float	Public forest burnt area (in ha)
9	float	Private forest burnt area (in ha)
10	float	Total forest burnt area (public + private) (in ha)
11	float	Total burnt area (shrubland + forest) (in ha)
12	float	Agricultural burnt area (in ha)
13	integer	Type of fire (0 [single fire]; 1 [reignition fire])
14	integer	Fire cause code (see Fire cause code description below)
15	integer	Fire ignition date excel code (number of days since 1 January 1900; e.g., 1 January 2005 is 38353) (# of days)
16	float	Fire ignition time excel code (fraction of 24 hours; e.g., 0.854167 corresponds to 20:30:00)
17	integer	Fire ignition hour (in hour)
18	integer	Fire ignition minute (in minute)
19	integer	Fire extinction date excel code (number of days since 1 January 1900; e.g., 1 January 2005 is 38353) (# of days)
20	float	Fire extinction time excel code (fraction of 24 hours; 0.854167 corresponds to 20:30:00)
21	integer	Fire extinction hour (in hour)
22	integer	Fire extinction minute (in minute)
23	float	Fire duration (fraction of 24 hours)
24	integer	Flag1 (number of the variable/column changed as a consequence of date, time, or fire duration errors) 
25	integer	Flag2 (as Flag1, but used when more than one variable changed) 
26	integer	Flag3 (1 ["Parish name" was changed to correct inconsistencies between parish/locality and the county/district]; 0 ["Parish name" unchanged])
27	integer Flag4 (1 ["Parish name" was changed to replace locality name by the corresponding parish name]; 0 ["Parish name" unchanged])
28	integer Time (hour: minute) of first fire fighting intervention
29	integer Day of the week (1 [Sunday], 2 [Monday], ..., 7 [Saturday]) 


V. MISSING VALUE CODES
 -999.9 for float type variables
  99:99 for time of first fire fighting intervention
  UNKNOWN for string type variables

VI. FIRE CAUSE CODES
	
Code	Description
0	Unknown (not labelled by fire managers)
1	Negligence
2	Prescribed burning (cleaning of agricultural land, renewal of grassland for grazing)
3	Forest works (emission of incandescent particles, sparks and heat transfer by conduction)
4	Bonfires (use of fires for camping sites, cooking food, heating, construction, repair or maintenance of paved roads)
5	Smoking (cigarettes and/or match released to the ground by smokers)
6	Sparks
7	Railways (incandescent material from the braking system or movement of rail traffic)
8	Electric lines (transmission lines of electricity which when in contact discharge, breach or cause an electric arc, give rise to ignition.)
9	Fireworks
10	Intentional
11	Natural (lightning)
12	Unknown (fires whose cause was officially recognized as "unknown")
13	Other
14	Agricultural work (ignitions originating from the friction of metal parts of tools and other agricultural machines with stones)
15	Other forest works (other forestry equipment, not included above, giving rise to ignition)
16	Industrial activities (emission of incandescent particles, sparks and heat transfer by conduction)
17	Communications (causes associated with communications systems)
18	Public (causes associated with public state activities)
19	Reignition (of a previous fire)
20	Accident
21	Structural (ignitions associated with land use change and management, hunting and wildlife, fire prevention and pressure to sell woody materials)
22	Vandalism


The AFN (2011b) now define 69 distinct fire causes in their database, broadly classified as (i) human (accidental, illegal, prescribed burned), (ii) natural (lightning), (iii) undetermined. However, we decided to keep the previous fire cause codes to prevent reclassification errors. 

VII. REFERENCES CITED

AFN (Autoridade Florestal Nacional)(2011a). National Forest Authority: Statistics: Wildfire Data [Online] Available at: http://www.afn.min-agricultura.pt/portal/dudf/estatisticas/estatistica-2013-dados-sobre-incendios-florestais [Accessed 1 March 2011].

AFN (Autoridade Florestal Nacional)(2011b). National Forest Authority:  Fire cause coding categories and definition of the Causes [Online] Available at:http://www.afn.min-agricultura.pt/portal/dudf/Resource/pdf/estatisticas/dgrf-codificacao-causas.pdf [Accessed 1 March 2011].

Pereira, M. G., Malamud, B. D., Trigo, R. M., and Alves, P. I.: The history and characteristics of the 1980−2005 Portuguese rural fire database. Nat. Hazards Earth Syst. Sci., 11, 3343–3358, doi:10.5194/nhess-11-3343-2011, 2011.




