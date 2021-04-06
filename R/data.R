#' General Social Survey, 2014
#'
#' This subset of data comes from one iteration of the \emph{General Social Survey}, administered in 2014. These data were collected by the National Opinion Research Center (NORC) at the University of Chicago. The observations represent individuals' responses to survey questions. Information about the data set can be found in the GSS Codebook at: \url{http://burrelvannjr.com/docs/GSS_Codebook.pdf}.
#'
#' @format A data frame with 2538 observations and 676 variables.
#' \tabular{ll}{
#'  id \tab respondent id number \cr
#'  age \tab age of respondent \cr
#'	sex \tab respondents sex \cr
#'	race \tab race of respondent \cr
#'	educ \tab highest year of school completed \cr
#'	dipged \tab diploma, ged, or other \cr
#'	paeduc \tab highest year school completed, father \cr
#'	maeduc \tab highest year school completed, mother \cr
#'	speduc \tab highest year school completed, spouse \cr
#'	sei10 \tab r's socioeconomic index (2010) \cr
#'	conrinc \tab respondent income in constant dollars \cr
#'	coninc \tab family income in constant dollars \cr
#'	degree \tab rs highest degree \cr
#'	padeg \tab fathers highest degree \cr
#'	madeg \tab mothers highest degree \cr
#'	spdeg \tab spouses highest degree \cr
#'	citizen \tab are you a citizen of america? \cr
#'	born \tab was r born in this country \cr
#'	year \tab gss year for this respondent \cr
#'	cohort \tab year of birth \cr
#'	spsei10 \tab r's spouse's socioeconomic index (2010) \cr
#'	pasei10 \tab r's father's socioeconomic index (2010) \cr
#'	masei10 \tab r's mother's socioeconomic index (2010) \cr
#'	childs \tab number of children \cr
#'	immcrime \tab immigrants increase crime rates \cr
#'	abany \tab abortion if woman wants for any reason \cr
#'	abdefect \tab strong chance of serious defect \cr
#'	abhlth \tab womans health seriously endangered \cr
#'	abnomore \tab married--wants no more children \cr
#'	abpoor \tab low income--cant afford more children \cr
#'	abrape \tab pregnant as result of rape \cr
#'	absingle \tab not married \cr
#'	accptoth \tab r accept others even when they do things wrong \cr
#'	acqntsex \tab r had sex with acquaintance last year \cr
#'	actassoc \tab how important to be active on soc or pol association \cr
#'	actlaw \tab how likely r to do something if unjust law being cons \cr
#'	adults \tab household members 18 yrs and older \cr
#'	advfront \tab sci rsch is necessary and should be supported by federal govt \cr
#'	affctlaw \tab how lliely congress give serious attention to rs dema \cr
#'	affrmact \tab favor preference in hiring blacks \cr
#'	aged \tab should aged live with their children \cr
#'	aidscndm \tab condom can reduce aids \cr
#'	aidslook \tab a health-look person may have aids \cr
#'	amancstr \tab how important to have american ancestry \cr
#'	ambetter \tab agree america is a better country \cr
#'	ambornin \tab how important to have been born in america \cr
#'	amchrstn \tab how important to be a christian \cr
#'	amcit \tab how important to have american citizenship \cr
#'	amcitizn \tab agree i would rather be a citizen of america \cr
#'	amcult \tab it is impossible to become fully american \cr
#'	amenglsh \tab how important to be able to speak english \cr
#'	amfeel \tab how important to feel american \cr
#'	amgovt \tab how important to respect america's laws etc \cr
#'	amlived \tab how important to have lived in america for life \cr
#'	amownway \tab america should follow its own interests \cr
#'	amproud1 \tab how proud being american \cr
#'	amshamed \tab agree there are things make me ashamed \cr
#'	amsports \tab agree sports makes me proud to be an american \cr
#'	amtv \tab tv should give preference to american films \cr
#'	arthrtis \tab told have arthritis or rheumatism \cr
#'	astrolgy \tab ever read a horscope or persoanl astrology report \cr
#'	astrosci \tab astrology is scientific \cr
#'	attend \tab how often r attends religious services \cr
#'	attrally \tab attended a political meeting or rally \cr
#'	avoidbuy \tab boycotted products for pol reasons \cr
#'	babies \tab household members less than 6 yrs old \cr
#'	backpain \tab r had back pain in the past 12 months \cr
#'	balneg \tab sci research is strongly in favor of harmful results \cr
#'	balpos \tab sci research is strongly in favor of benefits \cr
#'	befair \tab how often do you think people take advantage \cr
#'	belikeus \tab agree better if people were more like americans \cr
#'	bettrlfe \tab science makes our lives better \cr
#'	betrlang \tab which language r speaks more fluent \cr
#'	bible \tab feelings about the bible \cr
#'	bigbang \tab sci knowledge:the universe began with a huge explosion \cr
#'	boyorgrl \tab sci knowledge:father gene decides sex of baby \cr
#'	buypol \tab how important to choose products for pol reasons \cr
#'	buyvalue \tab percent of company stock r bought from own money \cr
#'	cantrust \tab poeple can be trusted or cant be too careful \cr
#'	cappun \tab favor or oppose death penalty for murder \cr
#'	careself \tab those in need have to take care of themselves \cr
#'	carried \tab r carried a stranger's belongings \cr
#'	chldidel \tab ideal number of children \cr
#'	chngeoth \tab how often r try to persuade other to share views \cr
#'	chngtme \tab how often r allowed change schedule \cr
#'	choices \tab political parties dont give real policy choices \cr
#'	citworld \tab i am a citizen of the world \cr
#'	class \tab subjective class identification \cr
#'	closeblk \tab how close feel to blacks \cr
#'	closewht \tab how close feel to whites \cr
#'	clsenoam \tab how close do you feel to north america \cr
#'	clsestat \tab how close do you feel to your state \cr
#'	clsetown \tab how close do you feel to your town or city \cr
#'	clseusa \tab how close do you feel to america \cr
#'	cntctgov \tab contacted politician or civil servant to express view \cr
#'	colath \tab allow anti-religionist to teach \cr
#'	colcom \tab should communist teacher be fired \cr
#'	coldeg1 \tab the highest degree r have earned \cr
#'	colhomo \tab allow homosexual to teach \cr
#'	colmil \tab allow militarist to teach \cr
#'	colmslm \tab allow anti-american muslim clergymen teaching in college \cr
#'	colrac \tab allow racist to teach \cr
#'	colsci \tab r has taken any college-level sci course \cr
#'	colscinm \tab number of college-level sci courses r have taken \cr
#'	compperf \tab size of perf based pay depend on profits \cr
#'	comprend \tab rs understanding of questions \cr
#'	compuse \tab r use computer \cr
#'	conarmy \tab confidence in military \cr
#'	conbus \tab confidence in major companies \cr
#'	conclerg \tab confidence in organized religion \cr
#'	condemnd \tab r free from conflicting demands \cr
#'	condom \tab used condom last time \cr
#'	condrift \tab sci knowledge:the continents have been moving \cr
#'	coneduc \tab confidence in education \cr
#'	confed \tab confid. in exec branch of fed govt \cr
#'	confinan \tab confid in banks & financial institutions \cr
#'	conjudge \tab confid. in united states supreme court \cr
#'	conlabor \tab confidence in organized labor \cr
#'	conlegis \tab confidence in congress \cr
#'	conmedic \tab confidence in medicine \cr
#'	conpress \tab confidence in press \cr
#'	consci \tab confidence in scientific community \cr
#'	contv \tab confidence in television \cr
#'	corruptn \tab how widespread corruption is in pub service in americ \cr
#'	courts \tab courts dealing with criminals \cr
#'	cowrkhlp \tab coworkers can be relied on when r needs help \cr
#'	cowrkint \tab coworkers take a personal interest in r \cr
#'	crack30 \tab r last use crack cocaine \cr
#'	crimlose \tab people convicted of serious crimes lose citizen rights \cr
#'	cutahead \tab r allowed a stranger to go ahead of you in line \cr
#'	decsorgs \tab america should follow decision of intl org \cr
#'	defpensn \tab r has defined benefit pension plan \cr
#'	dem10fut \tab how well will democracy work in america in ten yrs \cr
#'	dem10pst \tab how well did democracy work in america ten yrs ago \cr
#'	demtoday \tab how well democracy work in america \cr
#'	denom \tab specific denomination \cr
#'	denom16 \tab denomination in which r was raised \cr
#'	depress \tab told have depression \cr
#'	diabetes \tab told have diabetes \cr
#'	directns \tab r has given directions to a stranger \cr
#'	discaff \tab whites hurt by aff. action \cr
#'	discaffm \tab a man won't get a job or promotion \cr
#'	discaffw \tab a woman won't get a job or promotion \cr
#'	discpol \tab how often r discuss politics \cr
#'	divlaw \tab divorce laws \cr
#'	divorce \tab ever been divorced or separated \cr
#'	dwelown \tab does r own or rent home? \cr
#'	earnrs \tab how many in family earned money \cr
#'	earthsun \tab sci knowledge:the earth goes around the sun \cr
#'	effctsup \tab supervisor effective solve work/personal conflicts \cr
#'	elecfair \tab how fair last natl election:opprtunities of candidate \cr
#'	electron \tab sci knowledge:electrons are smaller than atoms \cr
#'	elecvote \tab how honest last natl election:counting of votes \cr
#'	emailhr \tab email hours per week \cr
#'	emailmin \tab email minutes per week \cr
#'	empinput \tab r involved in any task force for decision-making \cr
#'	emptrain \tab received formal training from employer \cr
#'	eqwlth \tab should govt reduce income differences \cr
#'	esop \tab r is member of esop \cr
#'	ethnic \tab country of family origin \cr
#'	evcrack \tab r ever use crack cocaine \cr
#'	evidu \tab r ever inject drugs \cr
#'	evolved \tab sci knowledge:human beings developed from animals \cr
#'	evpaidsx \tab ever have sex paid for or being paid since 18 \cr
#'	evstray \tab have sex other than spouse while married \cr
#'	evwork \tab ever work as long as one year \cr
#'	excldimm \tab america should exclude illegal immigrants \cr
#'	expdesgn \tab better way to test drug btw control and non-control \cr
#'	exptext \tab why is it better to test drug this way \cr
#'	extrapay \tab eligible for performance based pay \cr
#'	extrayr \tab year of the most recent perf based payments \cr
#'	fair \tab people fair or try to take advantage \cr
#'	fairearn \tab how fair is what r earn on the job \cr
#'	famgen \tab number of family generations in household \cr
#'	family16 \tab living with parents when 16 yrs old \cr
#'	famvswk \tab how often fam life interfere job \cr
#'	famwkoff \tab how hard to take time off \cr
#'	fear \tab afraid to walk at night in neighborhood \cr
#'	fechld \tab mother working doesnt hurt children \cr
#'	feelevel \tab amount of fees paid \cr
#'	feeused \tab fee given to get case \cr
#'	fefam \tab better for man to work, woman tend home \cr
#'	fehire \tab should hire and promote women \cr
#'	fejobaff \tab for or against preferential hiring of women \cr
#'	fepol \tab women not suited for politics \cr
#'	fepresch \tab preschool kids suffer if mother works \cr
#'	finalter \tab change in financial situation \cr
#'	finrela \tab opinion of family income \cr
#'	forland \tab foreigners should not be allowed to buy land \cr
#'	form \tab form of split questionnaire asked \cr
#'	freetrde \tab free trade leads to better products \cr
#'	fringeok \tab fringe benefits are good \cr
#'	frndsex \tab r had sex with friend last year \cr
#'	fucitzn \tab is r planning/appling for us citizenship or not \cr
#'	fund \tab how fundamentalist is r currently \cr
#'	fund16 \tab how fundamentalist was r at age 16 \cr
#'	getahead \tab opinion of how people get ahead \cr
#'	givblood \tab r donated blood during the past 12 months \cr
#'	givchrty \tab r has given money to a charity \cr
#'	givhmlss \tab r has given food or money to a homeless person \cr
#'	givseat \tab r offered seat to a stranger during past 12 months \cr
#'	god \tab rs confidence in the existence of god \cr
#'	goodlife \tab standard of living of r will improve \cr
#'	govdook \tab we can trust people in govt \cr
#'	granborn \tab how many grandparents born outside u.s. \cr
#'	grass \tab should marijuana be made legal \cr
#'	grpother \tab r belongs to another voluntary association \cr
#'	grpparty \tab r belongs to a political party \cr
#'	grprelig \tab r belongs to a church or othr religious organization \cr
#'	grpsprts \tab r belongs to a sports, leisure, or cultural grp \cr
#'	grpwork \tab r belongs to a trade union or professtional associati \cr
#'	gunlaw \tab favor or oppose gun permits \cr
#'	gvtrghts \tab (on a scale of 1 to 7, where 1 is not at all important and 7 is very important \cr
#'	handmove \tab r perform forceful hand movements \cr
#'	hapcohab \tab happiness of relt with partner \cr
#'	hapmar \tab happiness of marriage \cr
#'	happy \tab general happiness \cr
#'	haveinfo \tab enough info to get the job done \cr
#'	health \tab condition of health \cr
#'	health1 \tab rs health in general \cr
#'	hefinfo \tab number of hef informant \cr
#'	height \tab r is how tall \cr
#'	helpaway \tab r looked after plant or pet of others while away \cr
#'	helpblk \tab should govt aid blacks? \cr
#'	helpful \tab people helpful or looking out for selves \cr
#'	helphwrk \tab helped someone with hwork during past 12 months \cr
#'	helpjob \tab helped somebody to find a job past 12 months \cr
#'	helpnot \tab should govt do more or less? \cr
#'	helpoth \tab to help others \cr
#'	helppoor \tab should govt improve standard of living? \cr
#'	helpsick \tab should govt help pay for medical care? \cr
#'	helpusa \tab how important to help worse off ppl in america \cr
#'	helpwrld \tab how important to help worse off ppl in rest of world \cr
#'	hhtype \tab household type \cr
#'	hhtype1 \tab household type (condensed) \cr
#'	hispanic \tab hispanic specified \cr
#'	hivkiss \tab kiss can spread hiv \cr
#'	hivtest \tab have you ever been tested for hiv \cr
#'	hivtest1 \tab in what month and year was your last hiv test \cr
#'	hivtest2 \tab where did you have your last hiv test \cr
#'	hivvac \tab there is a vaccine that can prevent hiv \cr
#'	hlpequip \tab enough help and equip to ge the job done \cr
#'	hlthall \tab healthcare provided for everyone \cr
#'	hlthdays \tab days of activity limitation past 30 days \cr
#'	homosex \tab homosexual sex relations \cr
#'	hompop \tab number of persons in household \cr
#'	hotcore \tab sci knowledge: the center of earth is very hot \cr
#'	hrs1 \tab number of hours worked last week \cr
#'	hrs2 \tab number of hours usually work a week \cr
#'	hrsrelax \tab hours per day r have to relax \cr
#'	hsbio \tab r ever took a high school biology course \cr
#'	hschem \tab r ever took a high school chemistry course \cr
#'	hsmath \tab the highest level of math r completed in high school \cr
#'	hsphys \tab r ever took a high school physics course \cr
#'	hunt \tab does r or spouse hunt \cr
#'	hurtatwk \tab number of injuries on the job past 12 months \cr
#'	hvylift \tab r do repeated lifting \cr
#'	hyperten \tab told have hypertension or high blood pressure \cr
#'	idu30 \tab r inject drugs in past 30 days \cr
#'	if08who \tab who you would have voted for \cr
#'	if12who \tab who would r have voted for in 2012 election \cr
#'	ifwrong \tab agree people should support their country \cr
#'	immameco \tab immigrants good for america \cr
#'	immassim \tab what statement about immigrants matches view \cr
#'	immcult \tab immigrants undermine american culture \cr
#'	immeduc \tab legal immigrants should have same education as americans \cr
#'	immideas \tab immigrants make america more open \cr
#'	immjobs \tab immigrants take jobs away \cr
#'	immrghts \tab legal immigrants should have same right as american \cr
#'	imports \tab america should limit the import \cr
#'	incom16 \tab rs family income when 16 yrs old \cr
#'	income \tab total family income \cr
#'	income06 \tab total family income \cr
#'	indperf \tab size of perf based pay depend on individual \cr
#'	intecon \tab interested in economic issues \cr
#'	inteduc \tab interested in local school issues \cr
#'	intenvir \tab interested in environmental issues \cr
#'	interpol \tab joined an internet political forum \cr
#'	intfarm \tab interested in farm issues \cr
#'	intintl \tab interested in international issues \cr
#'	intlblks \tab unintelligent - intelligent \cr
#'	intlincs \tab largee intl company damage to local business \cr
#'	intlwhts \tab unintelligent -intelligent \cr
#'	intmed \tab interested in medical discoveries \cr
#'	intmil \tab interested in military policy \cr
#'	intrhome \tab internet access in r's home \cr
#'	intsci \tab interested in new scientific discoveries \cr
#'	intspace \tab interested in space exploration \cr
#'	inttech \tab interested in technologies \cr
#'	jobfind \tab could r find equally good job \cr
#'	jobfind1 \tab how easy for r to find a same job \cr
#'	jobhour \tab short working hours \cr
#'	jobinc \tab high income \cr
#'	joblose \tab is r likely to lose job \cr
#'	jobmeans \tab work important and feel accomplishment \cr
#'	jobpromo \tab chances for advancement \cr
#'	jobsec \tab no danger of being fired \cr
#'	jobsecok \tab the job security is good \cr
#'	joindem \tab took part in a demonstration \cr
#'	kidssol \tab rs kids living standard compared to r \cr
#'	knowschd \tab how far in advance know work schedule \cr
#'	knowwhat \tab r knows what's expected on job \cr
#'	laidoff \tab r was laid off main job last year \cr
#'	lasers \tab sci knowledge:lasers work by focusing sound waves \cr
#'	learnnew \tab job requires r to learn new things \cr
#'	leftrght \tab how left or right in politics \cr
#'	lentto \tab lent money to another person past 12 months \cr
#'	lessprd \tab agree often less proud of america \cr
#'	letdie1 \tab allow incurable patients to die \cr
#'	letin1 \tab number of immigrants to america nowadays should be \cr
#'	letin1a \tab number of immigrants nowadays should be \cr
#'	libath \tab allow anti-religious book in library \cr
#'	libcom \tab allow communists book in library \cr
#'	libhomo \tab allow homosexuals book in library \cr
#'	libmil \tab allow militarists book in library \cr
#'	libmslm \tab allow anti-american muslim clergymen's books in library \cr
#'	librac \tab allow racists book in library \cr
#'	life \tab is life exciting or dull \cr
#'	liveblks \tab neighborhood half black \cr
#'	livewhts \tab r favors living in half white neighborhood \cr
#'	loanitem \tab r has let someone borrow a item of some value \cr
#'	localnum \tab number of employees: rs work site \cr
#'	maind10 \tab mothers industry code (naics 2007) \cr
#'	major1 \tab college major 1 \cr
#'	major2 \tab college major 2 \cr
#'	majorcol \tab the field of degree r earned \cr
#'	manvsemp \tab relations bw management and employees \cr
#'	maocc10 \tab mothers census occupation code (2010) \cr
#'	marasian \tab close relative marry asian \cr
#'	marblk \tab close relative marry black \cr
#'	marhisp \tab close relative marry hispanic \cr
#'	marhomo \tab homosexuals should have right to marry \cr
#'	marital \tab marital status \cr
#'	martype \tab marital type \cr
#'	marwht \tab r favor close relative marrying white person \cr
#'	matesex \tab was 1 of rs partners spouse or regular \cr
#'	mawrkgrw \tab mothers employment when r was 16 \cr
#'	mawrkslf \tab mother self-emp. or worked for somebody \cr
#'	meltpot1 \tab better to maintain distinct cultures \cr
#'	meovrwrk \tab men hurt family when focus on work too much \cr
#'	mincult \tab ethnic minorities should be given gov assistance \cr
#'	misswork \tab miss work for health past 30 days \cr
#'	mntlhlth \tab days of poor mental health past 30 days \cr
#'	mobile16 \tab geographic mobility since age 16 \cr
#'	mode \tab interview done in-person or over the phone \cr
#'	moredays \tab days per month r work extra hours \cr
#'	mustwork \tab mandatory to work extra hours \cr
#'	nafta1 \tab how much heard or read about nafta? \cr
#'	nafta2a \tab america benefits from being a member of nafta? \cr
#'	nataid \tab foreign aid \cr
#'	nataidy \tab assistance to other countries -- ver y \cr
#'	natarms \tab military, armaments, and defense \cr
#'	natarmsy \tab national defense -- version y \cr
#'	natchld \tab assistance for childcare \cr
#'	natcity \tab solving problems of big cities \cr
#'	natcityy \tab assistance to big cities -- version y \cr
#'	natcrime \tab halting rising crime rate \cr
#'	natcrimy \tab law enforcement -- verison y \cr
#'	natdrug \tab dealing with drug addiction \cr
#'	natdrugy \tab drug rehabilitation -- version y \cr
#'	nateduc \tab improving nations education system \cr
#'	nateducy \tab education -- version y \cr
#'	natenrgy \tab developing alternative energy sources \cr
#'	natenvir \tab improving & protecting environment \cr
#'	natenviy \tab the environment -- version y \cr
#'	natfare \tab welfare \cr
#'	natfarey \tab assistance to the poor -- version y \cr
#'	natheal \tab improving & protecting nations health \cr
#'	nathealy \tab health -- version y \cr
#'	natmass \tab mass transportation \cr
#'	natpark \tab parks and recreation \cr
#'	natrace \tab improving the conditions of blacks \cr
#'	natracey \tab assistance to blacks -- version y \cr
#'	natroad \tab highways and bridges \cr
#'	natsci \tab supporting scientific research \cr
#'	natsoc \tab social security \cr
#'	natspac \tab space exploration program \cr
#'	natspacy \tab space exploration -- version y \cr
#'	news \tab how often does r read newspaper \cr
#'	newsfrom \tab main source of information about events in the news \cr
#'	nextgen \tab science & tech. give more opportunities to next generation \cr
#'	notvote \tab citizens have right not to vote \cr
#'	ntcitvte \tab long-term residents should vote \cr
#'	ntwkhard \tab past week not work hard enough \cr
#'	numemps \tab number of employee for the self-employed \cr
#'	nummen \tab number of male sex partners since 18 \cr
#'	numorg \tab number of people working in organization at all locations \cr
#'	numwomen \tab number of female sex partners since 18 \cr
#'	obey \tab to obey \cr
#'	obeylaws \tab how important always to abey laws \cr
#'	opdevel \tab opportunity to develop my abilities \cr
#'	oppsegov \tab how important:citizen engage in acts of civil disobed \cr
#'	oth16 \tab other protestant denominations \cr
#'	other \tab other protestant denominations \cr
#'	othersex \tab r had sex with some other last year \cr
#'	othjew \tab consider self to be jewish \cr
#'	othlang \tab can r speak language other than english \cr
#'	othlang1 \tab what other languages does r speak \cr
#'	othlang2 \tab what other languages does r speak \cr
#'	othreasn \tab how important to try to undrstnd reasonings of othr o \cr
#'	othshelp \tab people should help less fortunate others \cr
#'	oversamp \tab weights for black oversamples \cr
#'	overwork \tab r has too much work to do well \cr
#'	owngun \tab have gun in home \cr
#'	ownstock \tab r has stock in rs company \cr
#'	paidsex \tab r had sex for pay last year \cr
#'	painarms \tab r had pain in the arms in the past 12 months \cr
#'	paind10 \tab fathers industry code (2010) \cr
#'	paocc10 \tab fathers census occupation code (2010) \cr
#'	parborn \tab were rs parents born in this country \cr
#'	parcit \tab were your parents citizens of america? \cr
#'	parsol \tab rs living standard compared to parents \cr
#'	partfull \tab was r's work part-time or full-time? \cr
#'	partners \tab how many sex partners r had in last year \cr
#'	partnrs5 \tab how many sex partners r had in last 5 years \cr
#'	partteam \tab r work as part of a team \cr
#'	partyid \tab political party affiliation \cr
#'	patriot1 \tab patriotic feelings strengthen america's place in world \cr
#'	patriot2 \tab patriotic feelings lead to intolerance in america \cr
#'	patriot3 \tab patriotic feelings are needed for america to remain united \cr
#'	patriot4 \tab patriotic feelings lead to negative feelings towards immigrants \cr
#'	pawrkslf \tab father self-emp. or worked for somebody \cr
#'	paytaxes \tab how important never to try to evade taxes \cr
#'	peocntct \tab how many people in contact in a typical weekday \cr
#'	peoptrbl \tab assisting people in trouble is very important \cr
#'	phase \tab subsampling: two-phase design. \cr
#'	phone \tab does r have telephone \cr
#'	physhlth \tab days of poor physical health past 30 days \cr
#'	pikupsex \tab r had sex with casual date last year \cr
#'	pillok \tab birth control to teenagers 14-16 \cr
#'	pistol \tab pistol or revolver in home \cr
#'	polabuse \tab citizen said vulgar or obscene things \cr
#'	polactve \tab pol party encourge ppl to be active in politics in am \cr
#'	polattak \tab citizen attacking policeman with fists \cr
#'	poleff11 \tab don't have any say about what the government does \cr
#'	poleff18 \tab govt do not care much what ppl like r think \cr
#'	poleff19 \tab r have a good understanding of pol issues \cr
#'	poleff20 \tab most ppl are better informed about politics than r is \cr
#'	polescap \tab citizen attempting to escape custody \cr
#'	polfunds \tab donated money or raised funds for soc or pol activity \cr
#'	polgreed \tab most politicians are only for what get out of politics \cr
#'	polhitok \tab ever approve of police striking citizen \cr
#'	polint1 \tab how interested in politics \cr
#'	polinter \tab expressed political views on internet past year \cr
#'	polmurdr \tab citizen questioned as murder suspect \cr
#'	polnews \tab how often use media to get political news \cr
#'	polopts \tab how important:ppl given chance to participate in deci \cr
#'	polviews \tab think of self as liberal or conservative \cr
#'	popespks \tab pope is infallible on matters of faith or morals \cr
#'	popular \tab to be well liked or popular \cr
#'	pornlaw \tab feelings about pornography laws \cr
#'	posslq \tab does r have marital partner \cr
#'	posslqy \tab relationship status and cohabitation or not \cr
#'	postlife \tab belief in life after death \cr
#'	powrorgs \tab intl orgs take away much power from american govt \cr
#'	pray \tab how often does r pray \cr
#'	prayer \tab bible prayer in public schools \cr
#'	premarsx \tab sex before marriage \cr
#'	pres08 \tab vote obama or mccain \cr
#'	pres12 \tab vote obama or romney \cr
#'	preteen \tab household members 6 thru 12 yrs old \cr
#'	prodctiv \tab work conditions allow productivity \cr
#'	promtefr \tab promotions are handled fairly \cr
#'	promteok \tab rs chances for promotion good \cr
#'	proudart \tab how proud its achievements in the arts & lit. \cr
#'	prouddem \tab how proud the way democracy works \cr
#'	proudeco \tab how proud america's economic achievements \cr
#'	proudemp \tab r proud to work for employer \cr
#'	proudgrp \tab how proud its fair and equal treatment \cr
#'	proudhis \tab how proud its history \cr
#'	proudmil \tab how proud america's armed forces \cr
#'	proudpol \tab how proud its political influence in the world \cr
#'	proudsci \tab how proud its scientific and tech achievements \cr
#'	proudspt \tab how proud its achievements in sports \cr
#'	proudsss \tab how proud its social security system \cr
#'	racdif1 \tab differences due to discrimination \cr
#'	racdif2 \tab differences due to inborn disability \cr
#'	racdif3 \tab differences due to lack of education \cr
#'	racdif4 \tab differences due to lack of will \cr
#'	raclive \tab any opp. race in neighborhood \cr
#'	racmeet \tab allowed to hold pub meeting for racist \cr
#'	racopen \tab vote on open housing law \cr
#'	racwork \tab racial makeup of workplace \cr
#'	radioact \tab sci knowledge:all radioactivity is man-made \cr
#'	rank \tab rs self ranking of social position \cr
#'	ratetone \tab r's facial coloring by interviewer \cr
#'	realinc \tab family income in constant $ \cr
#'	realrinc \tab rs income in constant $ \cr
#'	reborn \tab has r ever had a 'born again' experience \cr
#'	refrndms \tab referendum are good way to decide important pol quest \cr
#'	reg16 \tab region of residence, age 16 \cr
#'	relactiv \tab how often does r take part in relig activities \cr
#'	relatsex \tab relation to last sex partner \cr
#'	relig \tab rs religious preference \cr
#'	relig16 \tab religion in which raised \cr
#'	reliten \tab strength of affiliation \cr
#'	relmeet \tab allowed to hold pub meeting for religious extremist \cr
#'	relpersn \tab r consider self a religious person \cr
#'	res16 \tab type of place lived in when 16 yrs old \cr
#'	respect \tab r treated with respect at work \cr
#'	respnum \tab number in family of r \cr
#'	retchnge \tab r returned money after getting too much change \cr
#'	revmeet \tab allowed to hold pub meeting for ppl who want overthro \cr
#'	rghtsmin \tab how important:govt protect right of minorities \cr
#'	richwork \tab if rich, continue or stop working \cr
#'	rifle \tab rifle in home \cr
#'	rincblls \tab income alone is enough \cr
#'	rincom06 \tab respondents income \cr
#'	rincome \tab respondents income \cr
#'	rowngun \tab does gun belong to r \cr
#'	safefrst \tab no shortcuts on worker safety \cr
#'	safehlth \tab safety and health condition good at work \cr
#'	safetywk \tab worker safety priority at work \cr
#'	satfin \tab satisfaction with financial situation \cr
#'	satjob \tab job or housework \cr
#'	satjob1 \tab job satisfaction in general \cr
#'	savesoul \tab tried to convince others to accept jesus \cr
#'	scibnfts \tab benefits of sci research outweight harmful results \cr
#'	scifrom \tab main source of information about science and technology \cr
#'	scinews1 \tab newspaper printed or online \cr
#'	scinews2 \tab magazine printed or online \cr
#'	scinews3 \tab where online get info \cr
#'	scistudy \tab r has clear understanding of scientific study \cr
#'	scitext \tab what it means to r to study scienfically \cr
#'	secondwk \tab r has job other than main \cr
#'	sector \tab type of college respondent attended \cr
#'	seeksci \tab probable source of information about scientific issues \cr
#'	selffrst \tab people need not overly worry about others \cr
#'	selfless \tab r feels like a selfless caring for others \cr
#'	servepeo \tab how committed govt admnstrators are to serve people \cr
#'	sexeduc \tab sex education in public schools \cr
#'	sexfreq \tab frequency of sex during last year \cr
#'	sexornt \tab sexual orientation \cr
#'	sexsex \tab sex of sex partners in last year \cr
#'	sexsex5 \tab sex of sex partners last five years \cr
#'	shortcom \tab world better if america acknowledged shortcomings \cr
#'	shotgun \tab shotgun in home \cr
#'	sibs \tab number of brothers and sisters \cr
#'	signdpet \tab signed a petition \cr
#'	size \tab size of place in 1000s \cr
#'	slpprblm \tab trouble sleeping last 12 months \cr
#'	socbar \tab spend evening at bar \cr
#'	socfrend \tab spend evening with friends \cr
#'	socommun \tab spend evening with neighbor \cr
#'	socrel \tab spend evening with relatives \cr
#'	solarrev \tab sci knowledge:how long the earth goes around the sun \cr
#'	solok \tab how important:citizens have adequate standard of livi \cr
#'	spanking \tab favor spanking to discipline child \cr
#'	spden \tab specific denomination, spouse \cr
#'	spdipged \tab spouse diploma, ged, or other \cr
#'	spevwork \tab spouse ever work as long as a year \cr
#'	spfund \tab how fundamentalist is spouse currently \cr
#'	sphrs1 \tab number of hrs spouse worked last week \cr
#'	sphrs2 \tab no. of hrs spouse usually works a week \cr
#'	spind10 \tab spouses industry code (naics 2007) \cr
#'	spkath \tab allow anti-religionist to speak \cr
#'	spkcom \tab allow communist to speak \cr
#'	spkhomo \tab allow homosexual to speak \cr
#'	spklang \tab how well does r speak other language \cr
#'	spkmil \tab allow militarist to speak \cr
#'	spkmslm \tab allow muslim clergymen preaching hatred of the us \cr
#'	spkrac \tab allow racist to speak \cr
#'	spocc10 \tab spouse census occupation code (2010) \cr
#'	spother \tab other protestant denominations \cr
#'	sprel \tab spouses religious preference \cr
#'	sprtprsn \tab r consider self a spiritual person \cr
#'	spsector \tab type of college spouse attended \cr
#'	spvtrfair \tab supervisor is fair \cr
#'	spwrkslf \tab spouse self-emp. or works for somebody \cr
#'	spwrksta \tab spouse labor force status \cr
#'	stockops \tab r hold any stock options of rs company \cr
#'	stockval \tab total dollar value of rs stock \cr
#'	stress \tab how often does r find work stressful \cr
#'	stress12 \tab stress management program last 12 months \cr
#'	strredpg \tab access to stress management \cr
#'	suicide1 \tab suicide if incurable disease \cr
#'	suicide2 \tab suicide if bankrupt \cr
#'	suicide3 \tab suicide if dishonored family \cr
#'	suicide4 \tab suicide if tired of living \cr
#'	supcares \tab supervisor concerned about welfare \cr
#'	suprvsjb \tab does r supervise others at work \cr
#'	suphelp \tab supervisor helpful to r in getting job done \cr
#'	talkedto \tab talked with someone depressed past 12 months \cr
#'	talkspvs \tab comfortable talking with supervisor about personal \cr
#'	tax \tab rs federal income tax \cr
#'	teamsafe \tab mgt and employees work together re safety \cr
#'	teens \tab household members 13 thru 17 yrs old \cr
#'	teensex \tab sex before marriage -- teens 14-16 \cr
#'	thnkself \tab to think for ones self \cr
#'	toofast \tab science makes our way of life change too fast \cr
#'	toofewwk \tab how often not enough staff \cr
#'	trdestck \tab company stock publicly traded \cr
#'	trdunion \tab workers need strong unions \cr
#'	trust \tab can people be trusted \cr
#'	trustman \tab r trust management at work \cr
#'	trynewjb \tab how likely r make effort for new job next year \cr
#'	tvhours \tab hours per day watching tv \cr
#'	unemp \tab ever unemployed in last ten yrs \cr
#'	union \tab does r or spouse belong to union \cr
#'	unrelat \tab number in household not related \cr
#'	uscitzn \tab is r us citizen \cr
#'	usedup \tab how often during past month r felt used up \cr
#'	usemedia \tab contacted in the media to express view \cr
#'	useskill \tab how much past skills can you make use in present \cr
#'	usetech \tab percentage of time use tech \cr
#'	usewww \tab r use www other than email \cr
#'	uswar \tab expect u.s. in war within 10 years \cr
#'	uswary \tab expect u.s. in world war in 10 years \cr
#'	valgiven \tab total donations past year r and immediate family \cr
#'	vetyears \tab years in armed forces \cr
#'	viruses \tab sci knowledge:antiviotics kill viruses as well as bacteria \cr
#'	visitors \tab number of visitors in household \cr
#'	voedcol \tab non-college postsecondary education (voednme1) \cr
#'	voednme1 \tab postsecondary institution attended for credit \cr
#'	voedncol \tab non-college postsecondary education (voednme2) \cr
#'	voednme2 \tab postsecondary institution attended for credit \cr
#'	volchrty \tab r done volunteer work for a charity \cr
#'	volmonth \tab volunteer in last month \cr
#'	vote08 \tab did r vote in 2008 election \cr
#'	vote12 \tab did r vote in 2012 election \cr
#'	voteelec \tab how important always to vote in elections \cr
#'	watchgov \tab how important to keep watch on action of govt \cr
#'	waypaid \tab how paid in main job \cr
#'	wealth \tab total wealth of respondent \cr
#'	webmob \tab r uses home internet through mobile device \cr
#'	weekswrk \tab weeks r. worked last year \cr
#'	weight \tab r weighs how much \cr
#'	whencol \tab when received college degree \cr
#'	whenhs \tab when received hs degree \cr
#'	whoelse1 \tab presence of others:children under six \cr
#'	whoelse2 \tab presence of others:older children \cr
#'	whoelse3 \tab presence of others:spouse partner \cr
#'	whoelse4 \tab presence of others:other relatives \cr
#'	whoelse5 \tab presence of others:other adults \cr
#'	whoelse6 \tab presence of others:no one \cr
#'	whywkhme \tab usual reason r work at home \cr
#'	widowed \tab ever been widowed \cr
#'	wkageism \tab r feels discriminated because of age \cr
#'	wkcontct \tab how often contacted about work when not working \cr
#'	wkdecide \tab how often r take part in decisions \cr
#'	wkfreedm \tab a lot of freedom to decide how to do job \cr
#'	wkharoth \tab r threatened on the job last 12 months \cr
#'	wkharsex \tab r sexually harassed on the job last 12 months \cr
#'	wkpraise \tab r is likely to be praised by supervisor \cr
#'	wkracism \tab r feels discriminated because of race \cr
#'	wksexism \tab r feels discriminated because of gender \cr
#'	wksmooth \tab workplace runs in smooth manner \cr
#'	wksub \tab does r or spouse have supervisor \cr
#'	wksubs \tab does supervisor have supervisor \cr
#'	wksup \tab does r or spouse supervise anyone \cr
#'	wksups \tab does subordinate supervise anyone \cr
#'	wkvsfam \tab how often job interferes fam life \cr
#'	wlthblks \tab rich - poor \cr
#'	wlthwhts \tab rich - poor \cr
#'	workblks \tab hard working - lazy \cr
#'	workdiff \tab r does numerous things on job \cr
#'	workfast \tab job requires r to work fast \cr
#'	workfor1 \tab r work for whom \cr
#'	workhard \tab to work hard \cr
#'	workwhts \tab hard working - lazy \cr
#'	wrkgovt \tab govt or private employee \cr
#'	wrkhome \tab how often r works at home \cr
#'	wrksched \tab usual work schedule \cr
#'	wrkslf \tab r self-emp or works for somebody \cr
#'	wrkstat \tab labor force status \cr
#'	wrktime \tab r has enough time to get the job done \cr
#'	wrktype \tab work arrangement at main job \cr
#'	wrkwayup \tab blacks overcome prejudice without favors \cr
#'	wrldgovt \tab international bodies should enforce environment \cr
#'	wwwhr \tab www hours per week \cr
#'	wwwmin \tab www minutes per week \cr
#'	xmarsex \tab sex with person other than spouse \cr
#'	xmovie \tab seen x-rated movie in last year \cr
#'	xnorcsiz \tab expanded n.o.r.c. size code \cr
#'	yearsjob \tab time at current job \cr
#'	yearval \tab total dollar value of payments in that year \cr
#' }
#' @source Data: \url{https://sda.berkeley.edu/sdaweb/analysis/?dataset=gss14}
#' @source Codebook: \url{http://burrelvannjr.com/docs/GSS_Codebook.pdf}

#'
"GSS2014"
