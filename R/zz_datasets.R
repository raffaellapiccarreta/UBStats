#' Data: MktDATA.Orig
#'
#' This dataset contains the variables from a survey on a set of customers
#' of a company operating in the retail food sector. The company sells
#' products from 3 major categories (referred to as A, B, C) The customers can
#' order and acquire products in the company physical stores, or through the
#' company's website (in this case, they can order on the website and pick up the
#' order in one store). Information is collected on customers' activity in the
#' last two years (observation period), as well as some information retrieved
#' through questionnaires or fidelity cards. During such period different
#' marketing strategies were adopted to improve customers' fidelization, and 5
#' marketing campaigns were launched; a last campaign was launched at the end of
#' the observation period.
#'
#' @usage data(MktDATA.Orig)
#'
#' @format A data frame with *n* = 2224 observations and the following 19
#' variables (levels of the variables listed in alphabetical order):
#' \describe{
#'   \item{CustId}{(num) customer's identification label}
#'   \item{Gender}{(chr) customer's gender (`F`, `M`)}
#'   \item{Age}{(num) customer's age (in years)}
#'   \item{Education}{(chr) customer's level of education(`College`, `Graduate`,
#'     `HighSchool`, `Post-Grad`)}
#'   \item{Marital_Status}{(chr) customer's marital status
#'     (`Divorced`, `Married`, `Single`, `Together`, `Widow`)}
#'   \item{Children}{(num) number of children in the household}
#'   \item{Kids}{(num) number of kids aged less than 12 in the
#'     household}
#'   \item{Income}{(chr) customer's income (measured in classes)}
#'   \item{Baseline}{(num) index (from 0 to 1) assigned by the marketing dept
#'     indicating how promising the customer was judged at the beginning of
#'     the observation period}
#'   \item{LikeMost}{(chr) Most frequently bought category in the last two
#'     years (`P.A`, `P.B`, `P.C`)}
#'   \item{TotVal}{(num) amount spent in the last 2 years}
#'   \item{NPickUp_Purch}{(num) number of purchases made through company's
#'     website and picked up in physical store  }
#'   \item{NWeb_Purch}{(num) number of purchases made through company's website
#'     and delivered at home}
#'   \item{NStore_Purch}{(num) number of purchases made in a physical store}
#'   \item{NDeals}{(num) number of products purchases with discount}
#'   \item{CustClass}{(chr) customer's classification (assigned by the marketing
#'     dept) based on past profitability (`Bronze`, `Gold`, `Platinum`,
#'     `Silver`)}
#'   \item{PastCampaigns}{(num) number of offers accepted by the customer in the
#'     last 2 years' marketing campaigns}
#'   \item{LastCampaign}{(num) binary variable (0/1) indicating whether (1) or
#'     not (0) the customer accepted the offer in the campaign launched at
#'     the end of the observation period}
#'   \item{WouldSuggest}{(chr) variable signalling whether (`Yes`) or not
#'     (`No`) the customer declared they would suggest the company's products
#'     to friends and family}
#' }
#' 
#' @source The data set has been adapted from \href{https://www.kaggle.com/code/dmitryuarov/customers-clustering-eda}{https://www.kaggle.com/code/dmitryuarov/customers-clustering-eda}.
"MktDATA.Orig"


#' Data MktDATA: 
#'
#' This dataset is a modification of the original \code{\link{MktDATA.Orig}}
#' dataset and it is provided for user convenience.
#'
#' @usage data(MktDATA)
#'
#' @format{A data frame with *n* = 2224 observations and 26 variables.}
"MktDATA"


#' Data Banner: Banner Click-Through Rate Experiment
#'
#' The data frame contains data collected by a company interested in comparing
#' a \emph{Redesigned} banner (featuring a brighter colour scheme and modified
#' layout) with the \emph{Original} version. A random sample of users from a
#' social network is shown one of two banners, and the effectiveness of the
#' two banners is assessed via the \strong{click-through rate (CTR)} -- the
#' proportion of users who click on a banner after viewing it.
#'
#' @format A data frame with *n* = 5976 observations and 3 variables:
#' \describe{
#'   \item{User}{(int) user id}
#'   \item{Banner}{(chr) banner shown to the user (Original, Redesigned)}
#'   \item{Click}{(logical) logical vector indicating whether the user
#'     clicked on the banner (TRUE) or not}
#' }
#'
#' @references
#' Piccarreta, R., Tonini, D., & Trentini, F. (2026). \emph{From Data to
#' Decisions: An Applied Introduction to Statistics}. BUP. ISBN
#' 9788823824096.
#'
#' @docType data
#' @keywords datasets
#' @usage data(Banner)
"Banner"


#' Data BasketValue: Supermarket Spending and Basket Variety
#'
#' The data frame contains data on a supermarket customers living relatively
#' close to the store. Based on the details provided when the loyalty card
#' was registered and on recorded expenses, for each customer information is
#' available on a composite index, \emph{SpendMix}, which captures how
#' supermarket clients differ not only in how much they spend but also in the
#' range of products they purchase. We are interested to identify factors
#' associated with the level of spending and basket variety. Some
#' characteristics (e.g., income and age) are obvious candidates: richer
#' individuals usually spend more, and spending habits often change over the
#' course of a person's life. Yet behavioural differences can also play a
#' role. For instance, shoppers who are particularly responsive to discounts
#' may be more inclined to experiment with new items or brands.
#'
#' @format A data frame with *n* = 128 observations and 5 variables:
#' \describe{
#'   \item{Id}{(int) customer identification number}
#'   \item{Age}{(num) age of the customers in years}
#'   \item{Wealth}{(num) composite indicator of household economic
#'     well-being obtained by estimating household wealth from the average
#'     market price and rental value of dwellings located in the same street
#'     as each customer's declared address, and adjusting for the number of
#'     cohabitants}
#'   \item{Promo}{(factor) binary variable indicating whether promotional
#'     products are frequently purchased (No, Yes)}
#'   \item{SpendMix}{(num) composite index which reflects both spending
#'     level and basket variety}
#' }
#'
#' @references
#' Piccarreta, R., Tonini, D., & Trentini, F. (2026). \emph{From Data to
#' Decisions: An Applied Introduction to Statistics}. BUP. ISBN
#' 9788823824096.
#'
#' @docType data
#' @keywords datasets
#' @usage data(BasketValue)
"BasketValue"


#' Data CallCentre_KPI: Call Centre Key Performance Indicators
#'
#' This data frame contains data on the calls to the call centre of a
#' telecommunication company. It contains information about the calls (day
#' and hour of the call), about the calling customer's characteristics
#' (private or corporate, or unknown), and about some key performance
#' indicators (KPI). In particular, it contains information on the duration
#' of the phases before the customer eventually reaches an operator. First,
#' an automated system directs incoming calls to the appropriate department
#' based on the reason for the call; then the customer waits in a queue until
#' an operator becomes available. It also contains a variable indicating
#' whether the caller hung up during a specific phase. For those customers
#' whose call was finally handled by an operator, we also know whether their
#' issue was solved and their level of satisfaction, expressed at the end of
#' the call.
#'
#' @format A data frame with *n* = 5007 observations and 11 variables:
#' \describe{
#'   \item{ID_Call}{(int) call identification}
#'   \item{Day}{(factor) weekday of the call (Mon, Tue, Wed,..., Sat)}
#'   \item{Hour}{(num) hour of the call}
#'   \item{Cust.Type}{(chr) customer's type (Unknown, Private, Corporate)}
#'   \item{Time.Auto}{(num) time (in seconds) spent interacting with the
#'     automatic responder}
#'   \item{Time.Queue}{(num) time (in seconds) spent waiting in the queue
#'     for an operator}
#'   \item{Time.Talk}{(num) duration (in seconds) of the (eventual)
#'     conversation with an operator}
#'   \item{Reason}{(chr) reason for contact (Admin -- administrative
#'     issues; Landline -- issues with fixed line; Mobile; and
#'     Activ/Transf -- calls related to the activation or the transfer of
#'     a line)}
#'   \item{Outcome}{(chr) outcome of the call (Left.Aut_Resp -- the client
#'     hung up before entering the queue; Left.Queue -- the client hung up
#'     while waiting for an operator; Operator -- the call was handled by
#'     an operator)}
#'   \item{Solved}{(chr) variable indicating whether the client's issue was
#'     solved (No, Yes) -- available only for clients who interacted with
#'     an operator}
#'   \item{Satisf}{(factor) level of satisfaction expressed by customers
#'     who interacted with an operator (VLow, Low, Med, High, VHigh)}
#' }
#'
#' @references
#' Piccarreta, R., Tonini, D., & Trentini, F. (2026). \emph{From Data to
#' Decisions: An Applied Introduction to Statistics}. BUP. ISBN
#' 9788823824096.
#'
#' @docType data
#' @keywords datasets
#' @usage data(CallCentre_KPI)
"CallCentre_KPI"


#' Data ESG_Returns: Simulated ESG Performance and Financial Returns
#'
#' This dataset was generated with the assistance of AI. Its structure and
#' parameter values were designed to reproduce stylised empirical patterns
#' discussed in academic studies on the relationship between environmental,
#' social, and governance (ESG) performance and financial returns during
#' crisis and normal periods. However, this dataset is entirely synthetic and
#' created for \strong{teaching purposes only}; it does not represent real
#' firms, markets, or observed behaviours and should not be interpreted as
#' empirical evidence.
#'
#' The data frame contains (simulated) weekly observations for 300 firms over
#' 60 weeks (starting from 7 June 2020) across three industry sectors. For
#' each observation (firm-week), information is available on several groups
#' of variables. Its structural variables identify the firm, the observation
#' week, the firm's size, and its industry sector. The main variables of
#' interest are the firm's weekly risk-adjusted return, its monthly ESG
#' score, and the Crisis indicator for the market-stress period. The dataset
#' also includes market-related characteristics, such as Beta and Momentum,
#' together with a risk measure represented by firm-specific Volatility.
#'
#' @format A data frame with *n* = 18000 observations and 10 variables:
#' \describe{
#'   \item{Firm}{(num) Firm identification number}
#'   \item{Week}{(date) weekly time period to which each firm-level
#'     observation refers, covering 60 consecutive weeks beginning on 7
#'     June 2020}
#'   \item{Industry}{(chr) firm's sector classification (Consumer, Energy,
#'     or Technology)}
#'   \item{Return}{(num) realised stock Return (in percentage points),
#'     with positive values indicating firm-specific gains and negative
#'     values indicating losses. These returns are risk-adjusted: the
#'     common market component has been removed so that they reflect the
#'     firm's idiosyncratic performance rather than general market
#'     movements}
#'   \item{ESG}{(num) score, measured at the beginning of each month,
#'     which ranges between 0 and 100 and captures the firm's monthly
#'     sustainability performance}
#'   \item{Crisis}{(int) dummy variable identifying a market stress period
#'     within the sample, approximately corresponding to October-December
#'     2020. It is intended to mimic a phase of renewed financial
#'     uncertainty linked to the COVID-19 pandemic, when equity markets
#'     came under renewed pressure because of the second wave of
#'     infections and the reintroduction of restrictions, before the
#'     first positive vaccine announcements improved sentiment}
#'   \item{Beta}{(num) the firm's sensitivity to market movements
#'     estimated from a 12-month pre-sample window}
#'   \item{Size}{(num) a proxy for firm scale}
#'   \item{Volatility}{(num) the firm-specific volatility of returns,
#'     measured as the standard deviation of daily returns in the month
#'     immediately preceding the first week of the sample}
#'   \item{Momentum}{(num) the cumulative return over the previous 6 to 12
#'     months (excluding the most recent month)}
#' }
#'
#' @references
#' Piccarreta, R., Tonini, D., & Trentini, F. (2026). \emph{From Data to
#' Decisions: An Applied Introduction to Statistics}. BUP. ISBN
#' 9788823824096.
#'
#' @docType data
#' @keywords datasets
#' @usage data(ESG_Returns)
"ESG_Returns"


#' Data Grocery_NE: Grocery Chain Customer Survey (North-East Italy)
#'
#' This dataset contains the variables from a survey on a sample of
#' customers from a grocery chain operating in Italy. Specifically, data
#' refer to the sub-sample of customers who regularly shop in stores located
#' in north-eastern Italian region. Information is available on the activity
#' observed in the last year (number of visits and transactions, amount
#' spent), on customers' satisfaction with the retailer, and on the
#' perceived weakness of the most frequently visited store.
#'
#' @format A data frame with *n* = 3114 observations and 28 variables:
#' \describe{
#'   \item{Id}{(int) customer identification}
#'   \item{Sex}{(chr) customer's sex at birth, (F or M)}
#'   \item{Age}{(num) customer's age (in years)}
#'   \item{Tenure}{(factor) Customer tenure in years (coded in classes:
#'     [0,1), [1,3), [3,6), [6,10), [10,15), [15,25), [25,35))}
#'   \item{Status}{(chr) customers' status (Active, Silent)}
#'   \item{FavShop}{(chr) store customers visit the most (NE.01, NE.02,
#'     ..., NE.07)}
#'   \item{FavShop_Region}{(chr) macro region where the favourite shop is
#'     located (here, only North-East)}
#'   \item{TotShops}{(num) number of stores visited by the customer}
#'   \item{WeekDay}{(chr) preferred shopping day (1:Mon, 2:Tue, ..., 7:Sun)}
#'   \item{TimeSlot}{(chr) preferred shopping time slot (08-12, 12-14,
#'     14-17, 17-23)}
#'   \item{Satisf}{(chr) Customer's overall declared satisfaction with the
#'     retailer (VLow, QLow, Low, Med, QHigh, High, VHigh)}
#'   \item{Complaint}{(factor) major weakness of the typically visited
#'     store (Quality&Variety, Prices, Resupply, Staff, Crowded, Checkout)}
#'   \item{NMonths}{(num) number of months in which the customers visited
#'     a store at least once}
#'   \item{MonthExp}{(chr) amount spent per month, in classes ([0,50),
#'     [50,100), [100,150), [150,200), [200,300), [300,400), [400,600),
#'     [600,800))}
#'   \item{Transact_M}{(num) transactions per month}
#'   \item{TBP}{(num) time between purchases}
#'   \item{TotExp}{(num) amount spent in the last 12 months}
#'   \item{TotVisits}{(num) total number of visits in the last year}
#'   \item{Receipt}{(num) average receipt (transaction value)}
#'   \item{Visits_Regular}{(num) score (ranging from 0 to 100) indicating
#'     the regularity of customer's shopping trips}
#'   \item{Spending_Regular}{(num) score (ranging from 0 to 100)
#'     indicating the regularity of customer's spending}
#'   \item{Discount}{(num) average discount on purchased products}
#'   \item{CrossSelling}{(num) index measuring how diverse a customer's
#'     purchases are across different product categories}
#'   \item{RecencyScore}{(num) index reflecting how recently a customer
#'     last interacted with a company (higher scores reflecting more
#'     recent interactions)}
#'   \item{MonetaryScore}{(num) index reflecting the overall value of the
#'     customer to the business}
#'   \item{FrequencyScore}{(num) index reflecting the regularity or
#'     repetition of customer transactions, with higher scores indicating
#'     more frequent purchases}
#' }
#'
#' @references
#' Piccarreta, R., Tonini, D., & Trentini, F. (2026). \emph{From Data to
#' Decisions: An Applied Introduction to Statistics}. BUP. ISBN
#' 9788823824096.
#'
#' @docType data
#' @keywords datasets
#' @usage data(Grocery_NE)
"Grocery_NE"


#' Data JobEngage: Employee Job Engagement Survey
#'
#' The data frame contains the results of a survey conducted to analyse job
#' engagement (the emotional commitment of employees to their organisation)
#' within a large company, focusing on factors previously identified through
#' qualitative methods (including focus groups and exploratory interviews).
#' \emph{Constructs} related to the possible determinants of engagement were
#' measured using Likert scale items, where respondents were asked to
#' indicate their level of agreement with a series of statements pertaining
#' to each factor. For example, perceived compensation fairness was measured
#' through items such as 'My pay reflects the responsibilities of my role'
#' and 'I am fairly compensated relative to others in similar jobs'. The
#' responses to different items referring to the same construct were then
#' averaged to form a composite score.
#'
#' @format A data frame with *n* = 256 observations and 11 variables:
#' \describe{
#'   \item{Id}{(int) respondent id}
#'   \item{Engagement}{(num) score reflecting the respondent's level of
#'     engagement}
#'   \item{Leadership}{(num) respondent evaluation of managers, as well as
#'     the quality of their relationship with them}
#'   \item{Team}{(num) respondent evaluation of co-workers, respectively,
#'     as well as the quality of relationship with them}
#'   \item{Environment}{(num) respondent evaluation of the quality of the
#'     work environment}
#'   \item{Growth}{(num) indicator of professional development and
#'     advancement; it captures the degree to which employees see clear
#'     paths to promotion within the company and feel they are provided
#'     with opportunities to learn and develop skills}
#'   \item{Compensation}{(num) respondent perceived fairness of
#'     compensation}
#'   \item{WorkPlace}{(num) respondent evaluation of the physical work
#'     environment, including aspects such as lighting, noise level, the
#'     amount of space they have access to, and ergonomics}
#'   \item{RoleConflict}{(num) extent to which employees feel that the
#'     expectations connected with their role are inconsistent or
#'     incompatible with each other (for example, when they receive
#'     conflicting instructions from different supervisors or face
#'     demands that are difficult to reconcile in day-to-day tasks)}
#'   \item{Flex}{(num) degree to which employees experience freedom,
#'     comfort, and control in how and where they work (it reflects
#'     work-life balance, autonomy in tasks and time management)}
#'   \item{RemoteWork}{(factor) remote-work arrangements (No: the
#'     employee works entirely on-site; Structured: remote work on
#'     predefined days or under fixed rules; and Flexible: remote work
#'     without constraints)}
#' }
#'
#' @references
#' Piccarreta, R., Tonini, D., & Trentini, F. (2026). \emph{From Data to
#' Decisions: An Applied Introduction to Statistics}. BUP. ISBN
#' 9788823824096.
#'
#' @docType data
#' @keywords datasets
#' @usage data(JobEngage)
"JobEngage"


#' Data Marketing_Mix: Marketing Investments and Digital Performance
#'
#' This data frame contains data on marketing campaigns conducted across
#' municipalities in Italy. It provides information on marketing investments
#' in each area, including spending on online \strong{paid} \emph{search}
#' (paying to have a website or ad appear at the top of a search engine
#' results page), online \emph{display} (paying for visual ads on websites,
#' apps, or social media platforms) and \emph{retargeting} (targeting users
#' who previously interacted with the brand online -- e.g. visiting a
#' website, using an app, or engaging with content -- but did not complete a
#' desired action, like making a purchase).
#'
#' These investments affect key digital performance indicators (KPIs), such
#' as customer \emph{interactions} and \emph{paid} or \emph{organic} website
#' sessions (visits resulting from sponsored or non-paid links), which
#' represent intermediate outcomes that may influence \emph{sales} and new
#' \emph{customer acquisition} -- the campaign's key final targets. The
#' data frame also includes information about two types of \emph{local
#' promotions} possibly running in each municipality. All variables in the
#' data frame -- investments, KPIs and outcomes -- are adjusted to account
#' for structural differences between municipalities.
#'
#' @format A data frame with *n* = 247 observations and 13 variables:
#' \describe{
#'   \item{Municipality}{(chr) municipality id}
#'   \item{MacroArea}{(factor) macro geographical region (A, B, C, D, E)}
#'   \item{LocalPromo1}{(num) binary variable indicating whether Campaign
#'     1 is active (1) or not (0)}
#'   \item{LocalPromo2}{(num) binary variable indicating whether Campaign
#'     1 is active (1) or not (0)}
#'   \item{Search_Spend}{(num) marketing expenses on online search
#'     advertisements (e.g., Google Ads)}
#'   \item{Display_Spend}{(num) marketing expenses on online display
#'     advertisements (e.g., banner ads)}
#'   \item{Retargeting_Spend}{(num) marketing expenses on retargeting
#'     (e.g., ads targeting previous visitors)}
#'   \item{KPI_Interact}{(num) number of interactions (e.g., clicks,
#'     engagements) across all channels}
#'   \item{KPI_Paid}{(num) number of sessions or visits generated through
#'     paid traffic campaigns}
#'   \item{KPI_Organic}{(num) number of sessions or visits generated
#'     through organic (non-paid) traffic}
#'   \item{Sales}{(num) Total sales units generated during the period}
#'   \item{NewUsers}{(num) number of new customers acquired during the
#'     period}
#'   \item{Arpu}{(num) Average Revenue Per User, calculated as total
#'     revenue divided by active users (then rounded)}
#' }
#'
#' @references
#' Piccarreta, R., Tonini, D., & Trentini, F. (2026). \emph{From Data to
#' Decisions: An Applied Introduction to Statistics}. BUP. ISBN
#' 9788823824096.
#'
#' @docType data
#' @keywords datasets
#' @usage data(Marketing_Mix)
"Marketing_Mix"


#' Data Spending: Household Income and Spending Patterns
#'
#' The data frame contains information on a sample of households within a
#' specific demographic segment; it is used to explore the relationship
#' between monthly spending on \emph{Essentials} and \emph{Discretionary}
#' goods and monthly disposable \emph{Income}. All variables in the
#' data frame are normalised to ensure comparability across households of
#' different sizes. Income is expressed in thousands of pounds per month per
#' equivalised household to avoid scale artefacts.
#'
#' @format A data frame with *n* = 751 observations and 4 variables:
#' \describe{
#'   \item{Id}{(int) household identification number}
#'   \item{Essentials}{(num) household monthly expenditures in essential
#'     goods (e.g., groceries, utilities, basic transport); score
#'     normalised to ensure comparability across household of different
#'     sizes}
#'   \item{Discretionary}{(num) household monthly expenditures in
#'     discretionary goods (e.g., dining out, travel, electronics); score
#'     normalised to ensure comparability across household of different
#'     sizes}
#'   \item{Income}{(num) monthly disposable income (in thousands of
#'     pounds) per equivalised household (meaning adjusted to allow
#'     meaningful comparisons across households with different size and
#'     composition)}
#' }
#'
#' @references
#' Piccarreta, R., Tonini, D., & Trentini, F. (2026). \emph{From Data to
#' Decisions: An Applied Introduction to Statistics}. BUP. ISBN
#' 9788823824096.
#'
#' @docType data
#' @keywords datasets
#' @usage data(Spending)
"Spending"


#' Data Time_Social: Time Spent on a Social Media Platform
#'
#' The data frame contains information on the time spent on a social media
#' platform, after the algorithm that selects displayed content to users was
#' modified. Specifically, attention is limited to a sample of active users
#' within a specific socio-demographic segment, who live in two distinct
#' areas.
#'
#' @format A data frame with *n* = 5976 observations and 4 variables:
#' \describe{
#'   \item{User}{(int) user id}
#'   \item{Time}{(num) Time (in minutes) spent on the platform during a
#'     specific day}
#'   \item{Area}{(chr) geographic area where the user lives (A, B)}
#'   \item{Push}{(chr) variable indicating whether the user activated push
#'     notifications (No, Yes)}
#' }
#'
#' @references
#' Piccarreta, R., Tonini, D., & Trentini, F. (2026). \emph{From Data to
#' Decisions: An Applied Introduction to Statistics}. BUP. ISBN
#' 9788823824096.
#'
#' @docType data
#' @keywords datasets
#' @usage data(Time_Social)
"Time_Social"


#' Data Transition: Employee Productivity After Software Transition
#'
#' The data frame refers to data on the employees of a company that adopted a
#' new management software system to replace its legacy tools. To facilitate
#' a smooth transition, all administrative staff will participate in a
#' structured training program designed to build proficiency with the new
#' system. As part of the rollout, employees from two departments have been
#' selected to complete the training first. The dataset contains information
#' on each (trained) employee's productivity using the old and the new
#' software, recorded after a defined period of use.
#'
#' @format A data frame with *n* = 130 observations and 4 variables:
#' \describe{
#'   \item{Employee}{(int) employee id}
#'   \item{Pre}{(num) employee's productivity with the old software}
#'   \item{Post}{(num) employee's productivity with the new software
#'     (after the training program)}
#'   \item{Department}{(chr) employee's department (Dept1, Dept2)}
#' }
#'
#' @references
#' Piccarreta, R., Tonini, D., & Trentini, F. (2026). \emph{From Data to
#' Decisions: An Applied Introduction to Statistics}. BUP. ISBN
#' 9788823824096.
#'
#' @docType data
#' @keywords datasets
#' @usage data(Transition)
"Transition"
