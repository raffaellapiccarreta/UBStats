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
#' @format{
#'   A data frame with 2224 observations and the following 19
#'   variables (levels of the variables listed in alphabetical order):
#'   * **`CustId`** (`num`): customer's identification label
#'   * **`Gender`** (`chr`): customer's gender (`F`, `M`)
#'   * **`Age`** (`num`): customer's age (in years)
#'   * **`Education`** (`chr`): customer's level of education(`College`, `Graduate`,
#'     `HighSchool`, `Post-Grad`)
#'   * **`Marital_Status`** (`chr`): customer's marital status
#'     (`Divorced`, `Married`, `Single`, `Together`, `Widow`)
#'   * **`Children`** (`num`): number of children in the household
#'   * **`Kids`** (`num`): number of kids aged less than 12 in the
#'     household
#'   * **`Income`** (`chr`): customer's income (measured in classes)
#'   * **`Baseline`** (`num`): index (from 0 to 1) assigned by the marketing dept
#'     indicating how promising the customer was judged at the beginning of
#'     the observation period
#'   * **`LikeMost`** (`chr`): Most frequently bought category in the last two
#'     years (`P.A`, `P.B`, `P.C`)
#'   * **`TotVal`** (`num`): amount spent in the last 2 years
#'   * **`NPickUp_Purch`** (`num`): number of purchases made through company's
#'     website and picked up in physical store  
#'   * **`NWeb_Purch`** (`num`): number of purchases made through company's website
#'     and delivered at home
#'   * **`NStore_Purch`** (`num`): number of purchases made in a physical store
#'   * **`NDeals`** (`num`): number of products purchases with discount
#'   * **`CustClass`** (`chr`): customer's classification (assigned by the marketing
#'     dept) based on past profitability (`Bronze`, `Gold`, `Platinum`,
#'     `Silver`)
#'   * **`PastCampaigns`** (`num`): number of offers accepted by the customer in the
#'     last 2 years' marketing campaigns
#'   * **`LastCampaign`** (`num`): binary variable (0/1) indicating whether (1) or
#'     not (0) the customer accepted the offer in the campaign launched at
#'     the end of the observation period
#'   * **`WouldSuggest`** (`chr`): variable signalling whether (`Yes`) or not
#'     (`No`) the customer declared they would suggest the company's products
#'     to friends and family
#' }
#' 
#' @source The data set has been adapted from \href{https://www.kaggle.com/code/dmitryuarov/customers-clustering-eda}{https://www.kaggle.com/code/dmitryuarov/customers-clustering-eda}.
"MktDATA.Orig"

#' Data: MktDATA
#'
#' This dataset is a modification of the original \code{\link{MktDATA.Orig}}
#' dataset and it is provided for user convenience.
#'
#' @usage data(MktDATA)
#'
#' @format{A data frame with 2224 observations and 26 variables.}
"MktDATA"

#' Data: Grocery_NE
#'
#' This dataset contains the variables from a survey on a sample of customers from
#' a grocery chain operating in Italy. Specifically, data refer to the sub-sample
#' of customers who regularly shop in stores located in north-eastern Italian region.
#' Information is available on the activity observed in the last year (number of
#' visits and transactions, amount spent), on customers’ satisfaction with the
#' retailer, and on the perceived weakness of the most frequently visited store.
#'
#' @usage data(Grocery_NE)
#'
#' @format{
#'   A dataframe with *n* = 3114 observations and the following 28 variables
#'   (levels of the variables listed in alphabetical order):
#'   * **Id** (`int`): customer identification
#'   * **Sex** (`chr`): customer’s sex at birth, (F or M)
#'   * **Age** (`num`): customer's age (in years)
#'   * **Tenure** (`factor`): Customer tenure in years (coded in classes: [0,1),
#'     [1,3), [3,6), [6,10), [10,15), [15,25), [25,35))
#'   * **Status** (`chr`): customers’ status (Active, Silent)
#'   * **FavShop** (`chr`): store customers visit the most (NE.01, NE.02,...,
#'     NE.07)
#'   * **FavShop_Region** (`chr`) region where the favourite shop is located
#'     (here, only North-East)
#'   * **TotShops** (`num`): number of stores visited by the customer
#'   * **WeekDay** (`chr`): preferred shopping day (1:Mon, 2:Tue,..., 7:Sun)
#'   * **TimeSlot** (`chr`): preferred shopping time slot (08-12, 12-14, 14-17,
#'     17-23)
#'   * **Satisf**: (`chr`): Customer’s overall declared satisfaction with the
#'     retailer (VLow, QLow, Low, Med, QHigh, High, VHigh)
#'   * **Complaint** (`factor`): major weakness of the typically visited store
#'     (Quality&Variety, Prices, Resupply, Staff, Crowded, Checkout)
#'   * **NMonths** (`num`): number of months in which the customers visited a
#'     store at least once
#'   * **MonthExp** (`chr`) amount spent per month, in classes ([0,50),
#'     [50,100), [100,150), [300,400), [200,300), [300,400), [400,600),
#'     \[600,800\])
#'   * **Transact_M** (`num`): transactions per month
#'   * **TBP** (`num`): time between purchases
#'   * **TotExp** (`num`): amount spent in the last 12 year
#'   * **TotVisits** (`num`): total number of visits in the last year
#'   * **Receipt** (`num`): average receipt (transaction value)
#'   * **Visits_Regular** (`num`): score (ranging from 0 to 100) indicating the
#'     regularity of customer’s shopping trips 
#'   * **Spending_Regular** (`num`) score (ranging from 0 to 100) indicating
#'     the regularity of customer’s spending
#'   * **Discount** (`num`): average discount on purchased products
#'   * **CrossSelling** (`num`): index measuring how diverse a customer's
#'     purchases are across different product categories
#'   * **RecencyScore** (`num`): index reflecting how recently a customer
#'     last interacted with a company (higher scores reflecting more recent
#'     interactions)
#'   * **MonetaryScore** (`num`): index reflecting the overall value of the
#'     customer to the business
#'   * **FrequencyScore** (`num`): index reflecting the regularity or
#'     repetition of customer transactions, with higher scores indicating more
#'     frequent purchases
#' }
"Grocery_NE"

#' Data: CallCentre_KPI
#'
#' This dataframe contains data on the calls to the call centre of a
#' telecommunication company. In particular, it contains information about the
#' calls (day and hour of the call), about the calling customer’s
#' characteristics (private or corporate, or unknown), and about some key
#' performance indicators (KPI). In particular, it contains information on the
#' duration of the phases before the customer eventually reaches an operator.
#' First, an automated system directs incoming calls to the appropriate
#' department based on the reason for the call; then the customer waits in a
#' queue until an operator becomes available. It also contains a variable
#' indicating whether the caller hung up during a specific phase. For those
#' customers whose call wad finally handled by an operator, we also know whether
#' their issue was solved and their level of satisfaction, expressed at the end
#' of the call.
#'
#' @usage data(CallCentre_KPI)
#'
#' @format{
#'   A dataframe with *n* = 5007 observations and the following 11 variables
#'   (levels of the variables listed in alphabetical order):
#'   * **ID_Call** (`int`): call identification
#'   * **Day** (`factor`): weekday of the call (Mon, Tue, Wed,…, Sat)
#'   * **Hour** (`num`): hour of the call
#'   * **Cust.Type** (`chr`): customer’s type (Unknown, Private, Corporate)
#'   * **Time.Auto** (`num`): time (in seconds) spent interacting with the
#'     automatic responder
#'   * **Time.Queue** (`num`): time (in seconds) spent waiting in the queue
#'     for an operator
#'   * **Time.Talk** (`num`): duration (in seconds) of the
#'     (eventual) conversation with an operator
#'   * **Reason** (`chr`): reason for contact (Admin -- administrative issues,
#'     Landline -- fixed line, Mobile, and Activ/Transf -- calls related to
#'     the activation or the transfer of a line)
#'   * **Outcome** (`chr`): outcome of the call (Left.Aut_Resp -- the client
#'     hung up before entering the queue, Left.Queue -- the client hung up
#'     while waiting for an operator, Operator -- the  call was handled by
#'     an operator)
#'   * **Solved** (`chr`): variable indicating whether the client’s issue was
#'     solved (No, Yes)
#'   * **Satisf**  (`factor`): level of satisfaction expressed by customers
#'     who interacted with an operator (VLow, Low, Med, High, VHigh).
#' }
"CallCentre_KPI"

#' Data: Marketing_Mix
#'
#' This dataframe contains data on marketing campaigns conducted across
#' municipalities in Italy. It provides information on marketing investments in
#' each area, including spending on online paid search (paying to have a website
#' or ad appear at the top of a search engine results page), online display
#' (paying for visual ads on websites, apps, or social media platforms) and
#' retargeting (targeting users who previously interacted with the brand
#' online -- e.g. visiting a website, using an app, or engaging with content --
#' but did not complete a desired action, like making a purchase).
#' These investments affect key digital performance indicators (KPIs), such as
#' customer interactions and paid or organic website sessions (visits resulting
#' from sponsored or non-paid links), which represent intermediate outcomes that
#' may influence sales and new customer acquisition -- the campaign’s key final
#' targets. The dataframe also includes information about two types of local
#' promotions possibly running in each municipality. All variables in the
#' dataframe --investments, KPIs and outcomes -- are adjusted to account for
#' structural differences between municipalities.
#'
#' @usage data(Marketing_Mix)
#'
#' @format{
#'   A dataframe with *n* = 247 observations and the following 13 variables
#'   (levels of the variables listed in alphabetical order):
#'   * **Municipality** (`chr`): municipality id
#'   * **MacroArea** (`factor`): macro geographical region (A, B, C, D, E)
#'   * **LocalPromo1** (`num`): binary variable indicating whether Campaign 1
#'     is active (1) or not (0)
#'   * **LocalPromo2** (`num`): binary variable indicating whether Campaign 1
#'     is active (1) or not (0)
#'   * **Search_Spend** (`num`): marketing expenses on online search
#'     advertisements (e.g., Google Ads)
#'   * **Display_Spend** (`num`): marketing expenses on online display
#'     advertisements (e.g., banner ads)
#'   * **Retargeting_Spend** (`num`): marketing expenses on retargeting
#'     (e.g., ads targeting previous visitors)
#'   * **KPI_Interact** (`num`): number of interactions (e.g., clicks,
#'     engagements) across all channels.
#'   * **KPI_Paid** (`num`): number of sessions or visits generated through
#'     paid traffic campaigns.
#'   * **KPI_Organic** (`num`): number of sessions or visits generated through
#'     organic (non-paid) traffic
#'   * **Sales** (`num`): Total sales units generated during the period
#'   * **NewUsers** (`num`): number of new  customers acquired during the
#'     period
#'   * **Arpu** (`num`): Average Revenue Per User, calculated as total revenue
#'     divided by active users (then rounded)
#' }
"Marketing_Mix"

#' Data: Time_Social
#'
#' The dataframe contains information on the time spent on a social media
#' platform, after the algorithm that selects displayed content to users was
#' modified. Specifically, attention is limited to a sample of active users
#' within a specific socio-demographic segment, who live in two distinct areas.
#'
#' @usage data(Time_Social)
#'
#' @format{
#'   A dataframe with *n* = 5976 observations and the following 4 variables
#'   (levels of the variables listed in alphabetical order):
#'   * **User** (`int`): user id
#'   * **Time** (`num`): Time (in minutes) spent on the platform during a
#'     specific day
#'   * **Area** (`chr`): geographic area where the user lives (A, B)
#'   * **Push** (`chr`): variable indicating whether the user activated push
#'     notifications (No, Yes)
#' }
"Time_Social"

#' Data: Banner
#'
#' The dataframe contains data collected by a company interested in comparing a
#' Redesigned banner (featuring a brighter colour scheme and modified layout)
#' with the Original version. A random sample of users from a social network is
#' shown one of two banners, and the effectiveness of the two banners is
#' assessed via the click-through rate (CTR) -- the proportion of users who click
#' on a banner after viewing it.
#'
#' @usage data(Banner)
#'
#' @format{
#'   A dataframe with *n* = 2350 observations and the following 3 variables
#'   (levels of the variables listed in alphabetical order):
#'   * **User** (`int`): user id
#'   * **Banner** (`chr`): banner shown to the user (Original, Redesigned)
#'   * **Click** (`logical`): logical vector indicating whether the user
#'     clicked on the banner (`TRUE`) or not
#' }
"Banner"

#' Data: Transition
#'
#' The dataframe refers to data on the employees of a company that adopted a new
#' management software system to replace its legacy tools. To facilitate a
#' smooth transition, all administrative staff will participate in a structured
#' training program designed to build proficiency with the new system. As part
#' of the rollout, employees from two departments have been selected to complete
#' the training first. The dataset contains information on each
#' (trained) employee's productivity using the old and the new software,
#' recorded after a defined period of use.
#'
#' @usage data(Transition)
#'
#' @format{
#'   A dataframe with *n* = 130 observations and the following 4 variables
#'   (levels of the variables listed in alphabetical order):
#'   * **Employee** (`int`): employee id
#'   * **Pre** (`num`): employee’s productivity with the old software
#'   * **Post** (`num`): employee’s productivity with the new software
#'     (after the training program)
#'   * **Department** (`chr`): employee’s department (Dept1, Dept2)
#' }
"Transition"
