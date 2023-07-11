#' Data: MktDATA
#'
#' This dataset is a modification of the original \link{\code{MktDATA.Orig}}
#' dataset and it is provided for user convenience.
#'
#' @usage data(MktDATA)
#'
#' @description A data frame with 2224 observations and 26 variables.
"MktDATA"

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
#' @description A data frame with 2224 observations and the following 19
#' variables (levels of the variables listed in alphabetical order):
#' 
#' * **`CustId`** (num): customer's identification label
#' * **`Gender`** (chr): customer's gender (`F`, `M`)
#' * **`Age`** (num): customer's age (in years)
#' * **`Education`** (chr): customer's level of education(`College`, `Graduate`,
#    `HighSchool`,`Post-Grad`)
#' * **`Marital_Status`** (chr): customer's marital status
#    (`Divorced`, `Married`, `Single`, `Together`, #' `Widow`)
#' * **`Children`** (num): number of children in the household
#' * **`Kids`** (num): number of kids aged less than 12 in the
#'   household
#' * **`Income`** (chr): customer's income (measured in classes)
#' * **`Baseline`** (num): index (from 0 to 1) assigned by the marketing dept
#    indicating how promising the #' customer was judged at the beginning of
#    the observation period
#' * **`LikeMost`** (chr): Most frequently bought category in the last two
#    years (`P.A`, `P.B`, `P.C`)
#' * **`TotVal`** (num): amount spent in the last 2 years
#' * **`NPickUp_Purch`** (num): number of purchases made through company's
#    website and picked up in physical #' store  
#' * **`NWeb_Purch`** (num): number of purchases made through company's website
#    and delivered at home
#' * **`NStore_Purch`** (num): number of purchases made in a physical store
#' * **`NDeals`** (num): number of products purchases with discount
#' * **`CustClass`** (chr): customer's classification (assigned by the marketing
#    dept) based on past #' profitability (`Bronze`, `Gold`, `Platinum`,
#    `Silver`)
#' * **`PastCampaigns`** (num): number of offers accepted by the customer in the
#    last 2 years' marketing #' campaigns
#' * **`LastCampaign`** (num): binary variable (0/1) indicating whether (1) or
#    not (0) the customer accepted #' the offer in the campaign launched at
#    the end of the observation period
#' * **`WouldSuggest`** (chr): variable signalling whether (`Yes`) or not
#    (`No`) the customer declared they would suggest the company's products
#    to friends and family
#' 
#' @source The data set has been adapted from \href{https://www.kaggle.com/code/dmitryuarov/customers-clustering-eda}{https://www.kaggle.com/code/dmitryuarov/customers-clustering-eda}.
"MktDATA.Orig"
