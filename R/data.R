#' Second National Health and Nutrition Examination Survey (NHANES II 1976-1980).
#'
#' From the CDC:
#' "The second National Health and Nutrition Examination Survey, NHANES II, is a nationwide probability sample of 27,801 persons from 6 months - 74 years of age. From this sample, 25,286 people were interviewed and 20,322 people were examined, resulting in an overall response rate of 73 percent. Because children and persons classified as living at or below the poverty level were assumed to be at special risk of having nutritional problems, they were sampled at rates substantially higher than their proportions in the general population. Adjusted sampling weights were computed within 76 age-sex income groups in order to inflate the sample to closely reflect the target population at the midpoint of the survey."
#'
#' @format A data frame with 10351 rows and 58 variables. Key variables include:
#' \describe{
#'   \item{height}{height in inches}
#'   \item{weight}{weight in kilograms}
#'   \item{sex}{Male or Female}
#'   \item{race}{White, Black or Other}
#'   \item{age}{age in years}
#'   \item{bpsystol}{systolic blood pressure}
#'   \item{heartatk}{1 if the individual had a heart attack}
#'   \item{diabetes}{1 if the individual has diabetes}
#'   \item{rural}{1 if the individual lives in a rural county}
#'   ...
#' }
#' @note This data set is useful for teaching introductory topics. It can be treated as a random sample of U.S. adults. A number of the variables (e.g. \code{weight}, \code{height}) are normally distributed. Teaching regression is easier when using a model like \code{lm(weight ~ height)} because height can affect weight but not the other way round. For more recent NHANES data see the R packages \code{NHANES} (\url{https://cran.r-project.org/web/packages/NHANES/NHANES.pdf}) and \code{nhanesA} (\url{https://cran.r-project.org/web/packages/nhanesA/index.html}).
#' @source \url{https://wwwn.cdc.gov/nchs/nhanes/nhanes2/default.aspx}
"nhanes"

#' Body measurements for three species of penguins on the Palmer Archipelago in Antarctica.
#'
#'
#' @format A data frame with 333 rows and 8 variables:
#' \describe{
#'   \item{species}{a factor denoting penguin species (Adélie, Chinstrap and Gentoo)}
#'   \item{island}{a factor denoting island in Palmer Archipelago, Antarctica (Biscoe, Dream or Torgersen)}
#'   \item{bill_length_mm}{a number denoting bill length (millimeters)}
#'   \item{bill_depth_mm}{a number denoting bill depth (millimeters)}
#'   \item{flipper_length_mm}{an integer denoting flipper length (millimeters)}
#'   \item{body_mass_g}{an integer denoting body mass (grams)}
#'   \item{sex}{a factor denoting penguin sex (female, male)}
#'   \item{year}{an integer denoting the study year (2007, 2008, or 2009)}
#' }
#' @note \code{penguins} is a great introductory data set used throughout the data science and machine learning community (all the variables are exogenous; penguins can't choose their measurements!). To make things a bit easier, all rows with NAs were removed (to avoid issues like passing \code{na.rm = TRUE} to summary routines like \code{mean()} and so on).
#' @source This data set is borrowed from the R package \code{palmerpenguins} by Allison Horst, Alison Hill and Kristen Gorman (\url{https://allisonhorst.github.io/palmerpenguins/}). Check out their webpage for complimentary penguin artwork!
"penguins"

#' Housing prices and attributes from 1976.
#'
#'
#' @format A data frame with 506 rows and 12 variables, including:
#' \describe{
#'   \item{lprice}{log median house price in the community}
#'   \item{nox}{Nitrous Oxide (NOX) concentration; parts per million}
#'   \item{crime}{number of reported crimes per capita}
#'   \item{rooms}{average number of rooms in houses in the community}
#'   \item{dist}{weighted distance of the community to 5 employment centers}
#'   \item{stratio}{average student-teacher ratio of schools in the community}
#'   \item{proptax}{property taxes}
#'   ...
#' }
#' @note \code{hprice} is useful for teaching hedonic pricing (namely, the relative value of pollution as captured by housing prices), and linear regression in general.
#' @source Harrison, Jr., David, Rubinfeld, Daniel L. (1978/03)."Hedonic housing prices and the demand for clean air." Journal of Environmental Economics and Management 5(1): 81-102. <http://hdl.handle.net/2027.42/22636> \url{https://deepblue.lib.umich.edu/handle/2027.42/22636}
"hprice"

