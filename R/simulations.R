#' Bootstrap sampling distributions ("what if we ran the experiment many times...")
#'
#' Estimate the sampling distribution of a sample statistic or regression coefficient from a linear model (\code{lm})
#'
#' @author Lawrence R. De Geest
#' @param data data of class \code{data.frame}
#' @param variable a continuous variable from the data frame. Only relevant if using `stat` (to boostrap a sample statistic)
#' @param stat an R summary statistic function (e.g., `mean()`, `median()`, `max()`, `sd()`) entered as a string (e.g., "mean", "median", "max", "sd")
#' @param model a model or formula, e.g., `y ~ x1 + x2`. Use this to boostrap a linear model
#' @param focus an independent variable from \code{model} (defaults to FALSE). Only required if \code{model} is a multiple linear regression
#' @param group categorical grouping variable from \code{data} (defaults to NULL). If activated each simulation evenly samples \code{data} according to \code{group}
#' @param experiments number of experiments, i.e. runs of the boostrap sampler (defaults to 100)
#' @param sample_size sample size of each experiment (defaults to 100)
#' @param plot plot the sampling distribution of the regression coefficient (defaults to TRUE)
#' @param p.values plot the cumulative distribution of p-values and report the number of significant results (defaults to TRUE)
#' @param seed random number generator seed
#' @param ... additional arguments to adjust the plots
#' @return \code{ggplot} object if \code{plot = TRUE}, otherwise a \code{data.frame}
#' @examples
#' # using the nhanes data
#' data("nhanes", package = "SuffolkEcon")
#'
#' # sampling distribution of the mean of height
#' what_if(data = nhanes, variable = height, stat = 'mean')
#'
#'# sampling distribution and p-values for lm(formula = weight ~ height, data = nhanes)
#' what_if(data = nhanes, model = weight ~ height, sample_size = 30, experiments = 100, p.values = TRUE)
#'
#' # increase the number of experiments
#' what_if(data = nhanes, model = weight ~ height, sample_size = 30, experiments = 1000, p.values = TRUE)
#' @export
what_if = function(data, variable=NULL, model=NULL, stat=NULL, group=NULL, focus=NULL, experiments=100L, sample_size=100L, plot=TRUE, p.values=FALSE, seed = sample.int(.Machine$integer.max, 1), ...) {

  #=================================
  # CHECKS
  #=================================
  if(sample_size > nrow(data)) stop("Sample size cannot exceed the total available data", call. = FALSE)
  if(!(experiments%%1==0) || !(experiments > 0)) stop("`experiments` must be an integer", call. = FALSE)
  if(!(sample_size%%1==0) || !(sample_size > 0)) stop("`sample_size` must be a positive integer", call. = FALSE)
  if(!is.null(model) & !is.null(stat)) stop("choose one of `model` (bootstrap a linear model) or `stat` (bootstrap a statistic)", call. = FALSE)
  if(is.null(model) & is.null(stat)) stop("choose one of `model` (bootstrap a linear model) or `stat` (bootstrap a statistic)", call. = FALSE)

  # seed
  set.seed(seed)

  #=================================
  # A. BOOTSTRAP A SAMPLE STATISTIC
  #=================================
  if(!is.null(stat)){
    var_quo = enquo(variable)
    # make sure the variable is in the dataframe
    var_exists = rlang::with_handlers(
      error = ~ rlang::abort(c("unknown variable", x = "That variable is not a column in the data you provided", i =  "Did you spell it correctly?")),
      data %>% pull(!!var_quo)
    )
    # if it is, make sure it's numeric
    if(!is.numeric(pull(data, !!var_quo))) stop("`variable' must be numeric", call. = FALSE)
    # make sure the function exists
    f = rlang::with_handlers(
      error = ~ rlang::abort(c("unknown function", x = paste0("Could not find the function ",paste0("'",stat,"'")), i =  "Did you spell it correctly?",, i = "For a demo try `stat = mean`")),
      match.fun(stat)
    )

    # sim function
    sim_stat = function() {
      sim_data = dplyr::slice_sample(.data = data, n = sample_size)
      vec = sim_data %>% pull(!!var_quo)
      m = f(na.omit(vec))
      return(m)
    }
    sim_results = vector("double", length = experiments)
    for(i in seq_along(sim_results)) sim_results[i] = sim_stat()
    sim_results = data.frame("x" = sim_results)

    if(plot){
      l = list(...) # optional arguments only available for plotting
      if(is.null(l$bins)) l$bins = 30
      if(is.null(l$fill)) l$fill = "grey35"
      p = ggplot(sim_results, aes(x)) +
        geom_histogram(bins = l$bins, fill = l$fill) +
        labs(x = "Sample statistics",
             y = "Frequency",
             title = bquote("Sampling distribution of"~.(stat)~.(var_quo)),
             subtitle = bquote(.(experiments) ~ "experiments"))
      return(p)
    } else{
      return(sim_results)
    }
  }

  #==============================
  # B. BOOTSTRAP THE LINEAR MODEL
  #==============================

  else {
    model_call = rlang::with_handlers(
      error = ~ rlang::abort(
        c("could not construct the model",
          x = "Check that you correctly specified the model",
          i = "LHS and RHS variables should be separated by `~`",
          i = "RHS variables should be separated by `+`",
          i = "e.g. `y ~ x1 + x2`")),
      formula(model)
    )

    f = lapply(as.list(model_call), as.character)
    if(f[[2]] %in% f[[3]]) rlang::abort(c("model incorrectly specified",
                                          x = "LHS variable cannot appear on RHS",
                                          x = "Incorrect: `y ~ x + y`",
                                          i = "Correct: `y ~ x`"))
    bad_rhs = grepl("-", paste(f[[3]], collapse = " "))
    if(bad_rhs) rlang::abort(c("model incorrectly specified",
                               x = "RHS variables cannot be separated by subtraction or division",
                               i = "Use `+` to separate RHS terms and use `*` for interactions",
                               i = "e.g. `y ~ x1 + x2 + x1*x2`"))

    #### now gather stuff
    feature_quo = enquo(focus)
    model_vars = all.vars(model_call)
    dep_var = model_vars[1]
    ind_vars = model_vars[-1]

     #### now some more checks
    #1. is the response numeric? if not, shut down
    response_is_numeric = pull(data, dep_var) %>% is.numeric()
    if(!response_is_numeric) stop("y variable must be numeric")
    #2. is the model a multiple regression? if yes set multireg = TRUE
    if(length(model_vars) > 2) multireg = TRUE else multireg = FALSE
    #3. if the model is a multiple regression check if the feature argument is null. if so, shut down
    if(multireg & rlang::quo_is_null(feature_quo)) stop("For multiple regression you must use `focus` to select a focus variable for the sampling distribution", call. = FALSE)
    # 4. if the feature argument is not in the model arugment, shut down
    if(multireg & is.na(match(as_label(feature_quo), ind_vars))) stop("Focus variable must be part of the model", call. = FALSE)
    # 5. extract the feature_var. feature_var is a string, using this for plotting & subsetting
    # 5.1  check if feature is numeric. if FALSE then it's a character or factor. this matters for the plotting/subsetting
    feature_var = ifelse(multireg, as_label(feature_quo), ind_vars)
    feature_is_numeric = pull(data, feature_var) %>% is.numeric()
    # 6. check if we need to sample by groups
    group_quo = enquo(group)
    sample_by_group = !(rlang::quo_is_null(group_quo)) # easier to think about this to evaluate to TRUE if there is grouping but this is probably dumb
    if(sample_by_group) n_groups = data %>% pull(!!group_quo) %>% unique() %>% length()
    # 7. unpack optional arguments
    l = list(...)
    if(is.null(l$alpha)) l$alpha = 1
    if(is.null(l$fill)) l$fill = "grey35"
    if(is.null(l$color)) l$color = "grey35"
    if(is.null(l$bins)) l$bins = 30
    if(is.null(l$theme)) l$theme = theme_gray()

    #===============
    # run sims
    #===============

    # sample-then-reg function
    sim_lm = function() {
      sim_data = dplyr::slice_sample(data, n = sample_size)
      fit = lm(model_call, sim_data)
      tidy_fit = broom::tidy(fit)
      return(tidy_fit)
    }

    # run the sims
    ## bit faster than replicate (which has some overhead)
    sim_results_list = vector(mode = "list", length = experiments)
    for(i in seq_along(sim_results_list)){
      sim_results_list[[i]] = sim_lm()
    }
    sim_results = dplyr::bind_rows(sim_results_list)

    if(plot){
      # prepare plot data
      if(feature_is_numeric) plot_data = sim_results %>% filter(term == feature_var)
      else plot_data = sim_results %>% filter(str_detect(term, feature_var)) # inefficient because I search the entire term column
      # plot caption
      ### note: departsing the bquote version didn't work
      #caption = bquote(.(experiments)~experiments*","~.(sample_size)~obs.~per~sample)
      #caption = gsub("~ ", "", deparse(caption))
      caption = paste0(paste0(experiments, " experiments, "), paste0(sample_size, " obs. per experiment"))
      p_value_caption = ifelse(multireg, "\n", " ") # hack to keep the spacing nice
      # plot subtitles
      if(multireg) subtitle = bquote(bold('Model:')~italic(.(dep_var)~'=' ~ beta[0] + beta[1] ~ .(feature_var) + 'controls' + epsilon))
      else subtitle = bquote(bold('Model:')~italic(.(dep_var)~'=' ~ beta[0] + beta[1] ~ .(feature_var) + epsilon))
      if(multireg) {
        controls = ind_vars[!grepl(feature_var, ind_vars)]
        controls_label = paste0('Controls: ', paste(controls, collapse = ', '))
        caption = paste0(caption, "\n", controls_label)
      }
      # plot sampling distribution of feature coefficient
      p = ggplot(plot_data, aes(estimate)) +
        geom_histogram(bins = l$bins, fill = l$fill, alpha = l$alpha) +
        scale_y_continuous(expand = c(0,0)) +
        labs(x = bquote('Coefficient estimates for'~.(feature_var)~'('~hat(beta)[1]~')'),
             y = "Frequency",
             title = bquote('Sampling distribution of '~ beta[1]),
             subtitle = subtitle,
             caption = caption) +
        l$theme +
        theme(plot.caption = element_text(hjust = 0, face= "bold"))
      if(p.values){
        # prepare data
        pct_sig = plot_data %>%
          mutate(pct_sig = if_else(p.value < 0.05, 1, 0)) %>%
          summarise(mean(pct_sig)) %>%
          pull()
        # plot
        p2 = ggplot(plot_data, aes(p.value)) +
          scale_y_continuous(limits = c(-0.01, 1.01), breaks = c(0.0, 0.25, 0.50, 0.75, 1.0)) +
          xlim(c(0,1.05)) +
          stat_ecdf(geom='point', alpha=l$alpha, size=2, color = l$color) +
          annotate("rect", xmin = 0, xmax = 0.05, ymin = 0, ymax = 1, alpha = .25) + # if you want the area
          geom_vline(xintercept = 0.05, linetype = 'dashed', color = 'black') +
          labs(x = bquote('Probabilities of'~hat(beta)[1]~'under true'~H[0]~'(p)'),
               y = 'Proportion',
               subtitle = bquote(''%~~% .(100*pct_sig)~'% of estimates significant (p < 0.05)'),
               title = bquote('P-values'), caption = p_value_caption) +
          l$theme
        p_combined = p + p2 # using library(patchwork)
        return(p_combined)
      } else { # plot coefficients but not pvalues
        return(p)
      }
    } else { # no plot, return data frame of sim results
      return(sim_results)
    }
  }
}


#' Flip a fair coin
#'
#' Useful for demonstrating the Law of Large Numbers
#'
#' @author Lawrence R. De Geest
#' @param flips an integer (defaults to 100)
#' @param seed random number seed
#' @return \code{ggplot} object
#' @examples
#' # Flip a coin 1000 times and plot the probabilities of Heads and Tails:
#' flip_coin(flips = 1000)
#' @export
flip_coin = function(flips=100L, seed = sample.int(.Machine$integer.max, 1)) {
  if(!(flips%%1==0)) rlang::abort(c("`flips` must be an integer", i = "Example: `flips = 10`"))
  set.seed(seed)
  coin = c("Heads", "Tails")
  outcomes = sample(coin, size = flips, replace = TRUE) %>%
    table() / flips
  df = tibble(as.data.frame(outcomes))
  colnames(df) = c("Outcome", "Probability")
  if(nrow(df) == 1) df = add_row(df, Outcome = coin[-(match(df$Outcome[1], coin))], Probability = 0)
  p = ggplot(df, aes(x = Outcome, y = Probability)) +
    geom_col() +
    labs(x = "Outcome", y = "Probability (# outcomes / # flips)", title = bquote(.(flips)~'coin flips')) +
    ylim(c(0,1)) +
    geom_hline(yintercept = 0.5, color = 'red')
  return(p)
}

#' Roll a fair die
#'
#' Useful for demonstrating the Law of Large Numbers
#'
#' @author Lawrence R. De Geest
#' @param rolls an integer (defaults to 100)
#' @param seed random number seed
#' @return \code{ggplot} object
#' @examples
#' # Roll a die 1000 times and plot the probabilities of each outcome:
#' roll_die(rolls = 1000)
#' @export
roll_die = function(rolls=100L, seed = sample.int(.Machine$integer.max, 1)){
  if(!(rolls%%1==0)) rlang::abort(c("`rolls` must be an integer", i = "Example: `rolls = 10`"))
  set.seed(seed)
  die = seq(1,6,1)
  outcomes = sample(die, size = rolls, replace = TRUE) %>%
    table() / rolls
  df = tibble("Outcome" = as.numeric(rownames(outcomes)), "Probability" = as.numeric(outcomes))
  if(nrow(outcomes) < length(die)) df = add_row(df, Outcome = which(!(die %in% df[[1]])), Probability = 0)
  p = ggplot(df, aes(x = factor(Outcome), y = Probability)) +
    geom_col() +
    labs(x = "Outcome", y = "Probability (# outcomes / # rolls)", title = bquote(.(rolls)~'die rolls')) +
    ylim(c(0,1)) +
    geom_hline(yintercept = 1/6, color = 'red')
  return(p)
}
