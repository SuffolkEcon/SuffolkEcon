#' Plot a one-dimensional, user-defined function
#'
#'
#' @author Lawrence R. De Geest
#' @param f a single-line function 
#' @param lower an integer indicating the lower bound
#' @param upper an integer indicating the upper bound
#' @param optimize optimize the function and plot the optimum and FOC (defaults to FALSE). If TRUE it defaults to finding the global minimum. Set \code{maximum = TRUE} to find the global maximum.
#' @param maximize if \code{optimize = TRUE} find the maximum (defaults to FALSE and finds the minimum)
#' @param roots find and plot the roots over the given domain (defaults to FALSE)
#' @param optim use \code{optim()} to find optima (defaults to FALSE)
#' @param ... additional arguments passed to \code{optim()} if \code{optim = TRUE}
#' @return \code{ggplot} object
#' @examples
#' # declare some function
#' ## right now the function needs to be declared on a single line
#' f = function(x) -x^2 + 10*x
#' # plot the function over the domain x = {0, ..., 10}
#' plot_function(f, lower = 0, upper = 10)
#'
#' # calculate and show the global maximum over the domain. Default is minimization, so set maximum to TRUE:
#' plot_function(f, lower = 0, upper = 10, optimize = TRUE, maximum = TRUE)
#'
#' # show that the optimal point occurs where the derivative is zero (i.e., the first-order condition):
#' plot_function(f, lower = 0, upper = 10, derivative = TRUE, roots = TRUE)
#'
#' # you can also define an anonymous function (i.e., define a function on the fly *inside* `plot_function`):
#' plot_function(function(x) sin(x) + sin(3*x) + cos(3*x), lower = 0, upper = 6)
#'
#' # and show all roots over the domain:
#' plot_function(function(x) sin(x) + sin(3*x) + cos(3*x), lower = 0, upper = 6, roots = TRUE)
#'
#' # plot_function uses uses `optimize()` by default, which implements Brent optimization. 
#' # It can struggle to find local optima for messy functions.
#' # For instance, it misses the local optimum:
#' plot_function(function(x) sin(x) + sin(3*x) + cos(3*x), lower = 0, upper = 6, optimize = TRUE)
#'
#' # you can switch to `optim()` and then set the starting value `par` (from `optim`) to get better results:
#' plot_function(function(x) sin(x) + sin(3*x) + cos(3*x), lower = 0, upper = 6, optimize = TRUE, optim = TRUE, par = 5)
#'
#' # since `plot_function` returns a ggplot object you can add ggplot stuff (e.g., change the theme, the titles, etc.)
#' plot_function(function(x) sin(x) + sin(3*x) + cos(3*x), lower = 0, upper = 6) +
#'     theme_minimal() +
#'     labs(subtitle = "the subtitle", title = "the title")
#' @export
plot_function = function(f, lower, upper, derivative = FALSE, optimize = FALSE, maximum = FALSE, roots = FALSE, optim = FALSE, ...) {
  # sanity checks
  if(length(formals(f))>1) stop("plot_function can only be used with functions of one argument", call. = FALSE)
  if(lower > upper) stop("Your lower bound is greater than the upper bound!", call. = FALSE)
  # make sure the function actually works (evaluates without error) with some random number within the range
  fun_works = rlang::with_handlers(
    error = ~ rlang::abort(
      c("something is wrong with the function you wrote",
        i = "Make sure you only specified a function with one argument",
        i = "And make sure the function only handles a numerical argument",
        i = "e.g. `function(x) -x^2 + 10*x`")),
    f(runif(n = 1, min = lower, max = upper))
  )
  data = data.frame(x = seq(lower, upper, 0.1))
  l = list(...) # capture the dots
  if(is.null(l$optim_fill)) l$optim_fill = "white"
  if(is.null(l$optim_color)) l$optim_color = "blue"
  if(is.null(l$color)) l$color ="black"
  if(is.null(l$hline_color)) l$hline_color = "gray"
  ## could add more stuff here...
  if(!derivative){
    p = ggplot(data = data, aes(x=x)) +
      stat_function(fun=f, size = 1, color = l$color)  +
      labs(x = bquote(italic(x)), y = bquote(italic('f(x)')), title =  parse(text = paste0("f(x) == ", deparse(f)[2]))) # note: parse() returns an expression()
    if(optimize) {
      if(optim){
        # use optim() to do optimization
        # need error handling here?
        if(is.null(l$par)) l$par = 1
        if(is.null(l$method)) l$method = "L-BFGS-B"
        if(maximum) control=list(fnscale=-1) else control = list(fnscale=1)
        if(l$method %in% c("L-BFGS-B", "Brent")){
          optimum = optim(par = l$par, fn = f, method = l$method, control = control, lower = lower, upper = upper)
        } else{
          optimum = optim(par = l$par, fn = f, method = l$method, control = control)
        }
        y = optimum$value
        x_optim = optimum$par
      } else{ # use optimize() which implements Brent
        optimum = optimize(f = f, lower = lower, upper = upper, maximum = maximum)
        y = optimum[[2]]
        x_optim = optimum[[1]]
      }
      step = 0.1*(max(data$x) - min(data$x)) # segment is 10% of the range
      opt = ifelse(maximum, 'Maximum:', 'Minimum:')
      subtitle = bquote(.(opt)~italic('x =')~.(round(x_optim,2))~','~italic('f(x) =')~.(round(y,2)))
      p = p +
        geom_segment(data = data, aes(x = (x_optim - step), y = y, xend = (x_optim + step), yend = y), color=l$optim_color) +
        geom_point(aes(x = x_optim, y = y), size = 3.5, pch = 21, fill = l$optim_fill, color = l$optim_color) +
        labs(subtitle = subtitle)
    }
    if(roots){
      # need error handling here?
      all_roots = rootSolve::uniroot.all(f, interval = c(min(data$x), max(data$x)))
      all_roots_df = data.frame(x=all_roots, y=0)
      p = p +
        geom_hline(yintercept = 0, linetype = 'dotted') +
        geom_point(data = all_roots_df, aes(x = x, y= y), size = 3.5, fill = l$optim_fill, color = l$optim_color)
    }
  } else{ # plot the derivative
    fprime = Deriv::Deriv(f) # going this route instead of numDeriv::grad, so I can get a function fprime and then root it
    data = mutate(data, derivative = fprime(x))
    p = ggplot(data = data, aes(x=x, y = derivative)) +
      geom_hline(yintercept = 0, color = l$hline_color) +
      geom_line(size = 1, color = l$color) +
    labs(x = bquote(italic(x)), y = expression(over(df, dx)),
         subtitle = "First derivative",
         title =  parse(text = paste0("f(x) == ", deparse(f)[2]))) +
      theme(axis.title.y  = element_text(angle = 0, vjust = 0.5))
    if(optimize || roots || maximum){
      all_roots = rootSolve::uniroot.all(fprime, interval = c(min(data$x), max(data$x)))
      all_roots_df = data.frame(x=all_roots, y=0)
      p = p + geom_point(data = all_roots_df, aes(x = x, y= y), size = 3.5, pch = 21, fill = l$optim_fill, color = l$optim_color)
    }
  }
  return(p)
}


#' Simple demo of OLS
#'
#' Plot a random scatter of Y and X variables, then plot the OLS line with errors
#'
#' @author Lawrence R. De Geest
#' @param n number of observations. Defaults to 10 (smaller `n` is easier to visualize).
#' @param betas vector of coefficients (intercept and slope). Defaults to \code{c(intercept = 1, slope = 1)}
#' @param x vector of parameters of the normal distribution of the independent variable. Defaults to \code{c(mean = 0, sd = 1)}
#' @param error vector of parameters of the normal distribution of the error term. Defaults to \code{c(mean = 0, sd = 1)}
#' @param seed seed for the random number generator. Defaults to NULL (i.e., varies each time you run it)
#' @param show_raw_data plot the raw data alongside the plot with the regression line. Defaults to \code{TRUE}
#' @return \code{ggplot} object if \code{plot = TRUE}, otherwise a \code{data.frame}
#' @examples
#' # default values are probably sufficient to the the main idea across
#' plot_ols()
#'
#' # add some noise
#' plot_ols(error = c(mean = 0, sd = 10))
#'
#' # add some more points and make the relationship negative
#' plot_ols(n=50, betas = c(intercept = 10, slope = -5), error = c(mean = 0, sd = 10))
#' @export
plot_ols = function(n = 10,
                    betas = c(intercept = 1, slope = 1),
                    x = c(mean = 0, sd = 1),
                    error = c(mean = 0, sd = 1),
                    seed = NULL,
                    show_raw_data = TRUE,
                    ...){
  # play some defense
  if(!is.numeric(n)) stop("n must be a whole number (e.g., n = 10)", call. = FALSE)
  if(!is.null(seed) & !is.numeric(seed)) stop("seed for the random number generator must be a number (e.g., seed = 123)", call. = FALSE)
  if(!is.numeric(betas)) stop("betas (intercept and slope) must be numeric (e.g., betas = c(beta1 = 1, beta2 = 1))")
  if(!is.numeric(x)) stop("mean and standard deviation of the data must be numeric (e.g., x = c(mean = 0, sd = 1))")
  if(!is.numeric(error)) stop("mean and standard deviation of the random error must be numeric (e.g., error = c(mean = 0, sd = 1))")
  # collect optional arguments
  l = list(...)
  if(!is.null(seed)) set.seed(seed)
  if(is.null(l$point_color)) l$point_color = "black"
  if(is.null(l$point_size)) l$point_size = 3
  if(is.null(l$theme)) l$theme = theme_gray()
  # create X and y(X)
  X = rnorm(n, x[1], x[2])
  y = betas[1] + betas[2]*X + rnorm(n, error[1], error[2])
  # estimate the model
  lmfit = lm(y~X)
  # model output
  sse = round(sum(resid(lmfit)^2),2)
  b0 = round(coef(lmfit)[1],2)
  b1 = round(coef(lmfit)[2],2)
  # data
  data = data.frame(y, X)
  # plot the raw data
  p1 = data %>%
    ggplot() +
    geom_point(aes(x = X, y = y), size = l$point_size , color = l$point_color) +
    labs(x = "X", y = "y", title = "Raw data", subtitle = expression(y == f(X))) +
    l$theme
  # now raw data with reg line
  p2 = data %>%
    mutate(fitted = fitted(lmfit), resid = y - fitted) %>%
    ggplot() +
    geom_segment(aes(x = X, y = y, xend = X, yend = fitted, alpha = abs(resid)), size = 1) +
    geom_point(aes(x = X, y = y, alpha = abs(resid)), size = l$point_size, color = l$point_color) +
    geom_smooth(aes(x = X, y = y), method = "lm", se = FALSE, formula = y ~ x, fullrange = T) + # need to explicitly set the formula argument to suppress messages
    labs(x = "X", y = "y", title = bquote("Linear model: y = "~.(b0)~+~.(b1)~"X"),
         subtitle = bquote("Sum of squared errors: "~y~-~hat(y)~"="~.(sse))) +
    guides(alpha = FALSE) +
    l$theme
  if(show_raw_data) combo = p1 + p2 else combo = p2
  return(combo)
}


#' Plot t-distribution and visualize two-tailed p-value for a given t-statistic and degrees of freedom
#'
#'
#' @author Lawrence R. De Geest
#' @param t-value a value from the Student's T distribution
#' @param dof degrees of freedom
#' @param fill fill of the shaded area underneath the distribution for a given t-value. Defaults to "grey35" (ggplot2 default)
#' @param color color of the outline of the kernel density. Defaults to "grey35" (ggplot2 default)
#' @return \code{ggplot} object
#' @note add switch to plot standard normal and calculate p-values instead of t-distribution
#' @examples
#' # plot t distribution for 100 degrees of freedom, then calculate and shade-in the p-value for t = 1.25
#' plot_t_value(t_value = 1.25, dof = 100)
#' @export
plot_t_value = function(t_value, dof, fill = "grey35", color = "grey35", alpha = 1) {
  if(!(is.numeric(t_value))) rlang::abort(c("`t_value` must be a number", i = "e.g. `t_value = 1.52`"))
  if(!(is.numeric(dof))) rlang::abort(c("`dof` must be a number", i = "e.g. `dof = 99`"))
  if(dof == 0 || dof < 0) rlang::abort(c("`dof` must be positive", i = "e.g. `dof = 99`"))
  t_stats = seq(from=-4, to=4, by = 0.01)
  t_densities = dt(t_stats, df = dof)
  df = data.frame("t_stats" = t_stats, "t_density" = t_densities)
  p = ggplot(df, aes(x=t_stats, y=t_density)) +
    geom_line(color=color, size = 1) +
    labs(
      x = "T values",
      y = "Density",
      title = bquote('t-distribution for'~.(dof)~'degrees of freedom'),
      subtitle = bquote('2-tailed probability for'~italic(t)~'='~.(t_value)~':'~italic('p ='~.(round(2*pt(-abs(t_value), df = dof), 4)))~'(shaded area)')
    )
  if(t_value == 0){
    p = p + geom_area(aes(t_stats), fill = fill)
  } else{
    p = p +
      geom_area(aes(t_stats), fill = NA) +
      geom_area(data = dplyr::filter(df, t_stats > abs(t_value)),  fill = fill, alpha = alpha) +
      geom_area(data = dplyr::filter(df, t_stats < -abs(t_value)), fill = fill, alpha = alpha)
  }
  return(p)
}


#' Plot random numbers from a distribution
#'
#' @author Lawrence R. De Geest
#' @param model one of R's built-in probability distributions (e.g., 'rnorm', 'rpois', 'runif').
#' @param n number of observations. Defaults to 1000
#' @param density plot the kernel density estimates instead of the histogram. Defaults to FALSE.
#' @param cumulative show the cumulative distribution as well as the PDF? Defaults to FALSE.
#' @param fill fill color of the histogram or density. Defaults to "grey35" (the ggplot2 default)
#' @param alpha transparency of the histogram or density. Defaults to 1 (no transparency)
#' @param distribution name of the distribution (a string). Used in the plot title(s). Defaults to NULL, in which case `model` is used in the plot title.
#' @param details details about the distribution (e.g., mean and sd). Used in the plot subtitle(s). Defaults to NULL.
#' @param ... additional arguments passed to `model` (e.g. mean and sd for `rnorm`, lambda for `rpois`, etc.)
#' @return \code{ggplot} object
#' @examples
#' # plot the standard normal
#' plot_distribution(model = 'rnorm')
#'
#' # plot the standard normal, but this time show the density, change the fill and transparency, and add a name for a nicer title
#' plot_distribution(model = 'rnorm', density = TRUE, fill = "blue", alpha = 0.75, distribution = "Normal")
#'
#' # include the cumulative distribution
#' plot_distribution(model = 'rnorm', density = TRUE, cumulative = TRUE, fill = "blue", alpha = 0.75, distribution = "Normal")
#'
#' # use ... to send additional arguments to `model`.
#' # for instance, shift the distribution:
#' plot_distribution(model = 'rnorm', mean = 15, sd = 5, distribution = "Normal")
#'
#' # use the details argument to make a subtitle with more details about the distribution
#' plot_distribution(model = 'rnorm', mean = 15, sd = 5, distribution = "Normal", details = "Mean 15, SD 5")
#'
#' # uniform distribution
#' plot_distribution(model = 'runif', min = 0, max = 30, distribution = "Uniform")
#'
#' # Poisson distribution
#' plot_distribution(model = 'rpois', lambda = 5, cumulative = TRUE, distribution = "Poisson")
#'
#' @export
plot_distribution = function(model, n = 1000, density = FALSE, cumulative = FALSE, distribution = NULL, details = NULL, fill = "grey35", color  = "grey35", alpha = 1, ...){
  # make sure the function exists
  f = rlang::with_handlers(
    error = ~ rlang::abort(c("unknown function", x = paste0("Could not find the function ",paste0("'",model,"'")),
                             i =  "Did you spell it correctly? It should start with an 'r' (e.g. rnorm, runif, rpois)",
                             i = "For a demo try `model = rnorm`")),
    match.fun(model)
  )
  # now make sure it's an "r" function (e.g rnorm, not dnorm)
  if(substring(model,1,1) != "r") stop("make sure the model generates random numbers (e.g., 'rnorm' instead of 'dnorm', etc.)", call. = FALSE)
  # if they don't provide a name just use the function itself for the title (minus the "r")
  if(is.null(distribution)) distribution = substring(model,2)
  # throw a warning if density = TRUE for a discrete distribution
  if(model %in% c("rpois", "rbinom") && density) warning(model, ' is a discrete probability distribution and not suitable for density = TRUE', call. = FALSE)
  # ok, make the data
  data = rlang::with_handlers(
    error = ~ rlang::abort(c("problem simulating the distribution",
                             x = "Unknown arguments passed to `model`",
                             i =  "Did you forget to explicitly state a named argument?")),
    data.frame(x = f(n, ...))
  )
  # base plot
  p_base = data %>%
    ggplot(aes(x))
  if(density){
    p1 = p_base  +
      geom_density(color = NA, fill = fill, alpha = alpha) +
      labs(x = "Random Variable (X)", y = "Density", title = paste0(distribution, " PDF"), subtitle = details)
  } else{
    p1 = p_base +
      geom_histogram(bins = 30, fill = fill, alpha = alpha) +
      labs(x = "Random Variable (X)", y = "Frenquency", title = paste0(distribution, " histogram"), subtitle = details)
  }
  if(cumulative){
    p2 = p_base +
      stat_ecdf(size = 1, color = color) +
      labs(x = "Random Variable (X)", y = "Proportion", title = paste0(distribution, " CDF"), subtitle = details)
    return(p1 + p2)

  } else{

    return(p1)

  }
}

#' Plot probabilities as areas underneath the PDF
#'
#' @author Lawrence R. De Geest
#' @param model one of R's built-in probability distributions (e.g., 'rnorm', 'rpois', 'runif').
#' @param lower lower bound of P(a <= x <= b), i.e. b. Defaults to NULL.
#' @param uppder upper bound of P(a <= x <= b), i.e. a. Defaults to NULL.
#' @param density plot the kernel density estimates instead of the histogram. Defaults to FALSE.
#' @param fill fill color of the histogram or density. Defaults to "grey35" (the ggplot2 default)
#' @param alpha transparency of the histogram or density. Defaults to 1 (no transparency)
#' @param name name of the distribution (a string). Used in the plot title(s). Defaults to NULL, in which case `model` is used in the plot title.
#' @param details details about the distribution (e.g., mean and sd). Used in the plot subtitle(s). Defaults to NULL.
#' @param ... additional arguments passed to `model` (e.g. mean and sd for `rnorm`, lambda for `rpois`, etc.)
#' @note Should this function merge with `plot_distribution()`?
#' @return \code{ggplot} object
#' @examples
#' # plot the standard normal and show the entire distribution sums to 1
#' plot_probability(model = 'rnorm', lower = -Inf, distribution = "Standard Normal", details = "Mean 0 SD 1")
#'
#' # ...and half the distribution sums to 0.5
#' plot_probability(model = 'rnorm', lower = 0, distribution = "Standard Normal", details = "Mean 0 SD 1")
#'
#' # show the area between 0 and 1 on the standard normal
#' plot_probability(model = 'rnorm', lower = 0, upper =1,  distribution = "Standard Normal", details = "Mean 0 SD 1")
#'
#' # Chi-squared distribution with 5 degrees of freedome
#' plot_probability(model = 'rchisq', df = 5, lower = 8, distribution = "Chi-squared", details = "5 degrees of freedom")
plot_probability = function(model, lower=NULL, upper=NULL, distribution = NULL, details = NULL, fill = "grey35", color  = "grey35", alpha = 1, ...){

  # make sure the function exists
  f = rlang::with_handlers(
    error = ~ rlang::abort(c("unknown function", x = paste0("Could not find the function ",paste0("'",model,"'")),
                             i =  "Did you spell it correctly? It should start with an 'r' (e.g. rnorm, runif, rpois)",
                             i = "For a demo try `model = rnorm`")),
    match.fun(model)
  )

  # now make sure it's an "r" function (e.g rnorm, not dnorm)
  # it's a bit hacky since we in fact need the density function!
  # but this way the argument is the same as in plot_distribution()
  # revisit this
  if(substring(model,1,1) != "r") stop("make sure the model generates random numbers (e.g., 'rnorm' instead of 'dnorm', etc.)", call. = FALSE)
  # if they don't provide a name just use the function itself for the title (minus the "r")
  if(is.null(distribution)) distribution = substring(model,2)
  if(is.null(lower)) lower = -Inf
  if(is.null(upper)) upper = Inf
  # ok, get the draw
  draws = rlang::with_handlers(
    error = ~ rlang::abort(c("problem simulating the distribution",
                             x = "Unknown arguments passed to `model`",
                             i =  "Did you forget to explicitly state a named argument?")),
    f(n=10^4, ...)
  )

  # now get the corresponding density function
  d = match.fun(paste0("d", substring(model,2)))

  # build data frame
  ## generate a shorter sequence with same bounds so that it still ensures a smooth CDF
  ### otherwise geom_area takes too long
  data = data.frame(x =seq(min(draws), max(draws), length.out = 10^3))

  # add the densities (d is the matched density function)
  data$y = d(data$x, ...)

  # plot the pdf
  p = data %>%
    ggplot(aes(x = x, y = y)) +
    labs(x = "Random Variable (X)", y = "Density", title = paste0(distribution, " PDF"), subtitle = details)

  # get the shaded area
  shade = filter(data, x > lower & x < upper)

  # get the p function to calculate area
  pfunc = match.fun(paste0("p", substring(model,2)))
  pvalue = pfunc(upper, ...) - pfunc(lower, ...)

  # update the subtitle
  if(is.null(details)) new_subtitle = paste0("Shaded area = ", round(pvalue, 4))
  else new_subtitle = paste0(details, "\n", paste0("Shaded area = ", round(pvalue, 4)))

  # update the plot
  p = p +
    geom_area(data = shade, aes(x = x, y = y), fill = fill, alpha = alpha) +
    geom_line(size = 1, color = color) +
    labs(subtitle = new_subtitle)

  return(p)

}
