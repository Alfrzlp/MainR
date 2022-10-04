library(tidymodels)

str(cartlm::cartLM)
set_new_model("dt_regressor")
set_model_mode(model = "dt_regressor", mode = "regression")
set_model_engine(
  "dt_regressor", 
  mode = "regression", 
  eng = "cartlm"
)
set_dependency("dt_regressor", eng = "cartlm", pkg = "cartlm")

set_model_arg(
  model = "dt_regressor",
  eng = "cartlm",
  parsnip = "sub_classes",
  original = "subclasses",
  func = list(pkg = "foo", fun = "bar"),
  has_submodel = FALSE
)

show_model_info("dt_regressor")


dt_regressor <-
  function(mode = "regression",  sub_classes = NULL) {
    # Check for correct mode
    if (mode  != "regression") {
      rlang::abort("`mode` should be 'regression'")
    }
    
    # Capture the arguments in quosures
    args <- list(sub_classes = rlang::enquo(sub_classes))
    
    # Save some empty slots for future parts of the specification
    new_model_spec(
      "dt_regressor",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }




set_fit(
  model = "discrim_mixture",
  eng = "mda",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data"),
    func = c(pkg = "mda", fun = "mda"),
    defaults = list()
  )
)

set_encoding(
  model = "discrim_mixture",
  eng = "mda",
  mode = "classification",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)


class_info <- 
  list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      # These lists should be of the form:
      # {predict.mda argument name} = {values provided from parsnip objects}
      list(
        # We don't want the first two arguments evaluated right now
        # since they don't exist yet. `type` is a simple object that
        # doesn't need to have its evaluation deferred. 
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
  )

set_pred(
  model = "discrim_mixture",
  eng = "mda",
  mode = "classification",
  type = "class",
  value = class_info
)


prob_info <-
  pred_value_template(
    post = function(x, object) {
      tibble::as_tibble(x)
    },
    func = c(fun = "predict"),
    # Now everything else is put into the `args` slot
    object = quote(object$fit),
    newdata = quote(new_data),
    type = "posterior"
  )

set_pred(
  model = "discrim_mixture",
  eng = "mda",
  mode = "classification",
  type = "prob",
  value = prob_info
)

show_model_info("discrim_mixture")
#> Information for `discrim_mixture`
#>  modes: unknown, classification 
#> 
#>  engines: 
#>    classification: mda
#> 
#>  arguments: 
#>    mda: 
#>       sub_classes --> subclasses
#> 
#>  fit modules:
#>  engine           mode
#>     mda classification
#> 
#>  prediction modules:
#>              mode engine     methods
#>    classification    mda class, prob

