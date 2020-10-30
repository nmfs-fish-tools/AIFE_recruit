fit_split <- function(formula, model, split, ...) {
  wf <- workflows::add_model(workflows::add_formula(workflows::workflow(), 
                                                    formula, 
                                                    blueprint = hardhat::default_formula_blueprint(indicators = FALSE, allow_novel_levels = TRUE)), 
                             model)
  tune::last_fit(wf, split, ...)
}