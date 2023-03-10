#' Drop-down question for learnr tutorials.
#'
#'
#' Add interactive drop-down tasks to your `learnr` tutorials.
#' An alternative option to `question_radio` when you have a lot of options
#' and want to save space.
#'
#'
#' @param ... parameters passed onto learnr answer.
#' @param style can change display of question to "notes" or "exam"
#' @inheritParams learnr::question
#'
#' @return A custom `learnr` question, with `type = dropdown`.
#'
#' @examples
#' question_dropdown(
#'   "Pick the letter B",
#'   learnr::answer("A"),
#'   learnr::answer("B", correct = TRUE),
#'   learnr::answer("C"),
#'   learnr::answer("D"),
#'   allow_retry = TRUE,
#'   random_answer_order = TRUE
#' )
#'
#' @export
question_dropdown <- function(
  text,
  ...,
  type = "dropdown",
  correct = "Correct!",
  incorrect = "Incorrect",
  try_again = incorrect,
  allow_retry = FALSE,
  random_answer_order = FALSE,
  style = "tutorial_question"
) {
  question <-
    masteryExams:::drop_question(
      text = text,
      ...,
      type = "dropdown",
      correct = correct,
      incorrect = incorrect,
      allow_retry = allow_retry,
      random_answer_order = random_answer_order,
      style = style
    )

  answer_is_fn <- FALSE

  question
}


drop_question <- function(
    text,
    ...,
    type = c("dropdown"),
    correct = "Correct!",
    incorrect = "Incorrect",
    try_again = incorrect,
    message = NULL,
    post_message = NULL,
    loading = c("**Loading:** ", format(text), "<br/><br/><br/>"),
    submit_button = rlang::missing_arg(),
    try_again_button = rlang::missing_arg(),
    allow_retry = FALSE,
    random_answer_order = FALSE,
    style = "tutorial_question",
    options = list()
) {

  # one time tutor initialization
  initialize_tutorial()

  # capture/validate answers
  ellipsis::check_dots_unnamed() # validate all answers are not named and not a misspelling
  answers <- list(...)
  lapply(answers, function(answer) {
    checkmate::assert_class(answer, "tutorial_question_answer")
  })

  # ensure style is correct
  if (! style %in% c("tutorial_question", "notes", "exam", "notes_question")) {
    stop("style must be either 'tutorial_question', 'notes', or 'exam'")
  }

  # can not guarantee that `label` exists
  label <- knitr::opts_current$get('label')
  q_id <- label %||% learnr:::random_question_id()

  # i18nize button labels if default values are used
  submit_button <-
    if (rlang::is_missing(submit_button)) {
      learnr:::i18n_span("button.questionsubmit", "Submit Answer")
    } else {
      learnr:::quiz_text(submit_button)
    }

  try_again_button <-
    if (rlang::is_missing(try_again_button)) {
      learnr:::i18n_span("button.questiontryagain", "Try Again")
    } else {
      learnr:::quiz_text(try_again_button)
    }


  ret <- list(
    type = type,
    label = label,
    question = learnr:::quiz_text(text),
    answers = answers,
    style = style,
    button_labels = list(
      submit = submit_button,
      try_again = try_again_button
    ),
    messages = list(
      correct = learnr:::quiz_text(correct),
      try_again = learnr:::quiz_text(try_again),
      incorrect = learnr:::quiz_text(incorrect),
      message = learnr:::quiz_text(message),
      post_message = learnr:::quiz_text(post_message)
    ),
    ids = list(
      answer = NS(q_id)("answer"),
      question = q_id
    ),
    loading = learnr:::quiz_text(loading),
    random_answer_order = random_answer_order,
    allow_retry = allow_retry,
    # Set a seed for local testing, even though it is overwritten for each shiny session
    seed = learnr:::random_seed(),
    options = options
  )
  if(style == "notes"){
    class(ret) <- c(type, "notes_question")
  }else{
    class(ret) <- c(type, "tutorial_question")
  }

  ret
}


#' @export
#' @seealso question_dropdown
question_ui_initialize.dropdown <- function(question, value, ...) {

  choice_names <- learnr:::answer_labels(question, exclude_answer_fn = TRUE)
  choice_values <- learnr:::answer_values(question, exclude_answer_fn = TRUE)

  # set output to previous answers
  if (!is.null(value)) {
    ans <- unlist(value)
  } else {
    ans <- " "
  }

  selectInput(
    question$ids$answer,
    label = question$question,
    choices = c(" ", choice_values),
    #choiceNames = choice_names,
    #choiceValues = choice_values,
    selected = ans, # have previous answer selected
    width = "100%"
  )
}

#' @export
#' @seealso question_dropdown
question_ui_try_again.dropdown <- function(question, value, ...) {

  choice_values <- learnr:::answer_values(question, exclude_answer_fn = TRUE)

  learnr::disable_all_tags(
    selectInput(
      question$ids$answer,
      label = question$question,
      choices = c(" ", choice_values),
      selected = value, # have previous answer selected
      width = "100%"
    )
  )

}

#' @export
#' @seealso question_dropdown
question_is_correct.dropdown <- function(question, value, ...) {

  # if(lock_exam == TRUE){
  #    return(mark_as(TRUE, NULL))
  # }

  if(question$style == "exam"){
    return(mark_as(FALSE, NULL))
  }

  for (ans in question$answers) {
    if (as.character(ans$option) == value) {
      return(learnr::mark_as(
        ans$correct,
        ans$message
      ))
    }
  }
  learnr::mark_as(FALSE, NULL)
}


#' @export
#' @seealso question_dropdown
question_ui_completed.dropdown <- function(question, value, ...) {
  choice_values <- learnr:::answer_values(question)

  # update select answers to have X or ???
  choice_names_final <- lapply(question$answers, function(ans) {
    if (ans$correct) {
      tagClass <- "correct"
    } else {
      tagClass <- "incorrect"
    }
    tags$span(ans$label, class = tagClass)
  })

  learnr::disable_all_tags(
    learnr::finalize_question(
      selectInput(
        question$ids$answer,
        label = question$question,
        choices = choice_values,
        selected = value,
        width = "100%"
      )
    )
  )
}

