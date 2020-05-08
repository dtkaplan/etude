```{r BLOCK_NAME, echo=FALSE}
question(
  text = "The prompt for the question",
  answer("First choice", correct = TRUE, message = "The first one is *always* correct."),
  answer("Second choice", message = "The second choice is *never* correct."),
  answer("Third choice", correct = TRUE, message = "More than one answer can be true."),
  allow_retry = TRUE,
  random_answer_order = TRUE)
```
