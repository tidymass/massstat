library(massdataset)

expression_data <-
  as.data.frame(matrix(1:40, nrow = 4, ncol = 10))

colnames(expression_data) <-
  paste0("sample", 1:10)

rownames(expression_data) <-
  paste0("variable", 1:4)

sample_info <-
  data.frame(
    sample_id = colnames(expression_data),
    injection.order = 1:10,
    class = "QC",
    group = c(rep("control", 5), rep("case", 5))
  )

variable_info <-
  data.frame(
    variable_id = rownames(expression_data),
    mz = 1:4,
    rt = 2:5
  )

object <-
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )

test_that(desc = "mutate_fc",
          code = {
            ###expression_data
            object1 <-
              mutate_fc(
                object = object,
                control_sample_id = pull(
                  filter(
                    activate_mass_dataset(object, "sample_info"),
                    group == "control"
                  ),
                  "sample_id"
                ),
                case_sample_id =  pull(filter(
                  activate_mass_dataset(object, "sample_info"),
                  group == "case"
                ), "sample_id")
              )
            result1 <-
              check_mass_dataset(
                expression_data = object1@expression_data,
                sample_info = object1@sample_info,
                variable_info = object1@variable_info,
                sample_info_note = object1@sample_info_note,
                variable_info_note = object1@variable_info_note
              )
            testthat::expect_equal(object = result1, "all good.")
          })
