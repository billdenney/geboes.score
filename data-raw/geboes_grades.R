## code to prepare `geboes_grades` dataset goes here
library(tidyverse)
library(assertr)

geboes_levels_raw <- pdftools::pdf_text("https://europepmc.org/backend/ptpmcrender.fcgi?accid=PMC1728046&blobtype=pdf")

# The table is on page 2
cat(geboes_levels_raw[2])
geboes_grades_page_lines <-
  strsplit(geboes_levels_raw[2], split = "\n")[[1]]

# Find the first to last lines of the table
start_line <- which(startsWith(geboes_grades_page_lines, "Table 1")) + 1
end_line <- which(grepl(x = geboes_grades_page_lines, pattern = "^ +Surface epithelium")) - 1
geboes_grades_lines_raw <- geboes_grades_page_lines[start_line:end_line]

# substring to column 1, only
geboes_grades_lines_col1 <- substr(geboes_grades_lines_raw, 1, 88)
# filter to the lines with text, only
geboes_grades_lines <-
  geboes_grades_lines_col1[!grepl(x = geboes_grades_lines_col1, pattern = "^ *$")] |>
  grep(pattern = "^ *Subgrades *$", invert = TRUE, value = TRUE)

# Extract all the parts of the grades
geboes_grades_wide <-
  data.frame(text_raw = geboes_grades_lines) |>
  mutate(
    grade_raw = substr(text_raw, 1, 20),
    description = na_if(trimws(substr(text_raw, 21, 1e6)), "")
  ) |>
  select(-text_raw) |>
  mutate(
    description = gsub(x = description, pattern = "diVuse", replacement = "diffuse"),
    indentation = nchar(gsub(x = grade_raw, pattern = "[^ ].*$", replacement = "")),
    is_category = indentation == 0,
    is_subcategory = is.na(description),
    grade_category_num =
      as.integer(zoo::na.locf(case_when(
        is_category~gsub(x = grade_raw, pattern = "^Grade ([0-5]).*$", replacement = "\\1"),
        TRUE~NA_character_
      ))),
    grade_category_desc =
      zoo::na.locf(case_when(
        is_category ~ description,
        TRUE~NA_character_
      )),
    grade_subcategory_num =
      case_when(
        is_subcategory ~ substr(trimws(grade_raw), 1, 2),
        TRUE ~ NA_character_
      ),
    grade_subcategory_desc =
      case_when(
        is_subcategory ~ substr(trimws(grade_raw), 4, 1e6),
        TRUE ~ NA_character_
      ),
    grade =
      case_when(
        !is_category & !is_subcategory ~ gsub(x = trimws(grade_raw), pattern = " ", replacement = ""),
        TRUE~NA_character_
      )
  ) |>
  select(-indentation, -is_category, -is_subcategory, -grade_raw)

# Final cleanup
geboes_grades_original <-
  geboes_grades_wide |>
  group_by(grade_category_num) |>
  mutate(across(c(grade_subcategory_num, grade_subcategory_desc), .fns = \(x) zoo::na.locf(x, na.rm = FALSE))) |>
  ungroup() |>
  as.data.frame() |>
  filter(!is.na(grade)) |>
  mutate(
    grade_num = as.integer(gsub(x = grade, pattern = "^.*\\.", replacement = ""))
  ) |>
  verify(!is.na(grade_num)) |>
  select(grade_category_num, grade_category_desc, grade_subcategory_num, grade_subcategory_desc, grade, grade_num, description)

geboes_grades_original$grade <- ordered(geboes_grades_original$grade, levels = geboes_grades_original$grade)

# Add Li 2019 normal/abnormal definition
geboes_grades_li <-
  geboes_grades_original |>
  mutate(
    normal =
      case_when(
        (row_number() <= (row_number()[grade == "3.1"])) | endsWith(as.character(grade), ".0") ~ "Normal",
        TRUE ~ "Abnormal"
      )
  )

geboes_grades <- geboes_grades_li

usethis::use_data(geboes_grades, overwrite = TRUE)
