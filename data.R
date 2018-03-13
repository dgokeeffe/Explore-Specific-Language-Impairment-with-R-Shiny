df <- read_csv("/home/kazooy/SLI/data/all_data_R_high_cor_removed.csv", col_names = T, trim_ws = T)

# Rearrange the columns so my factors are at the front
df %>%
  select(group, corpus, sex, filename, everything()) %>% 
  mutate(age = age/12) -> df

# Find outliers using MAD ----
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

# Create outliers tibble ----
df %>%
  # Compute per group values of columns
  group_by(group) %>%
  mutate_if(is.numeric, isnt_out_mad) -> outliers

# Join outliers ----
df %>% 
  left_join(outliers, by = "filename", suffix=c('.x','.out')) -> df

# Map features to column names 
axis_vars <- c("Age" = "age.x",
    "Total Number of Words"="child_TNW.x",
    "Total Number of Sentences"="child_TNS.x",
    "Examiner Total Number of Words"="examiner_TNW.x" ,
    "Frequency of Word Types to Word Token Ratio"="freq_ttr.x",
    "Ratio of raw to inflected verbs"="r_2_i_verbs.x",
    "Number of Repetitions"="repetition.x",
    "Number of Retracings"="retracing.x",
    "Number of Fillers"="fillers.x",
    "Perplexity of 1-gram SLI"="s_1g_ppl.x",
    "Perplexity of 2-gram SLI"="s_2g_ppl.x",
    "Perplexity of 3-gram SLI"="s_3g_ppl.x",
    "Perplexity of 1-gram TD"="d_1g_ppl.x",
    "Perplexity of 2-gram TD"="d_2g_ppl.x",
    "Average number of Syllables per word"="average_syl.x",
    "Mean Length of Utterance of Words"="mlu_words.x",
    "Mean Length of Utterance in the 1st 100 Words"="mlu100_utts.x",
    "Number of verb utterances"="verb_utt.x",
    "Developmental Sentence Score"="dss.x",
    "Index of Productive Syntax Score"="ipsyn_total.x",
    "Number of Word Errors"="word_errors.x",
    "Flesch-Kincaid Score"="f_k.x",
    "Number of Nouns followed immediately by a verb"="n_v.x",
    "Number of Nouns followed immediately by an Auxillary verb"="n_aux.x",
    "Number of Third Singular Nouns followed immediately by a verb"="n_3s_v.x",
    "Number of Determinant Nouns followed by a Personal Pronoun"="det_n_pl.x",
    "Number of Determinant Pronouns followed by a Noun"="det_pl_n.x",
    "Pronouns followed by Auxillary Verb"="pro_aux.x",
    "3rd. singular nominative pronoun followed by Verb"="pro_3s_v.x",
    "Total number of morphosyntactic errors"="total_error.x",
    "Brown's Stage 1"="present_progressive.x",
    "Brown's Stage 2"="propositions_in.x",
    "Brown's Stage 3"="propositions_on.x",
    "Brown's Stage 4"="plural_s.x",
    "Brown's Stage 5"="irregular_past_tense.x",
    "Brown's Stage 6"="possessive_s.x",
    "Brown's Stage 7"="uncontractible_copula.x",
    "Brown's Stage 8"="articles.x",
    "Brown's Stage 9"="regular_past_ed.x",
    "Brown's Stage 10"="regular_3rd_person_s.x",
    "Brown's Stage 11"="irregular_3rd_person.x",
    "Brown's Stage 12"="uncontractible_aux.x",
    "Brown's Stage 13"="contractible_copula.x",
    "Brown's Stage 14"="contractible_aux.x"
) 


