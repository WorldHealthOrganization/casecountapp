#' Specify default views for a display
#'
#' @param ref_source TODO
#' @param comp_sources TODO
#' @param death_cutoff TODO
#' @param entity_pl TODO
#' @export
default_views <- function(
  ref_source,
  comp_sources = NULL,
  death_cutoff = 100,
  entity_pl = "entities"
) {
  lref_source <- tolower(ref_source)
  lcomp_sources <- tolower(comp_sources)

  views <- list(
    list(
      name = "Ordered by total cases",
      state = paste0("&nrow=2&ncol=3&arr=row&pg=1&labels=cur_case_",
        lref_source, "&sort=cur_case_", lref_source, ";desc&filter=&fv=")
    ),
    list(
      name = "Ordered by total deaths",
      state = paste0("&nrow=2&ncol=3&arr=row&pg=1&labels=cur_death_",
        lref_source, "&sort=cur_death_", lref_source, ";desc&filter=&fv=")
    ),
    list(
      name = "Ordered by weekly percent change in cases (high to low)",
      state = "&nrow=2&ncol=3&arr=row&pg=1&labels=new_wk_case_change_pct&sort=new_wk_case_change_pct;desc&filter="
    ),
    list(
      name = "Ordered by weekly percent change in deaths (high to low)",
      state = "&nrow=2&ncol=3&arr=row&pg=1&labels=new_wk_death_change_pct&sort=new_wk_death_change_pct;desc&filter="
    ),
    list(
      name = paste0("Ordered by case fatality rate for ",
        entity_pl, " with at least 100 deaths"),
      state = paste0("&nrow=2&ncol=3&arr=row&pg=1&labels=case_fatality_pct,cur_death_", 
        lref_source, "&sort=case_fatality_pct;desc&filter=var:cur_death_",
        lref_source, ";type:range;from:", death_cutoff, ";to:&fv=cur_death_",
        lref_source)
    ),
    list(
      name = "Ordered by attack rate (cases per 100,000 population)",
      state = "&nrow=2&ncol=3&arr=row&pg=1&labels=attack_rate,population&sort=attack_rate;desc&filter=&fv="
    ),
    list(
      name = "Ordered by new cases in past 2 weeks per 100,000 population",
      state = "&nrow=2&ncol=3&arr=row&pg=1&labels=new_2wk_case_per_100k,population&sort=new_2wk_case_per_100k;desc&filter=&fv="
    ),
    list(
      name = "Ordered by days since first case (most recent first)",
      state = "&nrow=2&ncol=3&arr=row&pg=1&labels=days_since_first_case&sort=days_since_first_case;asc&filter="
    )
  )

  for (src in comp_sources) {
    views[[length(views) + 1]] <- list(
      name = paste0("Ordered by abs diff between ", src," and ",
        ref_source, " cases"),
      state = paste0("&nrow=2&ncol=3&arr=row&pg=1&labels=case_abs_diff_",
        tolower(src), "&sort=case_abs_diff_", tolower(src), ";desc&filter=&fv=")
    )
    views[[length(views) + 1]] <- list(
      name = paste0("Ordered by abs diff between ", src," and ",
        ref_source, " deaths"),
      state = paste0("&nrow=2&ncol=3&arr=row&pg=1&labels=death_abs_diff_",
        tolower(src), "&sort=death_abs_diff_", tolower(src),
        ";desc&filter=&fv=")
    )
  }

  views
}