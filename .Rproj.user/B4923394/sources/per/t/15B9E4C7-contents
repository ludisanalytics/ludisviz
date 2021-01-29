#' Format dataframe
#'
#' formatDf returns a dataframe formatted to visualize aggregated column stats
#'
#' This function returns a formatted dataframe for visualizing data when the
#' column stats are aggregated together (eg. summed, used to calculate a ratio, etc.).
#' @param dt A dataframe of stats, eg. a Sportscode matrix output.
#' @param match (optional) A string. Indicates the game of interest.
#' @param rowStat (optional) A string. Indicates the specific row of interest.
#' @param statTeam (optional*) A string. Indicates the team for which the user wants to pull the stats.
#' - *if the user does not pass in a rowStat, they must pass in a statTeam
#' @param nStats A vector of strings. Indicate the columns used to calculate the aggregated value, or the numerator of a ratio when dStats are also passed in.
#' @param dStats (optional) A vector of strings. Indicate the columns used to calculate the denominator of a ratio.
#' @param statName A string. Indicates the formatted stat name for the plot.
#' @param xStat (optional) A string. Adds a stat column to the dataframe; for plots that use the stat variable along the x axis.
#' @param valType (optional) A string. Indicates how to calculate the stat value and set the stat label for the plot
#' @return If all parameters are valid for the input dataframe, then the output will be a formatted dataframe.
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
formatDf <- function(dt, match, rowStat, statTeam, nStats, dStats, statName, xStat, valType){
  if(missing(match)){
    temp_dt <- dt
  } else {
    temp_dt <- dt %>% dplyr::filter(.data$game == match)
  }
  if(missing(dStats) & missing(valType)){
    if(missing(rowStat)){
      df <- temp_dt %>% dplyr::group_by(.data$game) %>%
        dplyr::summarise_at(nStats, sum) %>%
        dplyr::mutate(val = rowSums(dplyr::select(., -.data$game))) %>%
        dplyr::select(.data$game, .data$val)
    } else {
      df <- temp_dt %>% dplyr::filter(stringr::str_ends(.data$Stat, rowStat)) %>%
        dplyr::group_by(.data$game, .data$Stat) %>%
        dplyr::summarise_at(nStats, sum) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(team = stringr::word(.data$Stat, 1, sep = "\\."),
                      val = rowSums(dplyr::select(., !c(.data$game, .data$Stat)))) %>%
        dplyr::select(.data$game, .data$team, .data$val)
    }
    df$lab <- ""
  } else {
    if(missing(rowStat)){
      n_temp <- temp_dt %>% dplyr::group_by(.data$game) %>%
        dplyr::summarise_at(nStats, sum) %>%
        dplyr::mutate(num = rowSums(dplyr::select(., -.data$game))) %>%
        dplyr::select(.data$game, .data$num)
      d_temp <- temp_dt %>% dplyr::group_by(.data$game) %>%
        dplyr::summarise_at(dStats, sum) %>%
        dplyr::mutate(dem = rowSums(dplyr::select(., -.data$game))) %>%
        dplyr::select(.data$dem)
    } else {
      n_temp <- temp_dt %>% dplyr::filter(stringr::str_ends(.data$Stat, rowStat)) %>%
        dplyr::group_by(.data$game, .data$Stat) %>%
        dplyr::summarise_at(nStats, sum) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(team = stringr::word(.data$Stat, 1, sep = "\\."),
                      num = rowSums(dplyr::select(., !c(.data$game, .data$Stat)))) %>%
        dplyr::select(.data$game, .data$team, .data$num)
      d_temp <- temp_dt %>% dplyr::filter(stringr::str_ends(.data$Stat, rowStat)) %>%
        dplyr::group_by(.data$game, .data$Stat) %>%
        dplyr::summarise_at(dStats, sum) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(dem = rowSums(dplyr::select(., !c(.data$game, .data$Stat)))) %>%
        dplyr::select(.data$dem)
    }
    df <- cbind(n_temp, d_temp)
    if(valType == "fraction"){
      df$val <- df$num
      df$lab <- paste0("/", df$dem, " (", round((df$num/df$dem)*100), "%)")
    } else if(valType == "percent"){
      df$val <- round((df$num/df$dem)*100)
      df$lab <- paste0("% (", df$num, "/", df$dem, ")")
    }
    if(!missing(statTeam)){
      df$team <- statTeam
    }
  }
  df$type <- statName
  if(!missing(xStat)){
    df$stat <- xStat
  }
  return(df)
}

#' Format dataframe, by column
#'
#' formatDfByCol returns a dataframe formatted to visualize distinct column stats
#'
#' This function returns a formatted dataframe for visualizing data when the
#' column stats for analysis each need to be visualized individually.
#' @param dt A dataframe of stats, eg. a Sportscode matrix output.
#' @param match (optional) A string. Indicates the game of interest.
#' @param trendTeam (optional) A string. Indicates the team for which the user wants to pull all existing game stats.
#' @param rowStat A string. Indicates the row of interest.
#' @param colStats A vector of strings. Indicates the columns of interest.
#' @param statNames A vector of strings. Indicates the formatted version of the colStat names for the plot.
#' @inherit formatDf return
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
formatDfByCol <- function(dt, match, trendTeam, rowStat, colStats, statNames){
  if(missing(trendTeam)) {
    temp_dt <- dt %>% dplyr::filter(.data$game == match) %>% dplyr::filter(stringr::str_ends(.data$Stat, rowStat)) %>%
      dplyr::mutate(name = stringr::word(.data$Stat, 1, sep = "\\.")) %>%
      dplyr::select(.data$game, .data$name, colStats)
    df <- c()
    for(i in unique(temp_dt$name)){
      temp <- temp_dt %>% dplyr::filter(.data$name == i)
      temp_df <- data.frame(
        team = rep(temp$name, length(colStats)),
        val = t(temp %>% dplyr::select(colStats))
      )
      temp_df$perc <- round((temp_df$val/sum(temp_df$val))*100, 0)
      temp_df$lab <- paste0("/", sum(temp_df$val), " (", temp_df$perc, "%)")
      temp_df$type <- statNames
      df <- rbind(df, temp_df)
    }
  } else if(missing(match)){
    temp_dt <- dt %>% dplyr::filter(stringr::str_starts(.data$Stat, trendTeam)) %>% dplyr::filter(stringr::str_ends(.data$Stat, rowStat)) %>%
      dplyr::mutate(name = stringr::word(.data$Stat, 1, sep = "\\.")) %>%
      dplyr::select(.data$game, .data$name, colStats)
    df <- c()
    for(i in unique(temp_dt$game)){
      temp <- temp_dt %>% dplyr::filter(.data$game == i)
      temp_df <- data.frame(
        game = rep(temp$game, length(colStats)),
        val = t(temp %>% dplyr::select(colStats))
      )
      temp_df$perc <- round((temp_df$val/sum(temp_df$val))*100, 0)
      temp_df$lab <- paste0("/", sum(temp_df$val), " (", temp_df$perc, "%)")
      temp_df$type <- statNames
      df <- rbind(df, temp_df)
    }
  }
  return(df)
}

#' Format dataframe, complex ratios
#'
#' formatDfComplexRatio returns a dataframe formatted to visualize multi-row stat ratios
#'
#' This function returns a formatted dataframe for visualizing data when the
#' ratio being calculated is calculated from different row stats.
#' @param dt A dataframe of stats, eg. a Sportscode matrix output.
#' @param match (optional) A string. Indicates the game of interest.
#' @param trendTeam (optional) A string. Indicates the team for which the user wants to pull all existing game stats.
#' @param nrowStat A string. Indicates the row of interest for the numerator stats.
#' @param ncolStats A vector of strings. Indicates the columns used to calculate the numerator of the ratio.
#' @param drowStat A string. Indicates the row of interest for the denominator stats.
#' @param dcolStats A vector of strings. Indicates the columns used to calculate the denominator of the ratio.
#' @param statNames A vector of strings. Indicates the formatted numerator/denominator stat names for the plot.
#' @param ratioLab A string. Indicates the name of calculated ratio for the plot.
#' @inherit formatDf return
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
formatDfComplexRatio <- function(dt, match, trendTeam, nrowStat, ncolStats, drowStat, dcolStats, statNames, ratioLab){
  if(missing(trendTeam)) {
    temp_dt <- dt %>% dplyr::filter(.data$game == match)
  } else if(missing(match)){
    temp_dt <- dt %>% dplyr::filter(stringr::str_starts(.data$Stat, trendTeam))
  }
  n_temp <- temp_dt %>% dplyr::filter(stringr::str_ends(.data$Stat, nrowStat)) %>%
    dplyr::mutate(team = stringr::word(.data$Stat, 1, sep = "\\."),
                  num = rowSums(dplyr::select(., tidyselect::all_of(ncolStats)))) %>%
    dplyr::select(.data$game, .data$team, .data$num)
  d_temp <- temp_dt %>% dplyr::filter(stringr::str_ends(.data$Stat, drowStat)) %>%
    dplyr::mutate(team = stringr::word(.data$Stat, 1, sep = "\\."),
                  dem = rowSums(dplyr::select(., tidyselect::all_of(dcolStats)))) %>%
    dplyr::select(.data$game, .data$team, .data$dem)
  df <- n_temp %>% dplyr::left_join(d_temp, by = c("game", "team"))
  df$val <- round(df$num/df$dem, 1)
  df$lab <- paste0("\n", statNames[1], ": ", df$num, ", ", statNames[2], ": ", df$dem)
  df$type <- ratioLab
  return(df)
}

#' Format dataframe, by player
#'
#' formatDfByPlayer returns a dataframe formatted to visualize stats across players
#'
#' This function returns a formatted dataframe for visualizing data when
#' comparing stats across players
#' @param dt A dataframe of stats, eg. a Sportscode matrix output.
#' @param match (optional) A string. Indicates the game of interest.
#' @param nStats A vector of strings. Indicate the columns used to calculate the aggregated value, or the numerator of a ratio when dStats are also passed in.
#' @param dStats (optional) A vector of strings. Indicate the columns used to calculate the denominator of a ratio.
#' @param statName A string. Indicates the formatted stat name for the plot.
#' @param xStat (optional) A string. Adds a stat column to the dataframe; for plots that use the stat variable along the x axis.
#' @param valType (optional) A string. Indicates how to calculate the stat value and set the stat label for the plot
#' @param players A vector of strings. Specifies the names of the players for whom to pull stats.
#' @inherit formatDf return
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
formatDfByPlayer <- function(dt, match, nStats, dStats, statName, xStat, valType, players){
  temp_dt <- dt %>% dplyr::filter(.data$Stat %in% players)
  if(!missing(match)){
    temp_dt <- temp_dt %>% dplyr::filter(.data$game == match)
  }
  if(missing(dStats) & missing(valType)){
    df <- temp_dt %>% dplyr::group_by(.data$Stat, .data$game) %>%
      dplyr::summarise_at(nStats, sum) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(val = rowSums(dplyr::select(., !c(.data$game, .data$Stat)))) %>%
      dplyr::select(.data$Stat, .data$game, .data$val)
    df$lab <- ""
  } else {
    n_temp <- temp_dt %>% dplyr::group_by(.data$Stat, .data$game) %>%
      dplyr::summarise_at(nStats, sum) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(num = rowSums(dplyr::select(., !c(.data$game, .data$Stat)))) %>%
      dplyr::select(.data$Stat, .data$game, .data$num)
    d_temp <- temp_dt %>% dplyr::group_by(.data$Stat, .data$game) %>%
      dplyr::summarise_at(dStats, sum) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dem = rowSums(dplyr::select(., !c(.data$game, .data$Stat)))) %>%
      dplyr::select(.data$dem)
    df <- cbind(n_temp, d_temp)
    if(valType == "fraction"){
      df$val <- df$num
      df$lab <- paste0("/", df$dem, " (", round((df$num/df$dem)*100), "%)")
    } else if(valType == "percent"){
      df$val <- round((df$num/df$dem)*100)
      df$lab <- paste0("% (", df$num, "/", df$dem, ")")
    }
  }
  df$type <- statName
  if(!missing(xStat)){
    df$stat <- xStat
  }
  return(df)
}

