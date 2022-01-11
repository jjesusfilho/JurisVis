#' Convertion to title case with lower case for some classes of words.
#'
#' In written Portuguese, when converting to title case, it is not usual
#' to keep in title case some words, like prepositions, conjunctions,
#' articles and some kinds of pronouns.  This functions locates those
#' cases and converts them to lower case.
#'
#' @param string vector of characters to be converted to title case
#'     but with connector words (one-word prepositions and conjunctions)
#'     and articles (both definite and indefinite) and some pronouns
#'     in lower case.
#'
#' @details
#' The current list of words converted to lower case is:
#' - articles
#'   - o(s), a(s), um, uma(s), uns
#' - pronouns
#'   - me, mim, meu(s), minha(s)
#'   - te, ti, teu(s), tua(s)
#'   - lhe(s), seu(s), sua(s)
#'   - nos, nosso(a)(s)
#'   - vos, vosso(a)(s)
#' - prepositions, their contractions and combinations
#'   - prepositions
#'     - a, ante, até, após, com, contra, de, desde, em, entre, para,
#'       perante, por, sem, sob, sobre, trás
#'   - contractions
#'     - à(s), do(a)(s), no(a)(s), pelo(a)(s), pro(a)(s) (informal language)
#'   - combinations
#'     - ao(s)
#' - conjunctions
#'   - conforme, conquanto, contudo, durante, embora, enquanto, então,
#'     entretanto, exceto, logo, mas, nem, ou, ora, pois, porém, porque,
#'     porquanto, portanto, quando, quanto, que, se, senão, todavia
#'
#' The above list is far from complete or exaustive, mainly due to the absence
#' of the accidental prepositions and conjuntions, which are words that are not
#' originally prepositions or conjunctions, but can play those roles in some contexts,
#' like, for instance _segundo_, which can mean either the numeral _second_ or
#' the prepositional expression _acording to_.
#'
#' @return vector of characters with the same dimension of `string`
#' @export
#'
#'
ToTitleCasePT <- function(string) {

  string |>
    stringr::str_to_title() |>
    stringr::str_replace_all( # using `c("regex" = "replacement")` syntax
      c(
        # articles
        "(.)\\bA(s)?\\b" = "\\1a\\2",
        "(.)\\bO(s)?\\b" = "\\1o\\2",
        "(.)\\bU((m(a(s)?)?)|ns)\\b" = "\\1u\\2",
        # oblique pronouns
        "(.)\\bL(he(s)?)\\b" = "\\1l\\2",
        "(.)\\bM((e(u(s)?)?)|(i(m|(nha(s)?))))\\b" = "\\1m\\2",
        "(.)\\bN(os(s[ao](s)?)?)\\b" = "\\1n\\2",
        "(.)\\bS((e(u(s)?)?)|(ua(s)?))\\b" = "\\1s\\2",
        "(.)\\bT((e(u(s)?)?)|i|(ua(s)?))\\b" = "\\1t\\2",
        "(.)\\bV(os(s[ao](s)?)?)\\b" = "\\1v\\2",
        # prepositions
        "(.)\\bA((o)(s)?|nte|t\u00e9|p\u00f3s)\\b" = "\\1a\\2",
        "(.)\\b\u00c0(s)?\\b" = "\\1\u00e0\\2",
        "(.)\\bC(om|ontra)\\b" = "\\1c\\2",
        "(.)\\bD(((a|o)(s)?)|(e(sde)?))\\b" = "\\1d\\2",
        "(.)\\bE(m|ntre)\\b" =  "\\1e\\2",
        "(.)\\bN((a|o)(s)?)\\b" = "\\1n\\2",
        "(.)\\bP(ara|(e((l(a|o)(s)?)|rante))|or)\\b" = "\\1p\\2",
        "(.)\\bS(em|(ob(re)?))\\b" = "\\1s\\2",
        "(.)\\bT(r\u00e1s)\\b" = "\\1t\\2",
        # conjunctions
        "(.)\\bC(on(forme|quanto|tudo))\\b" = "\\1c\\2",
        "(.)\\bD(urante)\\b" = "\\1D\\2",
        "(.)\\bE((mbora|n(quanto|t(\u00e3o|retanto))|xceto)?)\\b" = "\\1e\\2",
        "(.)\\bL(ogo)\\b" = "\\1l\\2",
        "(.)\\bM(as)\\b" = "\\1m\\2",
        "(.)\\bN(em)\\b" = "\\1n\\2",
        "(.)\\bO(u|ra)\\b" = "\\1o\\2",
        "(.)\\bP(o(is|r(\u00e9m|qu(e|anto)|tanto)))\\b" = "\\1p\\2",
        "(.)\\bQ(u(an[dt]o|e))\\b" = "\\1q\\2",
        "(.)\\bS(e(n\u00e3o)?)\\b" = "\\1s\\2",
        "(.)\\bT(odavia)\\b" = "\\1t\\2"
      )
    )
}

label_both_upper <- function(labels, multi_line = TRUE, sep = ': ') {
  names(labels) <- ToTitleCasePT(names(labels))
  ggplot2::label_both(labels, multi_line, sep)
}

jus_theme <- function(...) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = 'bold', size = 9),
      plot.margin = ggplot2::margin(15, 15, 15, 15),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = 'bottom',
      panel.spacing.y = ggplot2::unit(15, 'pt'),
      strip.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2, b = 2), hjust = 0),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      ...
    )
}
