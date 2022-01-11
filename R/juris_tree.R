#' Cria árvore de probabilidades numa distribuição binomial
#'
#' @param p Se você informar uma única probabilidade,
#'     a função admitirá três tentativas de Bernoulli
#'     identicas e independentes, ou seja, uma distribuição
#'     binomial.
#' @param probs Lista de probabilidades. Caso queira informar
#'     probabilidades distintas para cada um dos eventos.
#'
#' @description Esta função cria uma visualização de árvore de
#'     probabilidades. Ela permite que você mantenha a mesma
#'     probabilidade em cada evento, e.g., qual a probabilidade de
#'     de eu ganhar duas ações de três distribuídas.  Ou calcular
#'     a probabilidade de ganhar uma mesma ação em todas as instâncias,
#'      ex. 1ª, 2ª e superior (STJ, STF), sabendo que para cada
#'      uma dessas instâncias a probabilidade é distinta.
#'
#' @return Árvore de probabilidade
#' @export
#'
juris_tree <- function(p= NULL,probs = list(.63,c(.56,.78),c(.89,.43,.76,.25))){

  ## Eventualmente, usarei este c\u00f3digo.

  # nos <- purrr::reduce(list(2,c(1:length(probs))),`^`) %>% sum(1,.,tail(.,1))
  #
  # if (length(nos)>26){
  #
  #   n <- nos - 26
  #
  #  labs <- paste0("a",letters[1:n])
  #
  # } else{
  #
  # labs <- letters[1:eixos]
  #
  #  }

  if (!is.null(p)){

    probs <- list(p,c(p,p),c(p,p,p,p))

  }


  labs <- letters[1:23]

  tree <-
    DiagrammeR::create_graph() %>%
    DiagrammeR::add_n_nodes(
      n = 23 ,
      type = "path",
      label = labs,
      node_aes = DiagrammeR::node_aes(
        shape = "circle",
        height = 1,
        width = 1,
        x = c(0, 3, 3, 6, 6, 6, 6, 9, 9, 9, 9,9,9,9,9,12,12,12,12,12,12,12,12),
        y = c(0, 4, -4, 5, 2, -2, -5, 6, 4, 3, 1,-1,-3,-4,-6, 6, 4, 3, 1,-1,-3,-4,-6))) %>%
    DiagrammeR::add_edge(
      from = 1,
      to = 2,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Favor\u00e1vel: ",probs[[1]]),
        fontsize = 15
      )
    ) %>%
    DiagrammeR::add_edge(
      from = 1,
      to = 3,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Desfavor\u00e1vel: ", 1-probs[[1]]),
        fontsize = 15

      )
    ) %>%
    DiagrammeR::add_edge(
      from = 2,
      to = 4,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Favor\u00e1vel: ",probs[[2]][1]),
        fontsize = 15

      )
    ) %>%
    DiagrammeR::add_edge(
      from = 2,
      to = 5,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Desfavor\u00e1vel: ",1-probs[[2]][1]),
        fontsize = 15

      )
    ) %>%
    DiagrammeR::add_edge(
      from = 3,
      to = 6,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Favor\u00e1vel: ",probs[[2]][2]),
        fontsize = 15

      )
    ) %>%
    DiagrammeR::add_edge(
      from = 3,
      to = 7,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Desfavor\u00e1vel: ",1 - probs[[2]][2]),
        fontsize = 15

      )
    ) %>%
    DiagrammeR::add_edge(
      from = 4,
      to = 8,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Favor\u00e1vel: ",probs[[3]][1]),
        fontsize = 15

      )
    ) %>%
    DiagrammeR::add_edge(
      from = 4,
      to = 9,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Desfavor\u00e1vel: ",1- probs[[3]][1]),
        fontsize = 15

      )
    ) %>%
    DiagrammeR::add_edge(
      from = 5,
      to = 10,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Favor\u00e1vel: ",probs[[3]][2]),
        fontsize = 15

      )
    ) %>%
    DiagrammeR::add_edge(
      from = 5,
      to = 11,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Desfavor\u00e1vel: ",1-probs[[3]][2]),
        fontsize = 15

      )
    )%>%
    DiagrammeR::add_edge(
      from = 6,
      to = 12,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Favor\u00e1vel: ",probs[[3]][3]),
        fontsize = 15

      )
    )%>%
    DiagrammeR::add_edge(
      from = 6,
      to = 13,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Desfavor\u00e1vel: ",1-probs[[3]][3]),
        fontsize = 15

      )
    )%>%
    DiagrammeR::add_edge(
      from = 7,
      to = 14,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Favor\u00e1vel: ",probs[[3]][4]),
        fontsize = 15

      )
    )%>%
    DiagrammeR::add_edge(
      from = 7,
      to = 15,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("Desfavor\u00e1vel: ",1-probs[[3]][4]),
        fontsize = 15

      )
    )%>%
    DiagrammeR::add_edge(
      from = 8,
      to = 16,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("FFF= ",probs[[1]]*probs[[2]][1]*probs[[3]][1]),
        fontsize = 15
      )
    )%>%
    DiagrammeR::add_edge(
      from = 9,
      to = 17,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("FFD= ",probs[[1]]*probs[[2]][1]*(1-probs[[3]][1])),
        fontsize = 15
      )
    )%>%
    DiagrammeR::add_edge(
      from = 10,
      to = 18,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("FDF= ",probs[[1]]*(1-probs[[2]][1])*probs[[3]][2]),
        fontsize = 15
      )
    )%>%
    DiagrammeR::add_edge(
      from = 11,
      to = 19,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("FDD= ",probs[[1]]*(1-probs[[2]][1])*(1-probs[[3]][2])),
        fontsize = 15
      )
    )%>%
    DiagrammeR::add_edge(
      from = 12,
      to = 20,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("DFF= ",(1-probs[[1]])*(probs[[2]][2])*(probs[[3]][3])),
        fontsize = 15
      )

    )%>%
    DiagrammeR::add_edge(
      from = 13,
      to = 21,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("DFD= ",(1-probs[[1]])*(probs[[2]][2])*(1-probs[[3]][3])),
        fontsize = 15
      )

    )%>%
    DiagrammeR::add_edge(
      from = 14,
      to = 22,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("DDF= ",(1-probs[[1]])*(1-probs[[2]][2])*(probs[[3]][4])),
        fontsize = 15
      )

    )%>%
    DiagrammeR::add_edge(
      from = 15,
      to = 23,
      edge_aes = DiagrammeR::edge_aes(
        label = paste0("DDD= ",(1-probs[[1]])*(1- probs[[2]][2])*(1- probs[[3]][3])),
        fontsize = 15
      )

    )
  DiagrammeR::render_graph(tree)
}
