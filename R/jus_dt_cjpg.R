#' Creates a DT datatable for cjpg
#'
#' @param df dataframe ou tibble
#'
#' @return datatable
#' @export
#'
jus_dt_cjpg <-  function(df){

  DT::datatable(df,
                extensions = 'Responsive',
                rownames=FALSE,
                filter="top",
                escape= FALSE,
                selection = 'none',
                options=list(lengthChange=FALSE,
                             dom = 'Bfrtip',
                             buttons = c("txt"),
                             responsive=TRUE,
                             buttons=TRUE,
                             language=list(search="Busca",
                                           info="Mostrando de _START_ a _END_ de _TOTAL_ decis\u00f5es",
                                           lengthMenu="Mostre _MENU_ decis\u00f5es",
                                           paginate=list(previous="anterior",
                                                         'next'="pr\u00f3ximo")),
                             columnDefs = list(list(className = 'dt-justify',
                                                    targets = "julgado")))
  )

}
