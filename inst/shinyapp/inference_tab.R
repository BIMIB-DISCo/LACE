### inference_tab.R

### Inference reactiveVals ####

inf_uis = c("inf_num_iter",
            "inf_num_rs",
            "inf_n_try_bs",
            "inf_num_processes",
            "inf_seed",
            "inf_random_tree",
            "inf_marginalize",
            "inf_keep_equivalent",
            "inf_check_indistinguishable",
            "inf_show",
            "inf_learning_rate",
            "inf_error_move")


# inf_rvs = reactiveValues(inf_buttons = list(),
#                          inf_observers = list())


for (inf_ui in inf_uis)
  inputs[[inf_ui]] <- reactiveVal()

inputs[['inf_alpha']] <- reactiveVal()
inputs[['inf_beta']] <- reactiveVal()
inputs[['inf_params']] <- reactiveVal()


if (is.null(session$userData[["ed_table_react_list"]])) {
  session$userData[["ed_table_react_list"]] <- reactiveValues()
}

if (is.null(session$userData[["ed_table_react_list_obj"]])) {
  session$userData[["ed_table_react_list_obj"]] <- list()
  session$userData[["ed_table_react_list_obj"]]$obj = reactiveValues()
  session$userData[["ed_table_react_list_obj"]]$observers = list()
}


## Create empty table
if (!is.null(inputs[['m_time_column']])) {
  empty_table <-
    data.frame(matrix(0,
                      ncol = length(inputs[['m_time_column']]()),
                      nrow = 1))
  colnames(empty_table) <-
    c(inputs[['m_time_column']]())
} else {
  empty_table <- data.frame(matrix(0, ncol = 1, nrow = 1))
  colnames(empty_table) <- c('T1')
}

## print(empty_table)
ed_tables <- list()

## Create false positive and false negative UI table set to empty table
ed_tables[['inf_alpha']] <-
  ed_table('inf_alpha', empty_table, 'alpha_', session)

ed_tables[['inf_beta']] <-
  ed_table('inf_beta', empty_table, 'beta_', session)


empty_cell <- data.frame(row=-1, col=-1 , value=0)
inputs[[paste0('inf_alpha','_cell_edit')]] <- reactiveVal(empty_cell)
inputs[[paste0('inf_beta','_cell_edit')]] <- reactiveVal(empty_cell)

### End inference reactiveVals ####

### Inference functions ####

hide_tab <- function() {
  shinyjs::show(id="computation_idCol_div")
  hideTab(inputId = "main_tabset", target = "Project")
  hideTab(inputId = "main_tabset", target = "SC metadata")
  hideTab(inputId = "main_tabset", target = "Annotations")
  hideTab(inputId = "main_tabset", target = "Filters")
  hideTab(inputId = "main_tabset", target = "SC sampling depths")
  hideTab(inputId = "main_tabset", target = "Variants")
  hideTab(inputId = "main_tabset", target = "Inference")
  hideTab(inputId = "main_tabset", target = "Longitudinal display")
}

show_tab <- function(disp = F) {
  #browser()
  shinyjs::hide(id="computation_idCol_div")
  showTab(inputId = "main_tabset", target = "Project")
  showTab(inputId = "main_tabset", target = "SC metadata")
  showTab(inputId = "main_tabset", target = "Annotations")
  showTab(inputId = "main_tabset", target = "Filters")
  showTab(inputId = "main_tabset", target = "SC sampling depths")
  showTab(inputId = "main_tabset", target = "Variants")
  showTab(inputId = "main_tabset", target = "Inference")
  showTab(inputId = "main_tabset", target = "Longitudinal display")
  if (disp)
    #delay(100,
          updateTabsetPanel(session, "main_tabset", selected = "Longitudinal display")
    #)

}


inf_exec <- function() {
  if (length(va_out_dir_()) == 0)
    return()
  if (dir.exists(va_out_dir_()))
    if (file.exists(file.path(va_out_dir_(), "D.RData"))) {
      load(file.path(va_out_dir_(),"D.RData"))


      make_numbers <- function(tab) {
        tab_tmp <- as.data.frame(lapply(tab, FUN = as.numeric), check.names = F)
        rownames(tab_tmp) <- rownames(tab)
        return(tab_tmp)
      }

      remove_repeated_rownames <- function(tab) {
        extra_col <- all(tab[[1]] == rownames(tab))
        if (is.na(extra_col))
          extra_col <- FALSE
        if (extra_col) {
          tab <- tab[ , -1, drop = F]
          remove_repeated_rownames(tab)
        }
        return(tab)
      }


      remove_na_row <- function(tab, row_min=2, left_rows=1) {

        row_basename <- rownames(tab)[1]
        if (str_length(row_basename)>0) {
          row_basename <- paste0(str_split_fixed(row_basename, pattern = "_", n = 2)[1], "_")
        }
        else
          row_basename <- "row_"

        keep_row <- c()

        if ( left_rows>0){
          if (left_rows>nrow(tab))
            left_rows <- nrow(tab)-1
          keep_row <- seq((nrow(tab)- left_rows):nrow(tab))
          print("keep_row")
          print(keep_row)
        }
        if ((nrow(tab) - left_rows)>0)
          range <-seq(1:(nrow(tab) - left_rows))
        else
          range <- NULL
        print("range")
        print(range)
        for (i in range) {
          canc <-0
          for (j in seq(1, ncol(tab))) {
            if (is.na(tab[i,j])) {
              canc <- canc+1
            }
            print(canc)
          }
          if(canc < ncol(tab))
            keep_row <- c(keep_row, i)
          else
          {
            for (j in seq(1, ncol(tab))) {
              if (is.na(tab[i,j]))
                tab[i,is.na(tab[i,])] <- 0.0
            }
          }
        }

        print(keep_row)

        tab_tmp <- tab[keep_row, ,drop = F]
        if (nrow(tab_tmp)<=row_min)
          tab_tmp <- tab

        return(tab_tmp)
      }

      put_valid_rates <- function(tab) {

        r_name <- rownames(tab)
        tab_tmp <- as.data.frame(lapply(tab, FUN = abs), check.names = F)
        tab_tmp[tab_tmp>=1.0] <- 0.999
        tab_tmp[is.na(tab_tmp)] <- 0.001
        rownames(tab_tmp) <- r_name

        return(tab_tmp)
      }

      fix_table <- function(tab) {

        tab <- remove_repeated_rownames(tab)
        tab <- make_numbers(tab)
        tab <- remove_na_row(tab, 0, 0)
        tab <- put_valid_rates(tab)

        return (tab)
      }


      #browser()

      alpha <- session$userData[["ed_table_react_list"]][["inf_alpha"]]$x$data
      alpha <- fix_table(alpha)
      r_names <- rownames(alpha)
      alpha <- transpose(as.data.table(alpha))
      colnames(alpha) <- r_names

      beta <- session$userData[["ed_table_react_list"]][["inf_beta"]]$x$data
      beta <- fix_table(beta)
      r_names <- rownames(beta)
      beta <- transpose(as.data.table(beta))
      colnames(beta) <- r_names



      ## Notes:
      ## MA: this is most likely best represented as a R
      ## 'formula'.

      string_tmp <-
        "LACE('D' = D,
                          'learning_rate' = input[['inf_learning_rate']],
                          'num_iter' = input[['inf_num_iter']],
                          'num_rs' = input[['inf_num_rs']],
                          'n_try_bs' = input[['inf_n_try_bs']],
                          'num_processes' = input[['inf_num_processes']],
                          'seed' = input[['inf_seed']],
                          'random_tree' = input[['inf_random_tree']],
                          'marginalize' = input[['inf_marginalize']],
                          'keep_equivalent' = input[['inf_keep_equivalent']],
                          'check_indistinguishable' = input[['inf_check_indistinguishable']],
                          'error_move' = input[['inf_error_move']],
                          'show' = input[['inf_show']],
                          'alpha' = as.list(alpha),
                          'beta' = as.list(beta)
                    )
                    "

      list_par <-
        list('D' = D,
             'learning_rate' = input[['inf_learning_rate']],
             'num_iter' = input[['inf_num_iter']],
             'num_rs' = input[['inf_num_rs']],
             'n_try_bs' = input[['inf_n_try_bs']],
             'num_processes' = input[['inf_num_processes']],
             'seed' = input[['inf_seed']],
             'random_tree' = input[['inf_random_tree']],
             'marginalize' = input[['inf_marginalize']],
             'keep_equivalent' = input[['inf_keep_equivalent']],
             'check_indistinguishable' = input[['inf_check_indistinguishable']],
             'error_move' = input[['inf_error_move']],
             'show' = input[['inf_show']],
             'alpha' = as.list(alpha),
             'beta' = as.list(beta)
        )



      # ##!!! ATTENZIONE QUESTO E' UN TEST FATTO CON 3 CELLULE TUTTE IN TIME_1
      # ## ATTENZIONE LACE1.0 NON ACCETTA che non ci siano cellule ad un tempo
      # ## OVVIO MA ANCHE NO
      # ## QUI SI DUPLICANO I TEMPI VUOTI
      # ## COSI NON DA ERRORE
      # ## METTERE UN CHECK PRIMA DI PROCEDERE
      # D$`before treatment`[1,2] <- 1
      # D$`4d on treatment` <- D$`before treatment` #copia
      # D$`28d on treatment` <- D$`before treatment` #copia
      # D$`57d on treatment` <- D$`before treatment` #copia
      #
      # rownames(D$`4d on treatment`)= c("S1","S2","S3") #nome cellule
      # rownames(D$`28d on treatment`)= c("S4","S5","S6")
      # rownames(D$`57d on treatment`)= c("S9","S8","S7")
      #
      # list_par[["D"]] <- D # must be erased
      #
      # ##!!! fine preparazione ATTENZIONE QUESTO E' UN TEST FATTO CON 3 CELLULE
      # ## CANCELLARE QUANTO SOPRA PRIMA DI MANDARE


      show_tmp <- list_par[["show"]]
      list_par[["show"]] <- F
      inputs[['inf_params']](list_par)

      #browser()
      #inference_res <- LACE::LACE(D, show = F)
      inference_res=do.call('LACE',inputs[['inf_params']]())

      save(inference_res,file=file.path(inputs[["project_folder_std"]](),"inference.RData"))

      returned_vals <- show_result(rs_())
      rs_(returned_vals$rs)
      #inputs[["res_lastrefresh"]](returned_vals$message)
    }
}



show_result <- function (rs, show = T) {


  #browser()


  so <- sopt()
  so$stderr <- "|"
  sopt(so)

  returned_vals <- list()

  if (file.exists(file.path(inputs[["project_folder_std"]](),"inference.RData"))) {
    load(file=file.path(inputs[["project_folder_std"]](),"inference.RData"))

    B <- inference_res$B
    clones_prevalence <- inference_res$clones_prevalence
    C <- inference_res$C
    error_rates <- inference_res$error_rates

    if (!is.null(B) && !is.null(C) && !is.null(clones_prevalence) && !is.null(error_rates))
      if (show) {
        #browser()
        x <- lace_interface(
          B_mat = B,
          clones_prevalence = clones_prevalence,
          C_mat = C,
          error_rates = error_rates
        )

        #browser()



        long_job <- function(port, x) {
          library(shiny)
          library(shinyjs)
          library(htmltools)
          library(htmlwidgets)
          #library(LACE)

          res <- list(
            "ui" = fluidPage(
              useShinyjs(),
              LACE:::LACEOutput("LCT_FP", height = "900px", width = "900px")
            ),
            "server" = function (input, output, session) {
              output$LCT_FP = LACE:::renderLACE(x)
              outputOptions(output,
                            "LCT_FP",
                            suspendWhenHidden = FALSE)
              hide(id = "navbar")
            }
          )

          server_env <- environment(res$server)
          server_env$x <- x
          shiny::runApp(res, display.mode = "normal", launch.browser=F, port = port, quiet = F )
        }

        port(httpuv::randomPort(min = 1024L, max = 49151L, host = "127.0.0.1", n = 20))

        if (class(rs)[1] == "r_session")
        {
          #rs$kill_tree()
          rs$close()
          rs$finalize()
        }

        rs <- callr::r_session$new(options = sopt(), wait = T)
        rs$supervise(T)
        rs$poll_process(4)

        rs$call(long_job, args= list("port" = port(), "x" = x))

        adr <- reactive({
          Sys.sleep(stime())
          print(paste0("http://127.0.0.1:", port()))
          paste0("http://127.0.0.1:", port())
        })


        stime(12)
        print(adr)
        output$res <- renderUI({
          tags$iframe(src=adr(),  height=1000, width=1000, frameborder = "no")
        })

        outputOptions(output,
                      "res",
                      suspendWhenHidden = FALSE)
        #browser()
        stime(15)
        # stime(2) # melius
        # stime(10) # abund
        # stime(10) # quam
        # stime(10)
        # stime(10)

        returned_vals$message <- paste("Last update: ", date())
        returned_vals$rs <- rs

        inputs[["long_job"]](inputs[["long_job"]]()+1)



      }
  } else {
    #returned_vals$message <- paste("Inference results not available. Run LACE first.")
    returned_vals$message <- paste("")

    returned_vals$rs <- rs

    showNotification("Inference results not available. Run LACE first.", duration = 10, type = "warning")

  }

  inputs[["res_lastrefresh"]](returned_vals$message)


  return(returned_vals)

}


observeEvent(inputs[["long_job"]](),{
  #browser()
  show_tab(T)
}, ignoreInit = T)

## inf_go

inf_go <- function() {
  #browser()
  # if(inputs[["demo"]]() == "Small_dataset")
  if(!is.null(inputs[["demo"]]()))
  {
    inf_exec()
  } else
  {
    av_exec()
    thr_exec()
    dp_exec()
    va_exec()
    inf_exec()
  }

  # shinyjs::hide(id="computation_idCol_div")
  # showTab(inputId = "main_tabset", target = "SC metadata")
  # showTab(inputId = "main_tabset", target = "Annotations")
  # showTab(inputId = "main_tabset", target = "Filters")
  # showTab(inputId = "main_tabset", target = "SC sampling depths")
  # showTab(inputId = "main_tabset", target = "Variants")
  # showTab(inputId = "main_tabset", target = "Inference")
  # browser()
  # delay(2000,
  #       updateTabsetPanel(session, "main_tabset", selected = "Longitudinal display")
  # )

}

### End inference functions ####

### Inference observers ####

observeEvent(reactiveValuesToList(input), {
  outs <- outputOptions(output)
  lapply(names(outs), function(name) {
    outputOptions(output, name, suspendWhenHidden = FALSE)
  })
},
once = T,
priority = -1)


observe({
  inf_observers = lapply(inf_uis,
                         function(i) {
                           observeEvent(input[[i]], {
                             ## req(input)
                             inputs[[i]](input[[i]])
                           })


                           output[[i]] <- renderText(inputs[[i]]())
                         }
  )
},
priority = -1)


observeEvent(session$userData[["ed_table_react_list"]][["inf_alpha"]], {
  ## inputs[["inf_alpha"]](session$userData[['ed_table_react_list']][['inf_alpha']]$x$data)
  ## inputs[["inf_alpha"]](session$userData[['ed_table_react_list_obj']]$obj[["inf_alpha"]]$x$data)
  ## inputs[["inf_alpha"]](do.call(cbind, yaml::yaml.load(yaml::as.yaml(session$userData[['ed_table_react_list_obj']]$obj[["inf_alpha"]]$x$data))) %>% {as.data.frame((.)[,-1], row.names = (.)[,1])})

  #inputs[["inf_alpha"]](session$userData[['ed_table_react_list']][['inf_alpha']]$x$data)
  #data.frame(rownames(session$userData[['ed_table_react_list']][['inf_alpha']]$x$data), session$userData[['ed_table_react_list']][['inf_alpha']]$x$data)
  x <- data.frame(rownames(session$userData[['ed_table_react_list']][['inf_alpha']]$x$data), session$userData[['ed_table_react_list']][['inf_alpha']]$x$data, check.names=F)
  colnames(x)[1]=""
  inputs[["inf_alpha"]](x)
  #inputs[["inf_alpha"]](session$userData[["ed_table_react_list_obj"]]$obj[["inf_alpha"]]$x$data)
})


observeEvent(session$userData[["ed_table_react_list"]][["inf_beta"]], {
  x <- data.frame(rownames(session$userData[['ed_table_react_list']][['inf_beta']]$x$data), session$userData[['ed_table_react_list']][['inf_beta']]$x$data, check.names=F)
  colnames(x)[1]=""
  inputs[["inf_beta"]](x)
})


## Find out interactively the time points and use them as columns
observeEvent(inputs[['m_time_column']](), {
  if (!is.null(inputs[['m_time_points']]) &
      length(inputs[['m_time_points']]()) > 0) {
    empty_table <-
      data.frame(matrix(0.001,
                        ncol = length(inputs[['m_time_points']]()),
                        nrow = 1))
    colnames(empty_table) <- c(inputs[['m_time_points']]())
  } else {
    empty_table <- data.frame(matrix(0.001, ncol = 1, nrow = 1))
    colnames(empty_table) <- c('T1')
  }
  ## ed_tables <- list()
  ## id <- 'inf_alpha'
  for (id in c('inf_alpha', 'inf_beta')) {
    ed_tables[[id]] <-
      ed_table(id,
               empty_table,
               paste0(str_split(id,pattern = '_')[[1]][2],
                      '_'),
               session)
    ## output[[id]] <- render_dt(ed_tables[[id]])
    ## outputOptions(output, id, suspendWhenHidden = FALSE)
  }
})


## Change the order of columns accordingly to time points user
## choice
observeEvent(inputs[['m_time_points']](), {
  req(inputs[['m_time_column']]())
  if(inputs[["load_all_configs"]]()) {
    id <- 'inf_alpha'
    for (id in c('inf_alpha', 'inf_beta')) {
      ## remove extra column
      mm <- isolate(session$userData[["ed_table_react_list"]][[id]]$x$data)
      extra_col <- all(mm[[1]] == rownames(mm))
      if (is.na(extra_col))
        extra_col <- FALSE
      if (extra_col)
        mm <- mm[-1]

      ## Reorder columns
      mm <- mm[,inputs[['m_time_points']](), drop = F]
      session$userData[["ed_table_react_list"]][[id]]$x$data <- mm

      ## Remove extra last row on NAN
      mm <- mm[-nrow(mm),, drop = F]

      ## Create table and render it
      ## output[[id]] <-NULL
      ed_tables[[id]] <-
        ed_table(id,
                 mm,
                 paste0(str_split(id,pattern = '_')[[1]][2], '_'),
                 session)
      print(id)
    }
    id <- 'inf_alpha'
    ## output[['inf_alpha']] <-render_dt(ed_tables[['inf_alpha']])
    session$userData[["ed_table_react_list_obj"]]$obj[[id]] <-
      ed_tables[[id]]
    ## outputOptions(output, id, suspendWhenHidden = FALSE)
    proxy<-dataTableProxy(id)
    replaceData(proxy, mm)

    id <- 'inf_beta'
    ## output[['inf_beta']] <-render_dt(ed_tables[['inf_beta']])
    session$userData[["ed_table_react_list_obj"]]$obj[[id]] <-
      ed_tables[[id]]

    ## outputOptions(output, id, suspendWhenHidden = FALSE)
    proxy<-dataTableProxy(id)
    replaceData(proxy, mm)
    ## refresh()
  }
})

# inputs[["inf_go"]] <- reactiveVal(F)
# observeEvent(inputs[["inf_go"]](), {
#   hideTab(inputId = "main_tabset", target = "SC metadata")
#   shinyjs::hide("SC metadata")
#   shinyjs::hide("Annotations")
#   shinyjs::hide("Filters")
#   shinyjs::hide("SC sampling depths")
#   shinyjs::hide("Variants")
#
#   hideTab(inputId = "main_tabset", target = "Annotations")
#   hideTab(inputId = "main_tabset", target = "Filters")
#   hideTab(inputId = "main_tabset", target = "SC sampling depths")
#   hideTab(inputId = "main_tabset", target = "Variants")
#   hideTab(inputId = "main_tabset", target = "Inference")
#
# }, priority = 5)

observeEvent(input[["inf_next"]], {
  ## inf_exec()
  #browser()

  #print("GGGGGGGGGGGGGGGGGGGGG")
  #inputs[["inf_go"]](!inputs[["inf_go"]]())
  # hideTab(inputId = "main_tabset", target = "Project")
  # hideTab(inputId = "main_tabset", target = "SC metadata")
  # hideTab(inputId = "main_tabset", target = "Annotations")
  # hideTab(inputId = "main_tabset", target = "Filters")
  # hideTab(inputId = "main_tabset", target = "SC sampling depths")
  # hideTab(inputId = "main_tabset", target = "Variants")
  # hideTab(inputId = "main_tabset", target = "Inference")
  # shinyjs::show(id="computation_idCol_div")

  hide_tab()

  delay(2000, inf_go())



},
ignoreInit = T)


observe({
  hide(id = "inf_alpha_div")
  if (file.exists(sc_metadata_label_()) )
    if (!is.null(inputs[['m_time_column']]))
      if (inputs[['m_time_column']]() > 0)
        shinyjs::show(id="inf_alpha_div")
})

observe({
  hide(id = "inf_beta_div")
  if ( file.exists(sc_metadata_label_()) )
    if (!is.null(inputs[['m_time_column']]))
      if (inputs[['m_time_column']]() > 0)
        shinyjs::show(id="inf_beta_div")
})

### End inference observes ####

### Inference outputs ####
output[['inf_alpha']] <-
  DT::renderDT(session$userData[["ed_table_react_list_obj"]]$obj[['inf_alpha']],
               server = T)
outputOptions(output, 'inf_alpha', suspendWhenHidden = FALSE)
output[['inf_beta']] <-
  DT::renderDT(session$userData[["ed_table_react_list_obj"]]$obj[['inf_beta']],
               server = T)
outputOptions(output, 'inf_beta', suspendWhenHidden = FALSE)


output[['inf_par']] <- renderPrint({
  alpha <-
    transpose(as.data.table(session$userData[["ed_table_react_list"]][["inf_alpha"]]$x$data));
  alpha <-
    alpha[, 1:(ncol(alpha) - 1)];
  beta <-
    transpose(as.data.table(session$userData[["ed_table_react_list"]][["inf_beta"]]$x$data));
  beta <-
    beta[,1:(ncol(beta)-1)];

  list("inf_learning_rate" = input[["inf_learning_rate"]],
       "inf_num_iter" = input[["inf_num_iter"]],
       "inf_num_rs" = input[['"inf_num_rs"']],
       "inf_n_try_bs" = input[["inf_n_try_bs"]],
       "inf_num_processes" = input[["inf_num_processes"]],
       "inf_seed" = input[["inf_seed"]],
       "inf_random_tree" = input[["inf_random_tree"]],
       "inf_marginalize" = input[["inf_marginalize"]],
       "inf_keep_equivalent" = input[["inf_keep_equivalent"]],
       "inf_check_indistinguishable" = input[["inf_check_indistinguishable"]],
       "inf_error_move" = input[["inf_error_move"]],
       "inf_show" = input[["inf_show"]],
       "inf_alpha" = as.list(alpha),
       "inf_beta" = as.list(beta) )
})


### End inference outputs ####


### end of file -- inference_tab.R


