# library(shiny)
# library(DT)
# library(stringr)



dt_output = function(title, id) {
  #fluidRow(
  #  column(12, h1(paste0('Table ', sub('.*?([0-9]+)$', '\\1', id), ': ', title)),
  #  hr(), DTOutput(id)
  #))
  DTOutput(id, width = "100%")
}
render_dt = function(data, server = TRUE, editable = 'cell', ...) {
  #renderDT(data, selection = 'none', server = server, editable = editable, ...)
  renderDT(data, server = server)
}



ed_table <- function(id, tb_data, row_basename='row_', session) {

  input <- session$input
  options(htmlwidgets.TOJSON_ARGS = list(na = 'null'))



  tb_data_tmp <- tb_data
  newrow_tmp<-as.list(rep(NA,ncol(tb_data_tmp)))
  names(newrow_tmp) <- colnames(tb_data_tmp)
  tb_data_tmp[nrow(tb_data_tmp)+1,]=newrow_tmp
  r_names <- paste0(row_basename,seq_len(nrow(tb_data_tmp)))
  r_names[length(r_names)] <- "add row"
  rownames(tb_data_tmp) = r_names


  js_key <- c(
    "table.on('key', function(e, datatable, key, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  if(key == 13 && targetName == 'body'){",
    "    $(cell.node()).trigger('dblclick.dt');",
    "  }",
    "});",
    "table.on('keydown', function(e){",
    "  var keys = [9,13,37,38,39,40];",
    "  if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1){",
    "     console.log($(e.target).val());",
    "     if((!$(e.target).val() || $(e.target).val().length === 0 )) {$(e.target).val('ee')};",
    "     console.log($(e.target).val());",
    "    $(e.target).trigger('blur');",
    "  }",
    "});",
    "table.on('key-focus', function(e, datatable, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  var type = originalEvent.type;",
    "  if(type == 'keydown' && targetName == 'input'){",
    "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
    "      $(cell.node()).trigger('dblclick.dt');",
    "    }",
    "  }",
    "});"
  )


  isolate({
    #session$userData[['ed_table_react_list']][[id]] <- NULL
    session$userData[['ed_table_react_list']][[id]] <- datatable(
      tb_data_tmp,
      selection = "none",
      editable = list(target = 'cell', disable = list(columns = c(0))),
      callback = JS(js_key),
      extensions = c('Scroller',"KeyTable"),#,"Buttons"),
      options = list(
        keys = TRUE,
        #keys = list(blurable= FALSE, editOnFocus = F),
        select =list(blurable=FALSE),
        #deferRender = F,
        scrollY = 100,
        scroller = TRUE,
        dom = 'Bit' #,
        #buttons = c('copy')
      )
    )
    log2_print('create alfa and beta tables')
    log2_print(session$userData[['ed_table_react_list']][[id]]$x$data)
    #print('step 0')
    #print(ed_table_react_list)
    #print(ed_table_react_list[[id]])
    #print(ed_table_react_list[[id]]$x$data)
    #print('step 0b')


  })


  #print('step 1')
  uis_tmp = list(id)



  ###
  #print ('step a')

  # observeEvent(input[[paste0(id,'_cell_edit')]],{
  #   #browser()
  #   print(id)
  #   ed_table_react_list[[id]]$x$data <<- ed_table_react_list[[id]]$x$data[-1]
  # }, priority = 1, once = TRUE)
  #isolate({
  if (!is.null(session$userData$ed_table_react_list_obj$observers[[paste0(id,'cell_edit')]]))
    session$userData$ed_table_react_list_obj$observers[[paste0(id,'cell_edit')]]$destroy()
  #})

  session$userData$ed_table_react_list_obj$observers[[paste0(id,'cell_edit')]]<-observeEvent(input[[paste0(id,'_cell_edit')]],{

    log2_print('-------------')
    #req(inputs[[paste0(id,'_cell_edit')]])
    # print(input[[paste0(id,'_cell_edit')]])
    # print('aaa')
    #old_e <- inputs[[paste0(id,'_cell_edit_old')]]()

    new_e <- isolate(inputs[[paste0(id,'_cell_edit')]]())
    #log2_print('ccc')
    act_e <- isolate(input[[paste0(id,'_cell_edit')]])
    #log2_print('bbb2')

    # print('old_e')
    # print(old_e)
    # print('act_e')
    #log2_print(act_e)
    # print('new_e')
    #log2_print(new_e)
    if ((new_e$col != act_e$col) | (new_e$row != act_e$row) | (new_e$value != act_e$value)) {
      #print((new_e$col != act_e$col) | (new_e$row != act_e$row) | (new_e$value != act_e$value))
      #print(new_e$col != act_e$col)
      #print(new_e$row != act_e$row)
      #print(new_e$value != act_e$value)
      #print(typeof(new_e$value))
      #print(typeof(act_e$value))
      #log2_print('made equal')
      #log2_print(paste('should pre fire', id))
      #print(act_e)
      #print(new_e)

      #inputs[[paste0(id,'_cell_edit_old')]](act_e)
      inputs[[paste0(id,'_cell_edit')]](act_e)
      #old_e <- inputs[[paste0(id,'_cell_edit_old')]]()


      #print(inputs[[paste0(id,'_cell_edit')]]())

    }
    else {
      #log2_print(paste('should no prefire', id))
    }
  })

  if (!is.null(session$userData$ed_table_react_list_obj$observers[[paste0(id,'cell_edit_s')]]))
    session$userData$ed_table_react_list_obj$observers[[paste0(id,'cell_edit_s')]]$destroy()

  session$userData$ed_table_react_list_obj$observers[[paste0(id,'cell_edit_s')]]<-observeEvent(inputs[[paste0(id,'_cell_edit')]](),{
    #log2_print(paste('pre fire', id))
    log2_print(isolate(inputs[[paste0(id,'_cell_edit')]]()))

    # if(inputs[[paste0(id,'_cell_edit')]]()$row==-1)
    # {
    #   print('return')
    #   return()
    # }
    #
    new_e <-  isolate(inputs[[paste0(id,'_cell_edit')]]())
    act_e <-  isolate(input[[paste0(id,'_cell_edit')]])
    #print(paste('new_e',new_e))
    #print(paste('act_e',act_e))
    #if ((new_e$col == act_e$col) | (new_e$row == act_e$row) | (new_e$value == act_e$value)) {
    #  print('return')
    #  return()
    #}

    #return()
    ev <- isolate(inputs[[paste0(id,'_cell_edit')]]())

    x <- id

    #print(id)
    #print('data')
    #print(session$userData[['ed_table_react_list']][[id]]$x$data)
    mm <- isolate(session$userData[['ed_table_react_list']][[id]]$x$data)
    extra_col <- all(mm[[1]] == rownames(mm))
    if (is.na(extra_col))
      extra_col <- FALSE
    if (extra_col){
      mm <- mm[-1]
      #print('extra_col')
    }
    #print('mm')
    #print(mm)
    #print('step 2')
    #print(mm)
    #browser()
    if(length(as.character(ev$value))>=1)
      if(str_length(as.character(ev$value))>=1) {
        if (input[[paste0(id,'_cell_edit')]]$row == nrow(mm)) {
          #print('step 3a')
          for (j in seq(1, ncol(mm))) {
            if (is.na(mm[nrow(mm),j]))
              mm[nrow(mm),j] <- "0.001"
          }
          newrow<-as.list(rep(NA,ncol(mm)))
          #names(newrow) <- colnames(mm)
          df_newrow <-data.frame(newrow)
          names(df_newrow) <- colnames(mm)

          mm <- rbind(mm, df_newrow)
          rownames(mm)[nrow(mm)] <- "add row"
        }
      }
    #print('step 3')
    r_names <- paste0(row_basename, seq(1:nrow(mm)))
    r_names[length(r_names)] <- "add row"
    rownames(mm) <- r_names

    val_tmp<-isolate(ev)
    if(length(as.character(ev$value))>=1)
      if (str_length(as.character(ev$value))==0)
        val_tmp$value=NA
    #print('step 4')
    if (is.na(mm[val_tmp$row, val_tmp$col]))
      mm[val_tmp$row, val_tmp$col]=val_tmp$value
    mm <- editData(mm, val_tmp, proxy = id, resetPaging = FALSE, rownames=TRUE )

    for (i in seq(1:max(nrow(mm)-1,1))) { # i = val_tmp$row
      canc <-0
      for (j in seq(1, ncol(mm))) {
        if (is.na(mm[i,j])) {
          canc <- canc+1
        }
      }
      #print("2222222222222222222222")
      #print(canc == ncol(mm) && i<nrow(mm))
      #print(nrow(mm))
      if(canc == ncol(mm) && i<nrow(mm)) {
        if (nrow(mm)>2) {
          mm_pre <- NULL
          mm_post <- NULL
          if (i>1)
            mm_pre <- mm[1:i-1,]
          if (i<nrow(mm))
            mm_post <- mm[(i+1):nrow(mm),]
          mm <- rbind(mm_pre, mm_post)
          r_names <- paste0(row_basename, seq(1:nrow(mm)))
          r_names[length(r_names)] <- "add row"
          rownames(mm) <- r_names
          break
        }
        else
        {
          for (j in seq(1, ncol(mm))) {
            if (is.na(mm[nrow(mm),j]))
              mm[1,j] <- "0.001"
          }
        }
      }
    }
    # if(is.na(mm[val_tmp$row, val_tmp$col]))
    #   mm[val_tmp$row, val_tmp$col]=NA


    proxy<-dataTableProxy(id)
    #print('mm')
    #print(mm)

    session$userData[['ed_table_react_list']][[id]]$x$data <- editData(mm, val_tmp, resetPaging = FALSE, rownames=TRUE )
    replaceData(proxy, mm)
    #print('update')
    log2_print(isolate(session$userData[['ed_table_react_list']][[id]]$x$data))
    #print(proxy)
    #session$userData[['ed_table_react_list']][[id]]$x$data
    #print(id)
    #print(ed_table_react_list[[id]]$x$data)
    #print('fine')
  }, ignoreInit = TRUE)
  ###

  return(isolate(session$userData[['ed_table_react_list']][[id]]))

}



# d1 = iris[1:3,1:4]
#
# shinyApp(
#   ui = fluidPage(
#     title = 'Double-click to edit table cells',
#     dt_output('prova', 'mytable'),
#     dt_output('prova', 'mytable2'),
#     DTOutput('inf_alpha')
#   ),
#
#   server = function(input, output, session) {
#
#     empty_table <- data.frame(matrix(ncol = 2, nrow = 0))
#     colnames(empty_table) <- c('T1', 'T2')
#     ed_table_alpha <- ed_table('inf_alpha', empty_table, 'alpha_', session)
#     output$inf_alpha <- render_dt(ed_table_alpha)
#
#     d1_tmp <-d1
#     rownames(d1_tmp) = c('alpha_1','alpha_2','alpha_3')
#     newrow2<-as.list(rep(NA,ncol(d1_tmp)))
#     names(newrow2) <- colnames(d1_tmp)
#     d1_tmp[3,]=newrow2
#
#     #options(DT.options = list(pageLength = 8))
#     ed_table_1 <- ed_table('mytable', d1, 'beta___', session)
#     ed_table_2 <- ed_table('mytable2', d1, 'alpha_', session)
#     output$mytable <- render_dt(ed_table_1)
#     output$mytable2 <- render_dt(ed_table_2)
#     #print(ed_table_1)
#   }
# )
