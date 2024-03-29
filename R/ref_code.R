

#' Reference coding for factors
#'
#' Use reference coding of factors to make sure there is no possibility of
#'   collinearity while I am running coxph routines inside of orsf().
#'
#' @param x_data data frame to convert to one-hot encoding
#' @param fi factor information
#' @param names_x_data names of the variables to consider
#'
#' @return the same data with factors formatted in one-hot encoding.
#'
#' @noRd
#'
#' @examples
#' dat_refcoded <- ref_code(x_data = pbc_orsf,
#'                   fi = fctr_info(pbc_orsf, .names = 'sex'),
#'                   names_x_data = c('age', 'sex'))
#'
#' head(dat_refcoded)
#'
ref_code <- function (x_data, fi, names_x_data){

 # Will use these original names to help re-order the output

 for(i in seq_along(fi$cols)){

  col_i <- fi$cols[i]

  if(col_i %in% names(x_data)){

   if(fi$ordr[i]){

    x_data[[ col_i ]] <- as.integer( x_data[[ col_i ]] )

   } else {

    for (j in seq(length(fi$keys[[i]]) - 1) ) {

     j_lvl <- fi$lvls[[i]][j+1]

     # find which rows to turn into 1's. These should be the
     # indices in the currect factor where it's value is equal
     # to the j'th level.

     vec <- data.frame(
      temp_name = collapse::alloc(0L, collapse::fnrow(x_data))
     )

     names(vec) <- fi$keys[[i]][j+1]

     hot_rows <- x_data[[col_i]] %==% j_lvl

     if(!is_empty(hot_rows)){
      collapse::setv(
       X = vec,
       v = hot_rows,
       R = 1L,
       vind1 = TRUE
      )
     }

     collapse::add_vars(x_data) <- collapse::get_vars(vec, 1)

    }

   }

  }

 }

 OH_names <- names_x_data

 for (i in seq_along(fi$cols)){

  if(fi$cols[i] %in% names_x_data){
   if(!fi$ordr[i]){

    OH_names <- insert_vals(
     vec = OH_names,
     where = OH_names %==% fi$cols[i],
     what = fi$keys[[i]][-1]
    )

   }
  }

 }

 select_cols(x_data, OH_names)

}

#' insert some value(s) into a vector
#'
#'
#' @param vec the vector to be edited
#' @param where where to insert values
#' @param what what is to be inserted
#'
#' @return a vector with new values inserted
#'
#' @details The value that was originally in the vector at
#'   `where` will be replaced (see example).
#'
#' @noRd
#'
#' @examples
#'
#' insert_vals(vec = 1:4, where = 4, what = c(5L))
#'
insert_vals <- function(vec, where, what){

 stopifnot(
  typeof(what) == typeof(vec),
  where >= 1L & where <= length(vec)
 )

 if(where == 1L){

  if(length(vec) == 1L) return(c(what)) else return(c(what, vec[-1L]))

 }

 if(where == length(vec)) return(c(vec[1L:(length(vec)-1L)], what))

 vec_left <- vec[1L:(where-1L)]
 vec_right <- vec[(where+1L):length(vec)]

 c(vec_left, what, vec_right)

}

