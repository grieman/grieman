#' loan_calc
#'
#' @param total debt outstanding at start
#' @param r interest rate per period
#' @param payment payment per period. scalar or vector
#'
#' @return a list of vectors - total outstanding, payments per period, and interest paid per period
#' @export
#'
#' @examples
#' loan = loan_calc(1000, .1, 110)
loan_calc = function(total, r, payment){
  remaining = c(total)
  i = 1
  if(length(payment)==1){
    while(remaining[i] > payment){
      remaining[i+1] = (remaining[i] * (1 + r) - payment)
      i = i + 1
    }
    payments = c(rep(payment, i-1), remaining[i]* (1 + r))
  } else {
    while(remaining[i] > payment[i]){
      remaining[i+1] = (remaining[i] * (1 + r) - payment[i])
      i = i + 1
      if(length(payment) < i){payment[i] = payment[i-1]}
    }
    payments = c(payment, remaining[i]* (1 + r))
  }
  remaining = c(remaining, 0)
  interest_paid = payments + diff(remaining)
  outlist = list(remaining, payments, interest_paid)
  names(outlist) <- c("Outstanding", "Payments", "Interest_Paid")
  return(outlist)
}
