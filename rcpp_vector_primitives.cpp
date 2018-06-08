
#include <Rcpp.h>
using namespace Rcpp;

//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//



// [[Rcpp::export]]
NumericVector rcpp_delay_n(NumericVector x, int n, double val = 0 ) {
  int k = x.size();
  NumericVector xx(k, val);
  
  for(int i = n; i < k; i++){
    xx[i] = x[i-n];
  }
  return xx;
}

// [[Rcpp::export]]
NumericVector rcpp_hasten_n(NumericVector x, 
                            int n, 
                            double val = 0 ) {
  int k = x.size();
  NumericVector xx(k, val);
  
  for(int i = n; i < k; i++){
    xx[i-n] = x[i];
  }
  return xx;
}

// pad vector with zeroes
// [[Rcpp::export]]
NumericVector rcpp_pad_n( NumericVector x, int n) {
  if(n<1)return x;
  int k = x.size();
  NumericVector xx(k+n*2);
  memcpy(&(xx[n]),&(x[0]),k*sizeof(x[0]));
  return xx;
}


// [[Rcpp::export]]
Rcpp::NumericVector rcpp_ifelse(Rcpp::LogicalVector a,
                                Rcpp::NumericVector x,
                                Rcpp::NumericVector y) {
  return ifelse(a, x, y);
}






