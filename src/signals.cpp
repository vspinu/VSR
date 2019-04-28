#include <Rcpp.h>
#include <vector>
using namespace Rcpp;
using std::vector;


//' @export
// [[Rcpp::export]]
NumericVector c_buy_sell_signal(NumericVector& X,
								double qenter_sell = .3, double qexit_sell = .5,
								double qenter_buy=.7, double qexit_buy = .5) {

  if (qexit_sell > qexit_sell || qexit_buy > qenter_buy)
	Rf_error("Exit/enter buy/sell quantifies are not in increasing order");
	  
  int N = X.length();
  NumericVector out(N);
  int prev_sig = 0;
  int ix = 0;
 
  while (ix < N) {
	double x = X[ix];
    if (ISNA(x)) {
      out[ix] = prev_sig;
    } else if (prev_sig > 0) {
	  if (x > qexit_buy)
		out[ix] = 1;
	  else if (x < qenter_sell)
		out[ix] = -1;
	  else
		out[ix] = 0;
	} else if (prev_sig < 0) {
	  if (x < qexit_sell)
		out[ix] = -1;
	  else if (x > qenter_buy)
		out[ix] = 1;
	  else
		out[ix] = 0;
	} else {
	  if (x > qenter_buy)
		out[ix] = 1;
	  else if (x < qenter_sell)
		out[ix] = -1;
	  else
		out[ix] = 0;
	}
	prev_sig = out[ix];
	ix++;
  }
  return out;
}
