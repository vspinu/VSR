#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;
using std::vector;

#define DEBUG

#ifdef DEBUG
#define P(...) printf(__VA_ARGS__)
#else
#define P(...)
#endif


//' @export
// [[Rcpp::export]]
NumericVector c_buy_sell_signal(NumericVector& X, LogicalVector& skip,
								double sell_entry = .3, double sell_exit = .5,
								double buy_exit=.7, double buy_entry = .5) {

  if (sell_entry > sell_exit || buy_exit > buy_entry)
	Rf_error("Exit/enter buy/sell quantifies are not in increasing order");
	  
  int N = X.length();
  NumericVector out(N);
  int prev_sig = 0;
  int ix = 0;
  bool vecskip = skip.length() > 1;
  if (vecskip && N != skip.length()) {
	Rf_error("If 'skip' is a vector it must be of the same length as X");
  }
  while (ix < N) {
	double x = X[ix];
	bool skipit = ISNA(x) || (vecskip ? skip[ix] : false);
    if (skipit) {
      out[ix] = prev_sig;
    } else if (prev_sig > 0) {
	  if (x >= buy_exit)
		out[ix] = 1;
	  else if (x <= sell_entry)
		out[ix] = -1;
	  else
		out[ix] = 0;
	} else if (prev_sig < 0) {
	  if (x <= sell_exit)
		out[ix] = -1;
	  else if (x >= buy_entry)
		out[ix] = 1;
	  else
		out[ix] = 0;
	} else {
	  if (x >= buy_entry)
		out[ix] = 1;
	  else if (x <= sell_entry)
		out[ix] = -1;
	  else
		out[ix] = 0;
	}
	prev_sig = out[ix];
	ix++;
  }
  return out;
}

 
//' @export
// [[Rcpp::export]]
NumericVector c_buy_sell_signal_interval(NumericVector& X,
										 double sell1 = .3, double sell2 = .5,
										 double buy1=.7, double buy2 = .5) {

  int N = X.length();
  NumericVector out(N);
  int prev_sig = 0;
  int ix = 0;

  if (sell1 > sell2)
	std::swap(sell1, sell2);
  if (buy1 > buy2)
	std::swap(buy1, buy2);
  
  int buysig = 0, sellsig = 0;

  while (ix < N) {
	double x = X[ix];
    if (ISNA(x)) {
      out[ix] = prev_sig;
    } else {
	  buysig = (x >= buy1 && x <= buy2);
	  sellsig = (x >= sell1 && x <= sell2);
	  if (buysig > sellsig)
		out[ix] = 1;
	  else if (sellsig > buysig)
		out[ix] = -1;
	  else out[ix] = 0;
	}
	prev_sig = out[ix];
	ix++;
  }
  
  return out;
}

 
//' @export
// [[Rcpp::export]]
NumericVector c_entry_exit_signal(NumericVector& X, double exit=.5, double entry = .7,
								  bool buy = true) {

  if (buy) {
	if (exit > entry) Rf_error("Buy exit > entry");
  } else {
	if (exit < entry) Rf_error("Sell exit < entry");
  }
	  
  int N = X.length();
  NumericVector out(N);
  int prev_sig = 0;
  int ix = 0;
  while (ix < N) {
	double x = X[ix];
	if (buy) {
	  if (prev_sig > 0)
		out[ix] = (x >= exit) ? 1 : 0;
	  else 
		out[ix] = (x >= entry) ? 1 : 0;
	} else {
	  if (prev_sig < 0)
		out[ix] = (x <= exit) ? -1 : 0;
	  else 
		out[ix] = (x <= entry) ? -1 : 0;
	}
	prev_sig = out[ix];
	ix++;
  }
  return out;
}

//' @export
// [[Rcpp::export]]
NumericVector c_butterfly_signal(NumericVector& X,
								 double sell_entry=.3,
								 double buy_entry = .7) {

  if (sell_entry > buy_entry) Rf_error("sell_entry > buy_entry");
	  
  int N = X.length();
  NumericVector out(N);
  int prev_sig = 0;
  int ix = 0;
  while (ix < N) {
	double x = X[ix];
	if (x > buy_entry)
	  out[ix] = 1;
	else if (x < sell_entry)
	  out[ix] = -1;
	ix++;
  }
  return out;
}
