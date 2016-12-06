#include <Rcpp.h>
#include <vector>
using namespace Rcpp;
using std::vector;

#define NEGINF -std::numeric_limits<double>::infinity()
#define POSINF std::numeric_limits<double>::infinity()
// #define NA_DBL std::numeric_limits<double>::quiet_NaN()

// [[Rcpp::plugins("cpp11")]]

// template <typename Compare>
// NumericVector ob_margin(NumericVector& X, NumericVector& V){
//   if (X.length() != V.length()){
//     Rf_error("Input vectors 'X' and 'V' must be of the same length.");
//   }

//   size_t N = X.length();
//   NumericVector out(N);

//   std::map <double, bool, Compare> ob;

//   for (size_t i = 0; i < N; i++) {
//     if (V[i] == 0.0) {
//       ob.erase(X[i]);
//     } else {
//       ob[X[i]] = 1;
//     }
//     // will fail on empty map
//     out[i] = ob.cbegin()->first;
//   }

//   // for (auto it = ob.begin(); it != ob.end(); ++it)
//   //   std::cout << it->first << " => " << it->second << '\n';

//   return out;
// }


// //' @export
// // [[Rcpp::export]]
// NumericVector c_max_ob_margin(NumericVector& X, NumericVector& V){
//   return ob_margin<std::greater<double>> (X, V);
// }

// //' @export
// // [[Rcpp::export]]
// NumericVector c_min_ob_margin(NumericVector& X, NumericVector& V){
//   return ob_margin<std::less<double>> (X, V);
// }

void update_obs(double X, double V, double side,
                std::map <double, double, std::less<double>> &ask_ob, 
                std::map <double, double, std::greater<double>> &bid_ob){

  if (side == 1){

    // BID SIDE
    if (V == 0.0) {
      bid_ob.erase(X);
    } else {
      bid_ob[X] = V;
    }

    auto ask_it = ask_ob.cbegin();
    double max_bid = bid_ob.empty() ? NEGINF : bid_ob.cbegin()->first;

    // erase erroneous leftovers on aks side
    while (ask_it != ask_ob.end() && max_bid > ask_it->first){
      ask_it++;
    }
    ask_ob.erase(ask_ob.cbegin(), ask_it);
      
  } else if (side == 2) {

    // ASK SIDE
    if (V == 0.0) {
      ask_ob.erase(X);
    } else {
      ask_ob[X] = V;
    }

    auto bid_it = bid_ob.cbegin();
    double min_ask = ask_ob.empty() ? POSINF : ask_ob.cbegin()->first;

    // erase erroneous leftovers on bids side
    while (bid_it != bid_ob.end() && bid_it->first > min_ask){
      bid_it++;
    }
    bid_ob.erase(bid_ob.cbegin(), bid_it);
    
  } else Rf_error("Invalid side %f", side);
}

//' @export
// [[Rcpp::export]]
List c_ob_margin(NumericVector& X, NumericVector& V, IntegerVector& side){

  // If for whatever reason values in OB are not removed (through 0.0 marker)
  // forcefully remove these values by means of opposite OB side.
  if (X.length() != V.length() && X.length() != side.length()){
    Rf_error("Input vectors 'X', 'V' and 'side' must be of the same length.");
  }

  size_t N = X.length();
  NumericVector bids(N), asks(N);

  std::map <double, double, std::less<double>> ask_ob;
  std::map <double, double, std::greater<double>> bid_ob;
  
  for (size_t i = 0; i < N; i++) {

    update_obs(X[i], V[i], side[i], ask_ob, bid_ob);

    asks[i] = ask_ob.empty() ? NA_REAL : ask_ob.cbegin()->first;
    bids[i] = bid_ob.empty() ? NA_REAL : bid_ob.cbegin()->first;
  }

  List out;
  out["bid"] = bids;
  out["ask"] = asks;
  return out;
}

template <typename Compare>
double ob_exp_sum(double ref, double focal, double n,
                  std::map <double, double, Compare> &ob,
                  double min_exp_bound = 0.001){
  double out = 0.0, wo = 0.0, wn = 0.0;
  // std::cout << "ref:" << ref << " focal:" << focal << " n:" << n << " entries: ";
  for(auto& el : ob){
    // std::cout << el.first << ":" << el.second << ' ';
    wn = exp(-fabs(fabs(el.first - ref) - focal)/n);
    if (wn < wo && wn < min_exp_bound)
      // Break when weigh decreased below boundary to save some cycles. 
      break;
    out += wn * el.second;
    wo = wn;
  }
  // std::cout <<  "(" << out << ")" << std::endl;
  return out;
}

//' @export
// [[Rcpp::export]]
List c_ob_exp_sum(NumericVector& price, NumericVector& size, IntegerVector& side,
                  NumericVector& focals, NumericVector& ns){
  
  size_t N = price.length();
  size_t F = focals.length();

  if (N != size.length() || N != side.length()){
    Rf_error("Input vectors 'price',  'size' and 'size' vectors must be of the same length.");
  }

  if (F != ns.length()){
    Rf_error("Input vectors 'focals' and 'ns' must be of the same length.");
  }
  
  std::map <double, double, std::less<double>> ask_ob;
  std::map <double, double, std::greater<double>> bid_ob;

  NumericMatrix ask_out(N, F);
  NumericMatrix bid_out(N, F);

  double am = -1, bm = -1;

  for (size_t n = 0; n < N; n++) {

    int s = side[n];
    
    update_obs(price[n], size[n], s, ask_ob, bid_ob);

    double ask_min = ask_ob.empty() ? NA_REAL : ask_ob.cbegin()->first;
    double bid_max = bid_ob.empty() ? NA_REAL : bid_ob.cbegin()->first;

    // std::cout << "ask_min:" << ask_min << " bid_max:" << bid_max << std::endl;
    
    if (!ISNA(ask_min) && !std::isinf(ask_min) &&
        !ISNA(bid_max) && !std::isinf(bid_max)) {
      // recompute on actual change only
      if (bm != bid_max || s == 2) {
        for (size_t f = 0; f < F; f++) {
          ask_out(n, f) = ob_exp_sum<std::less<double>>(bid_max, focals[f], ns[f], ask_ob);
        }
      } else {
        for (size_t f = 0; f < F; f++) {
          ask_out(n, f) = ask_out(n - 1, f);
        }
      }
      
      if (am != ask_min || s == 1) {
        for (size_t f = 0; f < F; f++) {
          bid_out(n, f) = ob_exp_sum<std::greater<double>>(ask_min, focals[f], ns[f], bid_ob);
        }
      } else {
        for (size_t f = 0; f < F; f++) {
          bid_out(n, f) = bid_out(n - 1, f);
        }
      }

    } else {
      for (size_t f = 0; f < F; f++){
        ask_out(n, f) = NA_REAL;
        bid_out(n, f) = NA_REAL;
      }
    }

    am = ask_min;
    bm = bid_max;
  }

  List out = List::create(
    Named( "ask" ) = ask_out, 
    Named( "bid" ) = bid_out);
  
  return out;
}


//
