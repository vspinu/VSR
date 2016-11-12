#include <Rcpp.h>
#include <vector>
using namespace Rcpp;
using std::vector;

#define NEGINF -std::numeric_limits<double>::infinity()
#define POSINF std::numeric_limits<double>::infinity()

// [[Rcpp::plugins("cpp11")]]

template <typename Compare>
NumericVector ob_margin(NumericVector& X, NumericVector& V){
  if (X.length() != V.length()){
    Rf_error("Input vectors 'X' and 'V' must be of the same length.");
  }

  size_t N = X.length();
  NumericVector out(N);

  std::map <double, bool, Compare> ob;

  for (size_t i = 0; i < N; i++) {
    if (V[i] == 0.0) {
      ob.erase(X[i]);
    } else {
      ob[X[i]] = 1;
    }
    // will fail on empty map
    out[i] = ob.cbegin()->first;
  }

  // for (auto it = ob.begin(); it != ob.end(); ++it)
  //   std::cout << it->first << " => " << it->second << '\n';

  return out;
}


//' @export
// [[Rcpp::export]]
NumericVector c_max_ob_margin(NumericVector& X, NumericVector& V){
  return ob_margin<std::greater<double>> (X, V);
}

//' @export
// [[Rcpp::export]]
NumericVector c_min_ob_margin(NumericVector& X, NumericVector& V){
  return ob_margin<std::less<double>> (X, V);
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

  std::map <double, bool, std::less<double>> ask_ob;
  std::map <double, bool, std::greater<double>> bid_ob;
  double
    max_bid = NEGINF, 
    min_ask = POSINF;

  for (size_t i = 0; i < N; i++) {
    
    if (side[i] == 1){

      // BID SIDE
      if (V[i] == 0.0) {
        bid_ob.erase(X[i]);
      } else {
        bid_ob[X[i]] = 1;
      }
      auto bid_it = bid_ob.cbegin();

      if(bid_ob.empty())
        {
          max_bid = NEGINF;
        }
      else
        {
          max_bid = bid_it->first;
          if (max_bid > min_ask) {
            auto ask_it = ask_ob.cbegin();
            while (ask_it != ask_ob.end() && max_bid > ask_it->first){
              ask_it++;
            }
            ask_ob.erase(ask_ob.cbegin(), ask_it);
            if(ask_ob.empty())
              min_ask = max_bid;
            else
              min_ask = ask_it->first;
          }
        }
      
    } else if (side[i] == 2) {

      // ASK SIDE
      if (V[i] == 0.0) {
        ask_ob.erase(X[i]);
      } else {
        ask_ob[X[i]] = 1;
      }
      auto ask_it = ask_ob.cbegin();

      if (ask_ob.empty())
        {
          min_ask = POSINF;
        }
      else
        {
          min_ask = ask_it->first;
          if(max_bid > min_ask){
            auto bid_it = bid_ob.cbegin();
            while (bid_it != bid_ob.end() && bid_it->first > min_ask){
              bid_it++;
            }
            bid_ob.erase(bid_ob.cbegin(), bid_it);
            if(bid_ob.empty())
              max_bid = min_ask;
            else 
              max_bid = bid_it->first;
          }
        }
    } else Rf_error("Invalid side at index %d", i + 1);

    asks[i] = std::isinf(min_ask) ? NA_REAL : min_ask;
    bids[i] = std::isinf(max_bid) ? NA_REAL : max_bid;
  }

  List out;
  out["bid"] = bids;
  out["ask"] = asks;
  return out;
}

void upate_obs(double X, double V, double side,
               std::map <double, double, std::less<double>> &ask_ob, 
               std::map <double, double, std::greater<double>> &bid_ob){

  auto bid_it = bid_ob.cbegin();
  auto ask_it = bid_ob.cbegin();

  double
    max_bid = bid_ob.empty() ? NEGINF : bid_it->first,
    min_ask = ask_ob.empty() ? POSINF : ask_it->first;
  
  if (side == 1){

    // BID SIDE
    if (V == 0.0) {
      bid_ob.erase(X[i]);
    } else {
      bid_ob[X] = V;
    }

    if (max_bid > min_ask) {
      // erase erroneous leftovers on aks side
      while (ask_it != ask_ob.end() && max_bid > ask_it->first){
        ask_it++;
      }
      ask_ob.erase(ask_ob.cbegin(), ask_it);
    }
      
  } else if (side[i] == 2) {

    // ASK SIDE
    if (V == 0.0) {
      ask_ob.erase(X);
    } else {
      ask_ob[X] = V;
    }

    if(max_bid > min_ask){
      // erase erroneous leftovers on bids side
      while (bid_it != bid_ob.end() && bid_it->first > min_ask){
        bid_it++;
      }
      bid_ob.erase(bid_ob.cbegin(), bid_it);
    }
    
  } else Rf_error("Invalid side %f", side);
}

//' @export
// [[Rcpp::export]]
List c_ob_exp_average(NumericVector& prices, NumericVector& sizes, NumericVector& focals, NumericVector& ns){
  size_t N = prices.length();
  size_t F = focals.length();

  if (N != size.length()){
    Rf_error("Input vectors 'prices' and 'sizes' must be of the same length.");
  }

  if (F != ns.length()){
    Rf_error("Input vectors 'focals' and 'ns' must be of the same length.");
  }

  NumericMatrix out(N, F);

  std::map <double, double, std::less<double>> ob;
  vector<double> acc(F, 0.0);

  for (size_t n = 0; n < N; n++) {
    double p = prices[n];
    double old_size = ob[p];
    double new_size = sizes[n];
    double size_diff = new_size - old_size;

    for (size_t f = 0; f < F; f++){
      double w = exp(-(abs(p - focals[f]))/ns[f]);
      acc[f] += size_diff * w;
    }
    ob[prices[n]] = new_size;
  }
  return out;
}
