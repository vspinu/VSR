#include <Rcpp.h>
#include <queue>
#include "all.hpp"

using namespace Rcpp ;

template <int RTYPE, typename ET>
List tabulate(const Vector<RTYPE>& v)
{
  std::map<ET, size_t> counts;

  for (auto it = v.begin(); it != v.end(); it++){
    // .find doesn't play nicely with double NAs, so just ignore for now. This
    // NA check is fucked, but there seem no other way around
    if(!ISNA(*it) && *it != NA_INTEGER){
      auto found = counts.find(*it);
      if (found != counts.end()){
        found->second++;
      } else {
        counts[*it] = 1;
      }
    }
  }
  
  Vector<RTYPE> keys(counts.size());
  IntegerVector vals(counts.size());

  size_t i = 0;
  
  for (auto it = counts.begin(); it != counts.end(); it++){
    keys[i] = it->first;
    vals[i] = it->second;
    i++;
  }
  
  List out;
  out["vals"] = keys;
  out["counts"] = vals;
  return out;
}
  
// [[Rcpp::export]]
List c_tab(SEXP x){
  switch( TYPEOF(x) ){
  case INTSXP: return tabulate<INTSXP, int>(x) ;
  case REALSXP: return tabulate<REALSXP, double>(x) ;
    // case STRSXP: return top_index<STRSXP, >( x, n ) ;
  default: stop("Type not handled") ; 
  }
}
