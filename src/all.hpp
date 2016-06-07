#include <Rcpp.h>
using namespace Rcpp ;

#ifndef HALL
#define HALL

template <int RTYPE> class IndexQueue;
template <int RTYPE, typename RETTYPE> RETTYPE top_index(Vector<RTYPE> v, int n);


#endif
