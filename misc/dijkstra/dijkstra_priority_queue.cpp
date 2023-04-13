#include <Rcpp.h>
#include <queue>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
IntegerVector top_i_pq(NumericVector v, int n) {
  typedef pair<double, int> Elt;
  priority_queue< Elt, vector<Elt>, greater<Elt> > pq;
  vector<int> result;
  
  for (int i = 0; i != v.size(); ++i) {
    if (pq.size() < n)
      pq.push(Elt(v[i], i));
    else {
      Elt elt = Elt(v[i], i);
      if (pq.top() < elt) {
        pq.pop();
        pq.push(elt);
      }
    }
  }
  
  result.reserve(pq.size());
  while (!pq.empty()) {
    result.push_back(pq.top().second + 1);
    pq.pop();
  }
  
  return wrap(result);
}

/*** R
top_i_pq(c(1:100), 10)
*/