// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]

#include <vector>
#include <Rcpp.h>
#include <RcppParallel.h>
#include "graph.h"
#include "cgraph.h"
#include "stall.h"
#include "distance_mat.h"
using namespace Rcpp;
using namespace RcppParallel;
using namespace std;

// [[Rcpp::export]]
NumericVector dijkstra_mat(int start, NumericVector nodes, std::size_t end){
  
  using PQ = priority_queue<pair<int, double>, vector<pair<int, double> >>;
  using IVec = vector<int>;
  using DVec = vector<double>;
  using SVec = vector<string>;
  
  // create distance vector and populate wtih infinity
  DVec distances(nodes, numeric_limits<double>::max());
    
  // create priority queue
  PQ Q;
  
  // set distance to 0 and push on top of priority queue
  distances[start] = 0.0;
  Q.push(make_pair(start, 0.0));
  
  while (!Q.empty()) {
    
    // pop v & w for current vertex and remove form queue
    int v = Q.top().first;
    double w = Q.top().second;
    Q.pop();
    
    // if w <= distance
    if (w <= distances[v]) {
      
      // for each neighbor in indG
      for (int i = m_gr->indG[v]; i<  m_gr->indG[v+1]; i++) {
        
        // identify alter & weight
        int v2 = m_gr->nodeG[i];
        double w2 = m_gr->wG[i];
        
        // update distance if less than record
        if (distances[v] + w2 < distances[v2]) {
          distances[v2] = distances[v] + w2;

          // add to to priority queue if updated
          Q.push(make_pair(v2, distances[v2]));
        }
      }
    }
  }
}