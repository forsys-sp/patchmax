// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]

#include <vector>
#include <Rcpp.h>
#include <RcppParallel.h>
#include "graph.h"
#include "distance_mat.h"
using namespace RcppParallel;

// constructor

distanceMat::distanceMat(Graph *gr, IVec dep, IVec arr, double max_aux, Rcpp::NumericMatrix result):
  m_gr(gr), m_dep(dep), m_arr(arr), d_max_aux(max_aux), m_result(result)
{
  add = false;
  if (m_gr->add.size() > 0) add = true;
}

void distanceMat::operator()(std::size_t begin, std::size_t end){
  dijkstra_mat(begin, end);
}

void distanceMat::dijkstra_mat(std::size_t begin, std::size_t end){
  
  DVec distances(m_gr->nbnode, numeric_limits<double>::max());
  DVec distances2;
  if (add) distances2.resize(m_gr->nbnode, numeric_limits<double>::max());
  
  for (std::size_t k = begin; k != end; k++){
    
    int start = m_dep[k];
    distances[start] = 0.0;
    if (add) distances2[start] = 0.0;
    
    PQ Q;
    Q.push(make_pair(start, 0.0));
    
    while (!Q.empty()) {
      
      int v = Q.top().first;
      double w = Q.top().second;
      Q.pop();
      
      if (w <= distances[v]) {
        
        for (int i = m_gr->indG[v]; i<  m_gr->indG[v+1]; i++) {
          
          int v2 = m_gr->nodeG[i];
          double w2 = m_gr->wG[i];
          
          if (distances[v] + w2 < distances[v2]) {
            distances[v2] = distances[v] + w2;
            if (add) {
              distances2[v2] = distances2[v] + m_gr->add[i];
              if (distances2[v2] > d_max_aux){
                break;
              }
            }
            
            Q.push(make_pair(v2, distances[v2]));
          }
        }
      }
    }
    
    for (int i = 0; i != m_arr.size(); i++){
      m_result(k,i) = distances[m_arr[i]];
    }
    
    //Reinitialize
    fill(distances.begin(),distances.end(),numeric_limits<double>::max());
    if (add) fill(distances2.begin(),distances2.end(),numeric_limits<double>::max());
  }
}
