#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame dijkstra_cpp(DataFrame edgelist, int start, double max_aux) {
  
  // extract edge list vector data 
  NumericVector e_f = edgelist["from"];
  NumericVector e_t = edgelist["to"];
  NumericVector e_d = edgelist["dist"];
  NumericVector e_a = edgelist["aux"];
  
  // create list of unvisited nodes
  NumericVector unvisited = unique(e_f);
  
  NumericVector n_v = unvisited;
  NumericVector n_d = rep(R_PosInf, unvisited.size());
  NumericVector n_a = rep(R_PosInf, unvisited.size());
  IntegerVector n_f = rep(0, unvisited.size());

  // set start dist to 0
  n_d[n_v == start] = 0;
  n_a[n_v == start] = 0;
  
  // set step count to 1
  int step = 1, n_sel, n;
  NumericVector sub_n_v, sub_n_v_min, sub_n_a, sub_n_d, neighbors;
  NumericVector dist_n_sel_to_s, dist_n_to_s, step_dist_n, step_aux_n, aux_n_sel_to_s, alt_dist_n_to_s, alt_aux_n_to_s;
  
  // continue until all vertices are visited
  while(unvisited.length() > 0){
    
    //Rprintf("\n\nStep %i", step);
    
    // find vertex with the shortest distance from start
    sub_n_v = n_v[n_f == 0];
    sub_n_d = n_d[n_f == 0];
    sub_n_a = n_a[n_f == 0];
    
    //min_d = min(sub_n_d);
    sub_n_v_min = sub_n_v[sub_n_d == min(sub_n_d)];

    // selected node
    n_sel = sub_n_v_min[0];

    //Rprintf(" @ node %i", n_sel);

    // record distance from selected node to start
    dist_n_sel_to_s = n_d[n_v == n_sel];
    aux_n_sel_to_s = n_a[n_v == n_sel];
    
    if (aux_n_sel_to_s[0] < max_aux) {
      
      //identify neighbors of current vertex
      neighbors = e_t[e_f == n_sel];
      
      // for each neighbor n
      for(int i = 0; i < neighbors.length(); i++){
        
        n = neighbors[i];
        
        // distance estimate from neighbor to start
        dist_n_to_s = n_d[n_v == n];
        
        // step distances
        step_dist_n = e_d[e_f == n_sel & e_t == n];
        step_aux_n = e_a[e_f == n_sel & e_t == n];
        
        // calculate distance going through current vertex
        alt_dist_n_to_s =  dist_n_sel_to_s + step_dist_n;
        alt_aux_n_to_s =  aux_n_sel_to_s + step_aux_n;
        
        //Rprintf("\n- Neighbor %i, step %d, old %d, new %d", n, d, b, a);
        
        // update distance to start if shorter
        if (alt_dist_n_to_s[0] < dist_n_to_s[0]) {
          //Rprintf(" *");
          n_d[n_v == n] = alt_dist_n_to_s[0];
          n_a[n_v == n] = alt_aux_n_to_s[0];
        }
      }
    }
    
    // flag current vertex as visited; remove from unvisited list
    n_f[n_v == n_sel] = 1;
    unvisited = unvisited[unvisited != n_sel];
    
    // increment step
    step++;
  }
  
  DataFrame df = DataFrame::create(
    _["vertex"] = n_v,
    _["dist"] = n_d,
    _["aux"] = n_a,
    _["flag"] = n_f);
  
  return df;
}

/*** R
edgelist_test <- data.frame(
  from=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
  to=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
  dist=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9),
  aux=rep(10,18))
dijkstra_cpp(edgelist_test, 1, 1000)
*/


