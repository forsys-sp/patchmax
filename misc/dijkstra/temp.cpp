
// Program to find Dijkstra's shortest path using
// priority_queue in STL
#include <Rcpp.h>
#include <queue>
using namespace std;

// define infinity
#define INF 0x3f3f3f3f
 
// iPair ==> Integer Pair
typedef pair<int, int> iPair;
 
// directed graph using adjacency list representation
class Graph {
  
  // No. of vertices
  int V;

  // create adj list  vertex and weight pair for every edge
  // * makes this into a pointer (allocated in the Graph function)
  list<pair<int, int>>* adj;
 
  public:
    
    // Constructor
    Graph(int V); 
   
    // function to add an edge to graph
    void add_edge(int u, int v, int w);
   
    // prints shortest path from s
    void dijkstra_distance_mat(int s);
      
};
 
// Allocates memory for adjacency list
// Arrow(->) operator exists to access the members of the structure
Graph::Graph(int V)
{
    this->V = V; // assign V to V
    adj = new list<iPair>[V]; // create adjacency list
}


void Graph::add_edge(int u, int v, int w)
{
    adj[u].push_back(make_pair(v, w));
    adj[v].push_back(make_pair(u, w));
}
 
// Prints shortest paths from src to all other vertices
void Graph::dijkstra_distance_mat(int src)
{
    // reverse priority queue for vertices to be processed
    priority_queue<iPair, vector<iPair>, greater<iPair>> pq;
 
    // Create a vector for distances and initialize all distances as infinite (INF)
    vector<int> dist(V, INF);
 
    // add source to priority queue and set distance to 0.
    pq.push(make_pair(0, src));
    dist[src] = 0;
 
    // loop until priority queue becomes empty (or all distances are not finalized) 
    while (!pq.empty()) {
      
        // Extract minimum distance vertex from priority queue
        int u = pq.top().second;
        pq.pop();
 
        // use i to iterate through neighbors of current vertex
        list<pair<int, int>>::iterator i;
        
        for (i = adj[u].begin(); i != adj[u].end(); ++i) {
          
            // get vertex label and weight of current neighbor of u.
            int v = (*i).first;
            int weight = (*i).second;
 
            // if path to v through u is shorter
            if (dist[v] > dist[u] + weight) {
              
                // updating distance to v
                dist[v] = dist[u] + weight;
              
                // push neighbor to priority queue
                pq.push(make_pair(dist[v], v));
            }
        }
    }
 
    // Print shortest distances stored in dist[]
    printf("Vertex\t\tDistance\n");
    for (int i = 0; i < V; ++i)
        printf("%d \t\t %d\n", i, dist[i]);
}
 
// Driver program to test methods of graph class
// [[Rcpp::export]]
int main()
{
    // create the graph given in above figure
    int V = 9;
    Graph g(V);
 
    // making above shown graph
    g.add_edge(0, 1, 4);
    g.add_edge(0, 7, 8);
    g.add_edge(1, 2, 8);
    g.add_edge(1, 7, 11);
    g.add_edge(2, 3, 7);
    g.add_edge(2, 8, 2);
    g.add_edge(2, 5, 4);
    g.add_edge(3, 4, 9);
    g.add_edge(3, 5, 14);
    g.add_edge(4, 5, 10);
    g.add_edge(5, 6, 2);
    g.add_edge(6, 7, 1);
    g.add_edge(6, 8, 6);
    g.add_edge(7, 8, 7);
 
    g.dijkstra_distance_mat(6);
 
    return 0;
}

/*** R
main()
*/