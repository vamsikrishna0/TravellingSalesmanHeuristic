# Implementing a Heuristic Algorithm to solve the Travelling Salesman Problem

Based on the research paper: Ant Colony System: A Cooperative Learning
Approach to the Traveling Salesman Problem,
Marco Dorigo, Senior Member, IEEE, and Luca Maria Gambardella, Member, IEEE

The goal of this project is to implement in Clojure programming language, a
Traveling Salesman Problem’s solution using Ant Colony System approach. 
The algorithm is based on the paper by, Marco Dorigo and Luca Maria
Gambardella. The shortest path is obtained by running the
algorithm with the input values taken from TSPLIB website[3]. The datasets are
geographical cities with (x,y) coordinates. The distances can be calculated by
euclidean distance using the coordinates for each pair of cities. The datasets used
are eil51, eil76and kroA100.

References:
[1] Ant Colony System: A Cooperative Learning Approach to the Traveling
Salesman Problem.
Marco Dorigo, Senior Member, IEEE, and Luca Maria Gambardella, Member, IEEE
[2] Traveling Salesman Problem In Wikipedia, The Free Encyclopedia.
Retrieved from: https://en.wikipedia.org/wiki/Travelling_salesman_problem
[3] Traveling Salesman Problem Library TSPLIB
Retreived from http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/tsp/
[4] Clojure documentation
