(C) Ricardo Cristovao Miranda, 2016.

This program is a Haskell solution to the problem described in Genetic Algorithms in 
Java Basics, Lee Jacobson, Burak Kanber, chapter 4, Traveling Salesman.
From the book:
The traveling salesman problem is often described in terms of optimizing a
route through a collection of cities; however, the traveling salesman problem
can be applied to other applications. For example, the notion of cities can be
thought of as customers for certain applications, or even as soldering points on a
microchip. The idea of distance can also be revised to consider other constraints
such as time.
The problem we will be tackling in this implementation is a typical traveling
salesman problem in which we need to optimize a route through a collection of
cities. We can generate a number of random cities in 2D space by setting each city
to a random x, y position.
The encoding we choose in this example will need to be able to encode a list of
cities in order. We can do this by assigning each city a unique ID then referencing
it using a chromosome in the order of the candidate route. This type of encoding
where sequences of genes are used is known as permutation encoding and is very
well suited to the traveling salesman problem.
The first thing we need to do is assign our cities unique IDs. If we have 5 cities to
visit, we can simply assign them the IDs: 1,2,3,4,5. Then when our genetic algorithm
has found a route, our chromosome may order the city IDs to look like the following:
3,4,1,2,5. This simply means we would start at city 3, then travel to city 4, then 
city 1, then city 2, then city 5, before returning back to city 3 to complete the
route.
