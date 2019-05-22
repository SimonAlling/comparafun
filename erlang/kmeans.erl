-module(kmeans).
-export([kmeans_seq/3, kmeans_par/4]).

%% A point is a 3-tuple of floats.

add_point({X1, Y1, Z1}, {X2, Y2, Z2}) ->
  {X1+X2, Y1+Y2, Z1+Z2}.

divide_point({X, Y, Z}, K) ->
  {X/K, Y/K, Z/K}.

distance_sq({X1, Y1, Z1}, {X2, Y2, Z2}) ->
  sq(X1-X2) + sq(Y1-Y2) + sq(Z1-Z2).

sq(X) ->
  X*X.

%% A cluster is a pair of a cluster-id and a point (centroid)

closest_cluster(Point,Clusters) -> do
  , { _, Id } = lists:min([ {distance_sq(Point, C), I} || {I, C} <- Clusters ])
  , Id
  .

assign_points_to_clusters(Points,Clusters) -> do
  , ClosestClusters = [ {closest_cluster(P, Clusters), P} || P <- Points ]
  , [
      { Id, [ P || {Id1, P} <- ClosestClusters, Id1 == Id ] }
    ||
      { Id, _ } <- Clusters
    ]
  .

cluster_pointsums(Points, Clusters) -> do
  , ClusterPoints = assign_points_to_clusters(Points,Clusters)
  , [ {Id, points_to_pointsum(Pts)} || {Id, Pts} <- ClusterPoints ]
  .

%% A pointsum is a pair of {number of points, sum of points}

add_pointsum({N1, P1}, {N2, P2}) ->
  {N1+N2, add_point(P1, P2)}.

points_to_pointsum(Points) ->
  add_pointsums([{1,P} || P <- Points]).

add_pointsums(PointSums) ->
  lists:foldr(fun add_pointsum/2,{0,{0,0,0}}, PointSums).

%% At each step, we replace the centroid of each cluster by the
%% average of the points assigned to it.

recentre(Points, Clusters) -> do
  , ClusterPointsums = cluster_pointsums(Points, Clusters)
  , [
      { Id, if N==0 -> Old; true -> divide_point(Sum, N) end }
    ||
      { {Id, Old},  {Id, {N, Sum}} } <- lists:zip(Clusters, ClusterPointsums)
    ]
  .

%% The k-means algorithm

kmeans_seq(0,_,Clusters) ->
  Clusters;
kmeans_seq(MaxIterations,Points,Clusters) -> do
  , NewClusters = recentre(Points,Clusters)
  , if dummy -> dummy
    ; Clusters == NewClusters -> Clusters
    ; true -> kmeans_seq(MaxIterations-1, Points, NewClusters)
    end
  .

%% Parallel version

kmeans_par(MaxIterations, Points, Clusters, NumWorkers) -> do
  , PointsPerWorker = (length(Points)+NumWorkers-1) div NumWorkers
  , Chunks = chunks(Points,PointsPerWorker)
  , Workers =
      [
        spawn_link(fun() -> worker(Chunk) end)
      ||
        Chunk <- Chunks
      ]
  , kmeans_par(MaxIterations, Workers, Clusters)
  .

kmeans_par(0, Workers, Clusters) -> do
  , [ W ! stop || W <- Workers ]
  , Clusters
  ;
kmeans_par(MaxIterations, Workers, Clusters) -> do
  , [ W ! { self(), Clusters } || W <- Workers ]
  , SubClusterPointSums =
      [
        receive  { W, ClusterPointSums } -> ClusterPointSums end
      ||
        W <- Workers
      ]
  , ClusterPointSums =
      [
        { Id, add_pointsums([proplists:get_value(Id, CPS) || CPS <- SubClusterPointSums]) }
      ||
        { Id, _ } <- Clusters
      ]
  , NewClusters =
      [
        { Id,  if N==0 -> Old; true -> divide_point(Sum, N) end }
      ||
        { {Id, Old },  {Id, {N, Sum}} } <- lists:zip(Clusters, ClusterPointSums)
      ]
  , if dummy -> dummy
    ; Clusters == NewClusters -> kmeans_par(0, Workers, Clusters)
    ; true -> kmeans_par(MaxIterations-1, Workers, NewClusters)
    end
  .

worker(Chunk) ->
  receive dummy -> dummy
  ; stop -> ok
  ; { Pid, Clusters } -> do
    , Pid ! { self(), cluster_pointsums(Chunk, Clusters) }
    , worker(Chunk)
  end
  .

chunks([], _) ->
  [];
chunks(Xs, N) -> do
  , if dummy -> dummy
    ; N > length(Xs) -> [ Xs ]
    ; true -> do
      , { Pre, Suf } = lists:split(N, Xs)
      , [ Pre | chunks(Suf, N) ]
    end
  .
