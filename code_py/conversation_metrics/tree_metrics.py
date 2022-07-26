import pandas as pd 
import numpy as np
import ast
import datetime
import igraph as ig 
from tqdm import tqdm


class Metrics:

    def get_toxicity_ratio(self, toxic_df, df):
        if len(toxic_df) > 0:
            return len(toxic_df)/len(df)
        else:
            return 0 

    def create_edge_list(self, vertices, root, df):
        edge_list = []
        orphans = []
        for vertex in set(vertices):
            if vertex != root :
                try:
                    parent = df[df.id == vertex].replied_id.values[0]
                    edge_list.append((vertex,parent))
                except:
                    orphans.append(vertex)
                    edge_list.append((vertex,root))
        return edge_list

    def create_graph(self, vertices, edge_list):
        g = ig.Graph(directed=True)
        g.add_vertices(vertices)
        g.add_edges(edge_list)

        if g.is_tree(mode='in') == False:
            raise Exception("The input graph should be a tree, please check...") 
        return g 

    def get_depth(self, gtree, root):
        return gtree.eccentricity(vertices = root, mode='all')

    def subtree_moments(self, T, root_node):
        if len(T.neighbors(root_node, mode='in')) == 0:
            # base case
            size = 1
            sum_sizes = 1
            sum_sizes_sq = 1
        else:
            # recurse over the children of the root
            size = 0
            sum_sizes = 0
            sum_sizes_sq = 0
            
            
            for child in T.neighbors(root_node, mode='in'):
                sub_tree_root = T.vs[child]["name"]
                c_size, c_sum_sizes, c_sum_sizes_sq = self.subtree_moments(T, sub_tree_root)

                size += c_size
                sum_sizes += c_sum_sizes
                sum_sizes_sq += c_sum_sizes_sq

            size += 1
            sum_sizes += size
            sum_sizes_sq += size**2

        return size, sum_sizes, sum_sizes_sq

    def get_wiener_index(self, T, root_node):
        size, sum_sizes, sum_sizes_sq = self.subtree_moments(T, root_node)
        avg_dist = (2.0 * size / (size - 1)) * (sum_sizes / size - sum_sizes_sq / size**2)
        return avg_dist

    def get_assortativity(self, graph, numeric_prop, directed_flag = False): 
        return graph.assortativity(types1 = graph.vs[numeric_prop], directed = directed_flag)

    def get_toxicity_distance(self, graph, root_node, target_node, mode = 'in'):
        """
        This function returns the distance of a toxic node from the source node
        """
        return len(graph.get_shortest_paths(v=root_node, to=target_node, weights=None, mode=mode, output='vpath')[0]) - 1

    def mean_root_distance(self, graph, df, root_node):
        avg_tox_distances = []
        if len(df) > 0:
            for node in df.id.tolist():
                avg_tox_distances.append(self.get_toxicity_distance(graph, root_node , target_node = node)/self.get_depth(graph, root_node))
        if len(avg_tox_distances)>0:
            return np.mean(avg_tox_distances)
    
    def cumulative_time(self, df, minutes_to_add):
        df.sort_values(by='created_at', ascending=True, inplace=True)
        df.reset_index(drop=True, inplace=True)
        df['created_at'] = pd.to_datetime(df['created_at'])
        start_time = df.iloc[0].created_at
        end_time = start_time + datetime.timedelta(minutes=minutes_to_add)
        #print(start_time, end_time)
        return df[df['created_at'] <= end_time]['id'].count()