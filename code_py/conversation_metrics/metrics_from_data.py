import pandas as pd 
import ast
import pandas as pd
import glob
import os 
from tqdm import tqdm
import numpy as np
import ast
import collections
from tqdm import tqdm
import datetime
import igraph as ig 
import networkx as nx 
from collections import defaultdict
pd.options.mode.chained_assignment = None

def cumulative_time(df, minutes_to_add):
    df.sort_values(by='created_at', ascending=True, inplace=True)
    df.reset_index(drop=True, inplace=True)
    df['created_at'] = pd.to_datetime(df['created_at'])
    start_time = df.iloc[0].created_at
    end_time = start_time + datetime.timedelta(minutes=minutes_to_add)
    #print(start_time, end_time)
    return df[df['created_at'] <= end_time]['id'].count()

def create_edge_list(vertices, root, df):
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

def create_igraph(vertices, edge_list):

    vertices = list(set(df.id.tolist() + df.replied_id.tolist()))
    g = ig.Graph(directed=True)
    g.add_vertices(vertices)
    g.add_edges(edge_list)

    if g.is_tree(mode='in') == False:
        raise Exception("The input graph should be a tree, please check...") 
    return g 

def create_graph(vertices, edge_list):

    G = nx.DiGraph()
    G = nx.from_edgelist(edge_list)

    if nx.is_tree(G) == False:
        raise Exception("The input graph should be a tree, please check...") 
    return G

def tree_depth(G, root):
    return nx.eccentricity(G,v=root)

def tree_structural_viralty(G):
    return nx.average_shortest_path_length(G)  #Note: this is very time-consuming for larger cascades

def tree_size(G):
    return G.number_of_nodes()

def preprocessing_df(df): 
    df = df[df['id'].notna()]
    df = df[df['replied_id'].notna()]
    #df['id'] = pd.to_numeric(df['id'])
    #df['replied_id'] = pd.to_numeric(df['replied_id'])
    #df = df[df['replied_id'].apply(lambda x: x.isnumeric())]
    df['created_at'] = pd.to_datetime(df['created_at'])
    df['toxicity_score'] = pd.to_numeric(df['toxicity_score'])
    df.sort_values(by='created_at', inplace=True)
    df.reset_index(drop=True, inplace=True)
    return df

if __name__ == "__main__":
    """
    'ascienthusiast', 'BIZPACReview', 'FoxNews', 'drchrisnorthrup', 'healthychildren', 'NewsBecker', 'chicksonright','newsmax','GovMikeHuckabee',
        'USATODAY', 'WayneDupreeShow', 'scarymommy', 'EpochTimes', 'ebonymag', 'NYDailyNews', 'twpundit', 'thetnstar', 'houstonpress',
        'WGNRadio', 'nypost', 'tassagency_en', 'wearemitu', 'percolately', 'GeorgiaStarNews', 'mindys4Biden', 'esquire',
    """
    outlets = ['KyivIndependent',
    'digg', 'nra', 'voxdotcom', 'CNN', 'FDRLST', 'nytimes', 'BreitbartNews', 'KyivPost', 'SputnikInt', 'GayCityNews']


    ng = pd.read_csv('/Users/alessandroquattrociocchi/Documents/data/NewsGuard/Countries/USA_newsguard_handle.csv')
    f = pd.DataFrame(columns=['outlet_name','tweet_id','reliability_score','reliability_label','toxicity_score'])
    f.to_csv('/Users/alessandroquattrociocchi/Documents/data/Twitter/comments_labelled_newsguard/metrics_df.csv', index=False)
    old_df = pd.read_csv('/Users/alessandroquattrociocchi/Documents/data/Twitter/comments_labelled_newsguard/metrics_df.csv')

    
    get_results_dict = defaultdict(list)
    overall_unique_users = []
    total_comments = 0 
    toxicity_threshold = 0.25
    comments_per_outlet = collections.defaultdict(dict)
    path = '/Users/alessandroquattrociocchi/Documents/data/Twitter/comments_labelled_newsguard/perspective_eval/'

    for outlet in (outlets):
        
        all_files = glob.glob(os.path.join(path + str(outlet) , "*.csv.xz"))
        for filename in tqdm(all_files):
            s = filename.split('/')
            tweet_id = s[-1][:-7]
            df = pd.read_csv(filename, index_col=None, header=0, low_memory=False,dtype=str)


            try: 
                df = df[['id','replied_id', 'author_id','in_reply_to_user_id', 'created_at', 'lang',
                'conversation_id', 'text', 'source','toxicity_score']]
            except:
                df.rename(columns={'perspective_score': 'toxicity_score'}, inplace=True)
                df = df[['id','replied_id', 'author_id','in_reply_to_user_id', 'created_at', 'lang',
                'conversation_id', 'text', 'source','toxicity_score']]

            df = preprocessing_df(df)

            toxic_df = df[df['toxicity_score'] >= toxicity_threshold]
            
            try:
                get_results_dict['toxicity_ratio'].append(len(toxic_df)/len(df))
            except:
                get_results_dict['toxicity_ratio'].append(0)

            #retrieving the tweet identifier
            get_results_dict['tweet_id'].append(df.conversation_id[0])
            #retrieving the outlet's name
            get_results_dict['outlet_name'].append(outlet)
            #retrieving the outlet's score
            get_results_dict['outlet_score'].append(ng[ng['Twitter Handle'] == outlet].Score.values[0])
            #retrieving the outlet's flag
            get_results_dict['outlet_flag'].append(ng[ng['Twitter Handle'] == outlet].Rating.values[0])
            #retrieving the number of comments under a discussion 
            get_results_dict['n_comments'].append(df.shape[0])
            #retrieving the unique users
            get_results_dict['unique_users'].append(len(set(df.author_id.tolist() + df.in_reply_to_user_id.tolist())))
            #retrieving the numer of comments in the first 10 minutes
            get_results_dict['n_60_mins'].append(cumulative_time(df, minutes_to_add=60))
            #retrieving the numer of comments between 10 and 20 minutes
            get_results_dict['n_120_mins'].append(cumulative_time(df, minutes_to_add=80))
            #retrieving the numer of comments between 20 and 30 minutes
            get_results_dict['n_180_mins'].append(cumulative_time(df, minutes_to_add=120))
            #retrieving the numer of comments between 30 and 40 minutes
            get_results_dict['n_240_mins'].append(cumulative_time(df, minutes_to_add=240))
            #retrieving the numer of comments between 40 and 50 minutes
            get_results_dict['n_300_mins'].append(cumulative_time(df, minutes_to_add=300))

            vertices = list(set(df.id.tolist() + df.replied_id.tolist()))
            edge_list = create_edge_list(vertices = vertices, root = df.conversation_id[0], df = df)
            try:
                #creating the graph
                gtree = create_graph(vertices, edge_list)
                #compute the structure virality, i.e. the average shortest path considering of node as root
                gtree_structural_virality = tree_structural_viralty(gtree)
                #computing the size of the tree
                gtree_size = tree_size(gtree)
                #compute the depth of the tree considering the root node only
                gtree_depth = tree_depth(G = gtree, root = df.conversation_id[0])
                ##

                get_results_dict['struct_virality'].append(gtree_structural_virality)
                get_results_dict['tree_size'].append(gtree_size)
                get_results_dict['tree_depth'].append(gtree_depth)
            except Exception as e:
                print(e)
                #get_results_dict['eccentricity'].append(0)
        

        print("saving checkpoints...")        
        xdf = pd.DataFrame(get_results_dict)
        xdf.to_csv('/Users/alessandroquattrociocchi/Documents/data/Twitter/comments_labelled_newsguard/metrics/' + str(outlet) + '_conversation_metrics.csv', index=False)
