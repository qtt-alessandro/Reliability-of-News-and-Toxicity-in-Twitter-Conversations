import pandas as pd


class PrePreprocessing:

    def adjust_columns_name(df):
        try: 
            df = df[['id','replied_id', 'author_id','in_reply_to_user_id', 'created_at', 'lang',
            'conversation_id', 'text', 'source','annotations','toxicity_score']]
        except:
            df.rename(columns={'perspective_score': 'toxicity_score'}, inplace=True)
            df = df[['id','replied_id', 'author_id','in_reply_to_user_id', 'created_at', 'lang',
            'conversation_id', 'text', 'source','annotations','toxicity_score']]
        
        return df


    def preprocessing_df(df, filter_na = True): 
            if filter_na:
                df = df[df['id'].notna()]
                df = df[df['replied_id'].notna()]
            # casting string to datetime    
            df['created_at'] = pd.to_datetime(df['created_at'])
            # casting string to integer 
            df['toxicity_score'] = pd.to_numeric(df['toxicity_score'])
            #sorting by datetime
            df.sort_values(by='created_at', inplace=True)
            df.reset_index(drop=True, inplace=True)
            return df

    def filter_toxic_comments(df, tox_threshold = 0.6):
        return df[df['toxicity_score'] >= tox_threshold]

    def get_root(df):
        return df.conversation_id[0]

    def get_label(outlet_score):
        if outlet_score >= 0 and outlet_score < 20:
            return "very_low"
        elif outlet_score >= 20 and outlet_score < 40:
            return "low"
        elif outlet_score >= 40 and outlet_score < 60:
            return "mixed"
        elif outlet_score >= 60 and outlet_score < 80:
            return "high"
        elif outlet_score >= 80 and outlet_score <= 100:
            return "very_high"