import itertools
import pandas as pd 



class ExtractAnnotations:

    def get_top_annotations_from_thread(df):
        cstring = df['annotations'][df.annotations.notnull()].str.replace('\d+', '', regex=True)
        cstring = cstring.str.replace('.', '', regex=True)
        cstring = cstring.str.replace('_', ' ', regex=True)
        return cstring.value_counts().index[:3].tolist()

    def divide_dataframe_by_score(xdf, score_flag):
        if score_flag == 'very_low':
            return xdf[(xdf.outlet_score >0)&(xdf.outlet_score <=20)]
        elif score_flag == 'low':
            return xdf[(xdf.outlet_score > 20)&(xdf.outlet_score <= 40)]
        elif score_flag == 'mixed':
            return xdf[(xdf.outlet_score > 40)&(xdf.outlet_score <= 60)]
        elif score_flag == 'high':
            return xdf[(xdf.outlet_score > 60)&(xdf.outlet_score <= 80)]
        elif score_flag == 'very_high':
            return xdf[(xdf.outlet_score > 80)&(xdf.outlet_score <=100)]

    def get_annotations_per_year(tmp_df, threshold, top_n_annotations):
        annotations_dict = {}
        df_20 = tmp_df[(tmp_df.created_at > "2020-01-01") & (tmp_df.created_at < "2021-01-01")]
        annotations20 = df_20[df_20.toxicity_ratio > threshold].top_3_annotations
        annotations_dict['2020'] = pd.Series(list(itertools.chain.from_iterable(annotations20))).value_counts()[:top_n_annotations].index

        df_21 = tmp_df[(tmp_df.created_at > "2021-01-01") & (tmp_df.created_at < "2022-01-01")]
        annotations21 = df_21[df_21.toxicity_ratio > threshold].top_3_annotations
        annotations_dict['2021'] = pd.Series(list(itertools.chain.from_iterable(annotations21))).value_counts()[:top_n_annotations].index

        df_22 = tmp_df[(tmp_df.created_at > "2022-01-01")]
        annotations22 = df_22[df_22.toxicity_ratio > threshold].top_3_annotations
        annotations_dict['2022'] = pd.Series(list(itertools.chain.from_iterable(annotations22))).value_counts()[:top_n_annotations].index
        return annotations_dict
