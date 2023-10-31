import pandas as pd
def data_cleaning(df):
    print(f'data shape: {df.shape}')
    sum = pd.DataFrame(df.dtypes, columns=['data type'])
    sum['#missing'] = df.isnull().sum().values 
    sum['%missing'] = df.isnull().sum().values * 100 / len(df)
    sum['#unique'] = df.nunique().values

    return sum.style.background_gradient(cmap='YlOrBr')

