#!/usr/bin/env python
# coding=utf-8

import sys
import pandas as pd

def news(df,name):
        news = df[[u'Country',u'Centroid X', u'Centroid Y', u'Notes and Comments (may include quoted headlines from copyrighted news stories; for internal research purposes only)']]
        news.to_csv(name, encoding = 'utf-8', header=['country','X','Y',  'content'])

if __name__ == '__main__':
        d = pd.read_excel('GlobalFloodsRecord.xls')
        #print d.columns.values
        news(d, 'news.csv')
