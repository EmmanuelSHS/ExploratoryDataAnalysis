#!/usr/bin/env python
# coding=utf-8

import pandas as pd

def merge(schools, crimes, name):
    #
    pass
    

if __name__ == '__main__':
    schools = pd.read_csv("./data/postscndryunivsrvy2013dirinfo.csv")
    ocCrime = pd.read_csv("./data/Crime2013EXCEL/oncampuscrime101112.xls")
    print schools.col[1:2,1:2]
    print ocCrime.col[1:2,1:2]
