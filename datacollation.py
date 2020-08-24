import pandas as pd
from glob import glob

all_data = pd.DataFrame()


for file in glob("/Users/jake/Documents/R/CPD_salaries/repolicesalaryovertimeexpensesfoiarequest/*.xlsx"):
  year = file.split("FY")[1].split(".xlsx")[0]
df = pd.read_excel(file, skiprows=[0,1,2])
df['year'] = year
all_data = all_data.append(df, ignore_index=True)

all_data.to_csv("/Users/jake/Documents/R/CPD_salaries/repolicesalaryovertimeexpensesfoiarequest/policedata.csv")