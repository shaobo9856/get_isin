import yfinance as yf
import json
import time

'''
def read_json(json_path):
    # read the json file
    f = open(json_path, 'r')
    content = f.read()
    a = json.loads(content)
    f.close()
    return a
'''

def get(code):
        # get current time
        #bisdatum = time.strftime('%Y-%m-%d',time.localtime(time.time()))
        #bisdatum2 = time.strftime('%Y%m%d', time.localtime(time.time()))
        fund = yf.Ticker("MSFT")
        # get isin
        fund.info
        fund.actions
        #print(fund.dividends)
        # show dividends
        fund.dividends
        isin =fund.get_isin(proxy="PROXY_SERVER")
        print(isin)

        # fix the filename
        #filename = filename.rstrip('.csv') + "_" + bisdatum2 + ".csv"
        #filepath = './'+ filename
        # save isin in csv

        #isin.to_csv(filepath)


if __name__ == '__main__':


    #json_path = "liste.json"
    #read file, get filename and fundname
    #data_name = read_json(json_path)

    #for row in data_name :

    #filename = "1" #row["filename"]
    code =   "ADS.DE"   #row["code"]
    #print(filename)
    print(code)
        # getinfo
    get(code)
