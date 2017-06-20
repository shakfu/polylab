import requests
from bs4 import BeautifulSoup
import goslate
from easydict import EasyDict


def translate(iterable, to_lang='en', from_lang='ar'):
    go = goslate.Goslate()
    return go.translate(iterable, to_lang, from_lang)


def check_cr(cr):
    result = EasyDict()
    COM = 'http://eservices.mci.gov.sa/Eservices/Commerce/CR_Details_Com.aspx?Loc={}&CR={}&Main=1'
    EST = 'http://eservices.mci.gov.sa/Eservices/Commerce/CR_Details_Est.aspx?Loc={}&CR={}&Main=1'
    cr = str(cr) # ensure it is a string
    location, cr_code = cr[:4], cr[4:]
    r = requests.get(COM.format(location, cr_code))
    soup = BeautifulSoup(r.text)
    tag = soup.find(id="ContentPlaceHolder1_lblCRName")
    if tag:
        result['cr_name'] = tag.text
        result['cr_address'] = soup.find(id='ContentPlaceHolder1_lblAderss').text
        result['cr_phone'] = soup.find(id='ContentPlaceHolder1_lblPhone1').text
        result['cr_fax'] = soup.find(id='ContentPlaceHolder1_lblFax1').text
        result['cr_pobox'] = soup.find(id='ContentPlaceHolder1_lblPOBOX').text
        result['cr_postcode'] = soup.find(id='ContentPlaceHolder1_lblPostCode').text
        result['cr_activities'] = soup.find(id='ContentPlaceHolder1_lblAct').text
        result['cr_type'] = 'com'
    else:
        r = requests.get(EST.format(location, cr_code))
        soup = BeautifulSoup(r.text)
        tag = soup.find(id="ContentPlaceHolder1_lblCRName")
        if tag:
            result['cr_name'] = tag.text
            result['cr_address'] = soup.find(id='ContentPlaceHolder1_lblAderss').text
            result['cr_phone'] = soup.find(id='ContentPlaceHolder1_lblPhone1').text
            result['cr_fax'] = soup.find(id='ContentPlaceHolder1_lblFax1').text
            result['cr_pobox'] = soup.find(id='ContentPlaceHolder1_lblPOBOX').text
            result['cr_postcode'] = soup.find(id='ContentPlaceHolder1_lblPostCode').text
            result['cr_activities'] = soup.find(id='ContentPlaceHolder1_lblAct').text
            result['cr_type'] = 'est'
    return result

if __name__ == '__main__':
    eg = '2000000000'
    er = '2050000000'  # error
    r = check_cr(eg)
    print r['cr_name']
