import unittest
import requests
import json

proxies = { "http": "http://localhost:3129",
            "https": "http://localhost:3129"}

class TestSurrogate(unittest.TestCase):

    def test_get(self):
        rp = requests.get("http://httpbin.org/get", proxies=proxies)
        r = requests.get("http://httpbin.org/get")
        self.assertEqual(rp.status_code, requests.codes.ok)
        self.assertEqual(r.status_code, requests.codes.ok)
        jsnp = json.loads(rp.content)
        jsn = json.loads(r.content)
        self.assertEqual(jsnp['url'], jsn['url'])
        
    
    def test_get_google(self):
        r = requests.get("https://www.google.com", proxies=proxies)
        
        self.assertEqual(r.status_code, requests.codes.ok)
        

if __name__ == '__main__':
    unittest.main()
