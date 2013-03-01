import unittest
import requests

proxies = { "http": "http://localhost:3129",
            "https": "http://localhost:3129"}

class TestSurrogate(unittest.TestCase):

    
    def test_get_google(self):
        r = requests.get("https://www.google.com", proxies=proxies)
        
        self.assertEqual(r.status_code, requests.codes.ok)
        

if __name__ == '__main__':
    unittest.main()
