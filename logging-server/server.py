from flask import Flask
from flask import request

import csv
from datetime import datetime

app = Flask(__name__)

@app.route('/', methods=['POST', 'GET'])
def hello_world():
    if request.method == 'POST':
        recording = request.get_json(force=True, silent=True)
        
        print("received JSON {}".format(recording))
        
        with open('temperatures.csv', 'a', newline='') as csvfile:
            tempwriter = csv.writer(csvfile)
            tempwriter.writerow([datetime.now(), recording["temp"], recording["humidity"]])
    return 'Post only.'
